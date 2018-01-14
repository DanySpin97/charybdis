// Copyright (C) Matrix Construct Developers, Authors & Contributors
// Copyright (C) 2016-2018 Jason Volk
//
// Permission to use, copy, modify, and/or distribute this software for any
// purpose with or without fee is hereby granted, provided that the above
// copyright notice and this permission notice is present in all copies.
//
// THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT,
// INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
// (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
// HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
// STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
// IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.

#include <ircd/asio.h>

namespace ircd
{
	template<class... args> std::shared_ptr<client> make_client(args&&...);
}

/// Linkage for the default settings
decltype(ircd::client::settings)
ircd::client::settings
{};

/// Linkage for the default conf
decltype(ircd::client::default_conf)
ircd::client::default_conf
{};

/// The pool of request contexts. When a client makes a request it does so by acquiring
/// a stack from this pool. The request handling and response logic can then be written
/// in a synchronous manner as if each connection had its own thread.
ircd::ctx::pool
ircd::client::context
{
	"client", settings.stack_size
};

// Linkage for the container of all active clients for iteration purposes.
template<>
decltype(ircd::util::instance_list<ircd::client>::list)
ircd::util::instance_list<ircd::client>::list
{};

//
// init
//

ircd::client::init::init()
{
	context.add(settings.pool_size);
}

void
ircd::client::init::interrupt()
{
	if(context.active() || !client::list.empty())
		log::warning("Interrupting %zu requests; dropping %zu requests; closing %zu clients...",
		             context.active(),
		             context.pending(),
		             client::list.size());

	context.interrupt();
	close_all_clients();
}

ircd::client::init::~init()
noexcept
{
	interrupt();

	if(context.active())
		log::warning("Joining %zu active of %zu remaining request contexts...",
		             context.active(),
		             context.size());
	else
		log::debug("Waiting for %zu request contexts to join...",
		           context.size());

	context.join();

	if(unlikely(!client::list.empty()))
	{
		log::error("%zu clients are unterminated...", client::list.size());
		assert(client::list.empty());
	}
}

//
// util
//

ircd::http::response::write_closure
ircd::write_closure(client &client)
{
	// returns a function that can be called to send an iovector of data to a client
	return [&client](const ilist<const const_buffer> &iov)
	{
		//std::cout << "<<<< " << size(iov) << std::endl;
		//std::cout << iov << std::endl;
		//std::cout << "---- " << std::endl;
		const auto written
		{
			write(*client.sock, iov)
		};
	};
}

ircd::parse::read_closure
ircd::read_closure(client &client)
{
	// Returns a function the parser can call when it wants more data
	return [&client](char *&start, char *const &stop)
	{
		try
		{
			char *const got(start);
			read(client, start, stop);
			//std::cout << ">>>> " << std::distance(got, start) << std::endl;
			//std::cout << string_view{got, start} << std::endl;
			//std::cout << "----" << std::endl;
		}
		catch(const boost::system::system_error &e)
		{
			using namespace boost::system::errc;

			switch(e.code().value())
			{
				case operation_canceled:
					throw http::error(http::REQUEST_TIMEOUT);

				default:
					throw;
			}
		}
    };
}

char *
ircd::read(client &client,
           char *&start,
           char *const &stop)
{
	assert(client.sock);
	auto &sock(*client.sock);
	const mutable_buffer buf
	{
		start, stop
	};

	char *const base(start);
	start += net::read(sock, buf);
	return base;
}

const char *
ircd::write(client &client,
            const char *&start,
            const char *const &stop)
{
	assert(client.sock);
	auto &sock(*client.sock);
	const const_buffer buf
	{
		start, stop
	};

	const char *const base(start);
	start += net::write(sock, buf);
	return base;
}

std::shared_ptr<ircd::client>
ircd::add_client(std::shared_ptr<socket> s)
{
	const auto client
	{
		make_client(std::move(s))
	};

	client->async();
	return client;
}

template<class... args>
std::shared_ptr<ircd::client>
ircd::make_client(args&&... a)
{
	return std::make_shared<client>(std::forward<args>(a)...);
}

void
ircd::close_all_clients()
{
	auto it(begin(client::list));
	while(it != end(client::list))
	{
		auto *const client(*it);
		++it; try
		{
			client->close(net::dc::RST, net::close_ignore);
		}
		catch(const std::exception &e)
		{
			log::warning("Error disconnecting client @%p: %s", client, e.what());
		}
	}
}

ircd::ipport
ircd::local(const client &client)
{
	if(!client.sock)
		return {};

	return net::local_ipport(*client.sock);
}

ircd::ipport
ircd::remote(const client &client)
{
	if(!client.sock)
		return {};

	return net::remote_ipport(*client.sock);
}

//
// async loop
//

namespace ircd
{
	static bool handle_ec_default(client &, const error_code &);
	static bool handle_ec_timeout(client &);
	static bool handle_ec_short_read(client &);
	static bool handle_ec_eof(client &);
	static bool handle_ec(client &, const error_code &);

	static void handle_client_request(std::shared_ptr<client>);
	static void handle_client_ready(std::shared_ptr<client>, const error_code &ec);
}

/// This function is the basis for the client's request loop. We still use
/// an asynchronous pattern until there is activity on the socket (a request)
/// in which case the switch to synchronous mode is made by jumping into an
/// ircd::context drawn from the request pool. When the request is finished,
/// the client exits back into asynchronous mode until the next request is
/// received and rinse and repeat.
//
/// This sequence exists to avoid any possible c10k-style limitation imposed by
/// dedicating a context and its stack space to the lifetime of a connection.
/// This is similar to the thread-per-request pattern before async was in vogue.
///
/// This call returns immediately so we no longer block the current context and
/// its stack while waiting for activity on idle connections between requests.
void
ircd::client::async()
{
	assert(bool(this->sock));
	assert(bool(this->conf));
	auto &sock(*this->sock);
	const net::wait_opts opts
	{
		net::ready::READ, conf->async_timeout
	};

	auto handler
	{
		std::bind(ircd::handle_client_ready, shared_from(*this), ph::_1)
	};

	sock(opts, std::move(handler));
}

/// The client's socket is ready for reading. This intermediate handler
/// intercepts any errors otherwise dispatches the client to the request
/// pool to be married with a stack. Right here this handler is executing on
/// the main stack (not in any ircd::context).
///
/// The context the closure ends up getting is the next available from the
/// request pool, which may not be available immediately so this handler might
/// be queued for some time after this call returns.
void
ircd::handle_client_ready(std::shared_ptr<client> client,
                          const error_code &ec)
{
	if(!handle_ec(*client, ec))
		return;

	auto handler
	{
		std::bind(ircd::handle_client_request, std::move(client))
	};

	client::context(std::move(handler));
}

/// A request context has been dispatched and is now handling this client.
/// This function is executing on that ircd::ctx stack. client::main() will
/// now be called and synchronous programming is possible. Afterward, the
/// client will release this ctx and its stack and fall back to async mode
/// or die.
void
ircd::handle_client_request(std::shared_ptr<client> client)
{
	if(!client->main())
	{
		client->close(net::dc::SSL_NOTIFY).wait();
		return;
	}

	client->async();
}

/// This error handling switch is one of two places client errors
/// are handled. This handles the errors when the client is in async
/// mode rather than during a request. This executes on the main/callback
/// stack, not in any ircd::ctx, and must be asynchronous.
///
bool
ircd::handle_ec(client &client,
                const error_code &ec)
{
	using namespace boost::system::errc;
	using boost::system::system_category;
	using boost::asio::error::get_ssl_category;
	using boost::asio::error::get_misc_category;

	if(ec.category() == system_category()) switch(ec.value())
	{
		case success:                return true;
		case operation_canceled:     return handle_ec_timeout(client);
		default:                     return handle_ec_default(client, ec);
	}
	else if(ec.category() == get_misc_category()) switch(ec.value())
	{
		case asio::error::eof:       return handle_ec_eof(client);
		default:                     return handle_ec_default(client, ec);
	}
	else if(ec.category() == get_ssl_category()) switch(uint8_t(ec.value()))
	{
		case SSL_R_SHORT_READ:       return handle_ec_short_read(client);
		default:                     return handle_ec_default(client, ec);
	}
	else return handle_ec_default(client, ec);
}

/// The client indicated they will not be sending the data we have been
/// waiting for. The proper behavior now is to initiate a clean shutdown.
bool
ircd::handle_ec_eof(client &client)
try
{
	log::debug("socket(%p) local[%s] remote[%s] end of file",
	           client.sock.get(),
	           string(local(client)),
	           string(remote(client)));

	client.close(net::dc::SSL_NOTIFY, net::close_ignore);
	return false;
}
catch(const std::exception &e)
{
	log::error("socket(%p) EOF: %s",
	           client.sock.get(),
	           e.what());

	return false;
}

/// The client terminated the connection, likely improperly, and SSL
/// is informing us with an opportunity to prevent truncation attacks.
/// Best behavior here is to just close the sd.
bool
ircd::handle_ec_short_read(client &client)
try
{
	log::warning("socket(%p) local[%s] remote[%s] short_read",
	             client.sock.get(),
	             string(local(client)),
	             string(remote(client)));

	client.close(net::dc::RST, net::close_ignore);
	return false;
}
catch(const std::exception &e)
{
	log::error("socket(%p) short_read: %s",
	           client.sock.get(),
	           e.what());

	return false;
}

/// The net:: system determined the client timed out because we set a timer
/// on the socket waiting for data which never arrived. The client may very
/// well still be there, so the best thing to do is to attempt a clean
/// disconnect.
bool
ircd::handle_ec_timeout(client &client)
try
{
	assert(bool(client.sock));
	log::warning("socket(%p) local[%s] remote[%s] disconnecting after inactivity timeout",
	             client.sock.get(),
	             string(local(client)),
	             string(remote(client)));

	client.close(net::dc::SSL_NOTIFY, net::close_ignore);
	return false;
}
catch(const std::exception &e)
{
	log::error("socket(%p) timeout: %s",
	           client.sock.get(),
	           e.what());

	return false;
}

/// Unknown/untreated error. Probably not worth attempting a clean shutdown
/// so a hard / immediate disconnect given instead.
bool
ircd::handle_ec_default(client &client,
                        const error_code &ec)
{
	log::warning("socket(%p) local[%s] remote[%s] %s",
	             client.sock.get(),
	             string(local(client)),
	             string(remote(client)),
	             string(ec));

	client.close(net::dc::RST, net::close_ignore);
	return false;
}

//
// client
//

ircd::client::client()
:client{std::shared_ptr<socket>{}}
{
}

ircd::client::client(std::shared_ptr<socket> sock)
:sock{std::move(sock)}
{
}

ircd::client::~client()
noexcept try
{
	//assert(!sock || !connected(*sock));
}
catch(const std::exception &e)
{
	log::critical("socket(%p) ~client(%p): %s",
	              sock.get(),
	              this,
	              e.what());
	return;
}

/// Client main loop.
///
/// Before main(), the client had been sitting in async mode waiting for
/// socket activity. Once activity with data was detected indicating a request,
/// the client was dispatched to the request pool where it is paired to an
/// ircd::ctx with a stack. main() is then invoked on that ircd::ctx stack.
/// Nothing from the socket has been read into userspace before main().
///
/// This function parses requests off the socket in a loop until there are no
/// more requests or there is a fatal error. The ctx will "block" to wait for
/// more data off the socket during the middle of a request until the request
/// timeout is reached. main() will not "block" to wait for more data after a
/// request; it will simply `return true` which puts this client back into
/// async mode and relinquishes this stack. returning false will disconnect
/// the client rather than putting it back into async mode.
///
/// Exceptions do not pass below main() therefor anything unhandled is an
/// internal server error and the client is disconnected. The exception handler
/// here though is executing on a request ctx stack, and we can choose to take
/// advantage of that; in contrast to the handle_ec() switch which handles
/// errors on the main/callback stack and must be asynchronous.
///
bool
ircd::client::main()
noexcept try
{
	char buffer[client::request::HEAD_MAX];
	parse::buffer pb{mutable_buffer{buffer}};
	parse::capstan pc{pb, read_closure(*this)}; do
	{
		if(!handle_request(pc))
			return false;

		// After the request, the head and content has been read off the socket
		// and the capstan has advanced to the end of the content. The catch is
		// that reading off the socket could have read too much, bleeding into
		// the next request. This is rare, but pb.remove() will memmove() the
		// bleed back to the beginning of the head buffer for the next loop.
		pb.remove();
	}
	while(pc.unparsed());

	return true;
}
catch(const boost::system::system_error &e)
{
	using namespace boost::system::errc;
	using boost::system::system_category;
	using boost::asio::error::get_ssl_category;
	using boost::asio::error::get_misc_category;

	log::debug("socket(%p) local[%s] remote[%s] error during request: %s",
	           sock.get(),
	           string(local(*this)),
	           string(remote(*this)),
	           string(e.code()));

	const error_code &ec{e.code()};
	const int &value{ec.value()};
	if(ec.category() == system_category()) switch(value)
	{
		case success:
			assert(0);
			return true;

		case broken_pipe:
		case connection_reset:
		case not_connected:
			close(net::dc::RST, net::close_ignore);
			return false;

		case operation_canceled:
			return false;

		case bad_file_descriptor:
			return false;

		default:
			break;
	}
	else if(ec.category() == get_ssl_category()) switch(uint8_t(value))
	{
		case SSL_R_SHORT_READ:
			close(net::dc::RST, net::close_ignore);
			return false;

		case SSL_R_PROTOCOL_IS_SHUTDOWN:
			close(net::dc::RST, net::close_ignore);
			return false;

		default:
			break;
	}
	else if(ec.category() == get_misc_category()) switch(value)
	{
		case boost::asio::error::eof:
			return false;

		default:
			break;
	}

	log::error("socket(%p) (unexpected) %s: (%d) %s",
	           sock.get(),
	           ec.category().name(),
	           value,
	           ec.message());

	close(net::dc::RST, net::close_ignore);
	return false;
}
catch(const std::exception &e)
{
	log::error("client[%s] [500 Internal Error]: %s",
	           string(remote(*this)),
	           e.what());

	#ifdef RB_DEBUG
		throw;
	#else
		return false;
	#endif
}

/// The constructor for request state is only made in
/// client::handle_request(). It is defined here to be adjacent to that
/// callsite
///
ircd::client::request::request(parse::capstan &pc)
:head
{
	// This is the first read off the wire. The headers are entirely read and
	// the tape is advanced.
	pc
}
,content_consumed
{
	// The size of HTTP headers are never initially known, which means
	// the above head parse could have read too much off the socket bleeding
	// into the content or even the next request entirely. That's ok because
	// the state of `pc` will reflect that back to the main() loop for the
	// next request, but for this request we have to figure out how much of
	// the content was accidentally read so far.
	std::min(pc.unparsed(), head.content_length)
}
,content_partial
{
	pc.parsed, content_consumed
}
{
	pc.parsed += content_consumed;
}

/// Handle a single request within the client main() loop.
///
/// This function returns false if the main() loop should exit
/// and thus disconnect the client. It should return true in most
/// cases even for lightly erroneous requests that won't affect
/// the next requests on the tape.
///
/// This function is timed. The timeout will prevent a client from
/// sending a partial request and leave us waiting for the rest.
/// As of right now this timeout extends to our handling of the
/// request too.
bool
ircd::client::handle_request(parse::capstan &pc)
try
{
	const socket::scope_timeout timeout
	{
		*sock, conf->request_timeout
	};

	struct request request{pc};
	assert(pc.parsed <= pc.read);
	this->request = &request;
	log::debug("socket(%p) local[%s] remote[%s] HTTP %s `%s' content-length:%zu part:%zu",
	           sock.get(),
	           string(local(*this)),
	           string(remote(*this)),
	           request.head.method,
	           request.head.path,
	           request.head.content_length,
	           request.content_consumed);

	bool ret
	{
		resource_request(request)
	};

	if(ret && iequals(request.head.connection, "close"_sv))
		ret = false;

	return ret;
}
catch(const ircd::error &e)
{
	log::error("socket(%p) local[%s] remote[%s]: %s",
	           sock.get(),
	           string(local(*this)),
	           string(remote(*this)),
	           e.what());

	resource::response
	{
		*this, e.what(), {}, http::INTERNAL_SERVER_ERROR
	};

	throw;
}

bool
ircd::client::resource_request(struct request &request)
try
{
	auto &resource
	{
		ircd::resource::find(request.head.path)
	};

	resource(*this, request, request.head);
	return true;
}
catch(const http::error &e)
{
	resource::response
	{
		*this, e.content, "text/html; charset=utf8", e.code, e.headers
	};

	switch(e.code)
	{
		// These codes are "recoverable" and allow the next HTTP request in
		// a pipeline to take place. In order for that to happen, any content
		// which wasn't read because of the exception has to be read now.
		default:
		{
			discard_unconsumed(request);
			return true;
		}

		// These codes are "unrecoverable" errors and no more HTTP can be
		// conducted with this tape. The client must be disconnected.
		case http::BAD_REQUEST:
		case http::PAYLOAD_TOO_LARGE:
		case http::REQUEST_TIMEOUT:
			close().wait();    // close wait because we're on a stack
			return false;

		// The client must also be disconnected at some point down the stack.
		case http::INTERNAL_SERVER_ERROR:
			throw;
	}
}

void
ircd::client::discard_unconsumed(struct request &request)
{
	if(unlikely(!sock))
		return;

	const size_t unconsumed
	{
		request.head.content_length - request.content_consumed
	};

	if(!unconsumed)
		return;

	log::debug("socket(%p) local[%s] remote[%s] discarding %zu of %zu unconsumed content...",
	           sock.get(),
	           string(local(*this)),
	           string(remote(*this)),
	           unconsumed,
	           request.head.content_length);

	request.content_consumed += net::discard_all(*sock, unconsumed);
	assert(request.content_consumed == request.head.content_length);
}

ircd::ctx::future<void>
ircd::client::close(const net::close_opts &opts)
{
	if(likely(sock))
		return net::close(*sock, opts);
	else
		return {};
}

void
ircd::client::close(const net::close_opts &opts,
                    net::close_callback callback)
{
	net::close(*sock, opts, std::move(callback));
}
