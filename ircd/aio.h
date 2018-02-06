// Matrix Construct
//
// Copyright (C) Matrix Construct Developers, Authors & Contributors
// Copyright (C) 2016-2018 Jason Volk <jason@zemos.net>
//
// Permission to use, copy, modify, and/or distribute this software for any
// purpose with or without fee is hereby granted, provided that the above
// copyright notice and this permission notice is present in all copies. The
// full license for this software is available in the LICENSE file.

#pragma once
#define HAVE_AIO_H

#include <RB_INC_SYS_SYSCALL_H
#include <RB_INC_SYS_EVENTFD_H
#include <linux/aio_abi.h>

/// AIO context instance. Right now this is a singleton with an extern
/// instance at fs::aioctx.
struct ircd::fs::aio
{
	struct request;

	/// Maximum number of events we can submit to kernel
	static constexpr const size_t &MAX_EVENTS
	{
		64
	};

	/// The semaphore value for the eventfd which we keep here.
	uint64_t semval{0};

	/// An eventfd which will be notified by the kernel; we integrate this with
	/// the ircd io_service core epoll() event loop. The EFD_SEMAPHORE flag is
	/// not used to reduce the number of triggers. We can collect multiple AIO
	/// completions after a single trigger to this fd. With EFD_SEMAPHORE, we
	/// would collect all the completions on the first trigger and then
	/// continue to get polled. Because EFD_SEMAPHORE is not set, the semval
	/// which is kept above will reflect a hint for how many AIO's are done.
	asio::posix::stream_descriptor resfd
	{
		*ircd::ios, int(syscall(::eventfd, semval, EFD_NONBLOCK))
	};

	/// Handler to the io context we submit requests to the kernel with
	aio_context_t idp{0};

	// Callback stack invoked when the sigfd is notified of completed events.
	void handle_event(const io_event &) noexcept;
	void handle_events() noexcept;
	void handle(const error_code &, const size_t) noexcept;
	void set_handle();

	aio()
	{
		syscall<SYS_io_setup>(MAX_EVENTS, &idp);
		set_handle();
	}

	~aio() noexcept
	{
		resfd.cancel();
		syscall<SYS_io_destroy>(idp);
	}
};

/// Generic request control block.
struct ircd::fs::aio::request
:iocb
{
	struct read;
	struct write;

	ssize_t retval {0};
	ssize_t errcode {0};
	ctx::ctx *waiter {ctx::current};

	void handle();

  public:
	size_t operator()();
	void cancel();

	request(const int &fd);
	~request() noexcept;
};

ircd::fs::aio::request::request(const int &fd)
:iocb{0}
{
	assert(aioctx);
	assert(ctx::current);

	aio_flags = IOCB_FLAG_RESFD;
	aio_resfd = aioctx->resfd.native_handle();
	aio_fildes = fd;
	aio_data = uintptr_t(this);
}

/// vtable base
ircd::fs::aio::request::~request()
noexcept
{
}

/// Cancel a request. The handler callstack is invoked directly from here
/// which means any callback will be invoked or ctx will be notified if
/// appropriate.
void
ircd::fs::aio::request::cancel()
{
	io_event result {0};
	const auto &cb{static_cast<iocb *>(this)};

	assert(aioctx);
	syscall_nointr<SYS_io_cancel>(aioctx->idp, cb, &result);
	aioctx->handle_event(result);
}

/// Submit a request and properly yield the ircd::ctx. When this returns the
/// result will be available or an exception will be thrown.
size_t
ircd::fs::aio::request::operator()()
try
{
	assert(aioctx);
	assert(ctx::current);
	assert(waiter == ctx::current);

	struct iocb *const cbs[]
	{
		static_cast<iocb *>(this)
	};

	syscall<SYS_io_submit>(aioctx->idp, 1, &cbs); do
	{
		ctx::wait();
	}
	while(!retval && !errcode);

	if(retval == -1)
		throw_system_error(errcode);

	return size_t(retval);
}
catch(const ctx::interrupted &e)
{
	// When the ctx is interrupted we're obligated to cancel the request.
	// The handler callstack is invoked directly from here by cancel() for
	// what it's worth but we rethrow the interrupt anyway.
	cancel();
	throw;
}

void
ircd::fs::aio::request::handle()
{
	assert(this->retval || errcode);
	if(likely(waiter && waiter != ctx::current))
		ircd::ctx::notify(*waiter);
}

//
// aio
//

void
ircd::fs::aio::set_handle()
{
	semval = 0;
	const asio::mutable_buffers_1 bufs(&semval, sizeof(semval));
	auto handler{std::bind(&aio::handle, this, ph::_1, ph::_2)};
	asio::async_read(resfd, bufs, std::move(handler));
}

/// Handle notifications that requests are complete.
void
ircd::fs::aio::handle(const error_code &ec,
                      const size_t bytes)
noexcept
{
	assert((bytes == 8 && !ec && semval >= 1) || (bytes == 0 && ec));
	assert(!ec || ec.category() == asio::error::get_system_category());

	switch(ec.value())
	{
		case boost::system::errc::success:
			handle_events();
			break;

		case boost::system::errc::operation_canceled:
			return;

		default:
			throw boost::system::system_error(ec);
	}

	set_handle();
}

void
ircd::fs::aio::handle_events()
noexcept try
{
	std::array<io_event, MAX_EVENTS> event;

	// The number of completed requests available in events[]. This syscall
	// is restarted on EINTR. After restart, it may or may not find any ready
	// events but it never blocks to do so.
	const auto count
	{
		syscall_nointr<SYS_io_getevents>(idp, 0, event.size(), event.data(), nullptr)
	};

	// The count should be at least 1 event. The only reason to return 0 might
	// be related to an INTR; this assert will find out and may be commented.
	assert(count > 0);

	for(ssize_t i(0); i < count; ++i)
		handle_event(event[i]);
}
catch(const std::exception &e)
{
	log::error("AIO(%p) handle_events: %s",
	           this,
	           e.what());
}

void
ircd::fs::aio::handle_event(const io_event &event)
noexcept try
{
	// Our extended control block is passed in event.data
	auto *const &request
	{
		reinterpret_cast<aio::request *>(event.data)
	};

	// The relevant iocb is repeated back to us in the result; we assert
	// some basic sanity here about the layout of the request conglomerate.
	assert(reinterpret_cast<iocb *>(event.obj) == static_cast<iocb *>(request));

	// error conventions are like so
	assert(event.res >= -1);  // unix syscall return value semantic
	assert(event.res2 >= 0);  // errno code semantic
	assert(event.res == -1 || event.res2 == 0);

	// Set result indicators
	request->retval = event.res;
	request->errcode = event.res2;
/*
	log::debug("AIO request(%p) fd:%d op:%d bytes:%lu off:%ld prio:%d ctx:%p result: bytes:%ld errno:%ld",
	           request,
	           request->aio_fildes,
	           request->aio_lio_opcode,
	           request->aio_nbytes,
	           request->aio_offset,
	           request->aio_reqprio,
	           request->waiter,
	           request->retval,
	           request->errcode);
*/
	request->handle();
}
catch(const std::exception &e)
{
	log::critical("Unhandled request(%lu) event(%p) error: %s",
	              event.data,
	              &event,
	              e.what());
}

///////////////////////////////////////////////////////////////////////////////
//
// fs/read.h
//

namespace ircd::fs
{
	string_view read__aio(const string_view &path, const mutable_buffer &, const read_opts &);
	std::string read__aio(const string_view &path, const read_opts &);
}

/// Read request control block
struct ircd::fs::aio::request::read
:request
{
	read(const int &fd, const mutable_buffer &buf, const read_opts &opts);
};

ircd::fs::aio::request::read::read(const int &fd,
                                   const mutable_buffer &buf,
                                   const read_opts &opts)
:request{fd}
{
	aio_reqprio = opts.priority;
	aio_lio_opcode = IOCB_CMD_PREAD;

	aio_buf = uintptr_t(buffer::data(buf));
	aio_nbytes = buffer::size(buf);
	aio_offset = opts.offset;
}

//
// Interface
//

std::string
ircd::fs::read__aio(const string_view &path,
                    const read_opts &opts)
{
	// Path to open(2) must be null terminated;
	static thread_local char pathstr[2048];
	strlcpy(pathstr, path, sizeof(pathstr));

	const auto fd
	{
		syscall(::open, pathstr, O_CLOEXEC, O_RDONLY)
	};

	const unwind cfd{[&fd]
	{
		syscall(::close, fd);
	}};

	struct stat stat;
	syscall(::fstat, fd, &stat);
	return string(stat.st_size, [&fd, &opts]
	(const mutable_buffer &buf)
	{
		aio::request::read request
		{
			int(fd), buf, opts
		};

		return request();
	});
}

ircd::string_view
ircd::fs::read__aio(const string_view &path,
                    const mutable_buffer &buf,
                    const read_opts &opts)
{
	// Path to open(2) must be null terminated;
	static thread_local char pathstr[2048];
	strlcpy(pathstr, path, sizeof(pathstr));

	const auto fd
	{
		syscall(::open, pathstr, O_CLOEXEC, O_RDONLY)
	};

	const unwind cfd{[&fd]
	{
		syscall(::close, fd);
	}};

	aio::request::read request
	{
		int(fd), buf, opts
	};

	const size_t bytes
	{
		request()
	};

	const string_view view
	{
		reinterpret_cast<const char *>(data(buf)), bytes
	};

	return view;
}
