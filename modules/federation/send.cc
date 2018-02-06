// Matrix Construct
//
// Copyright (C) Matrix Construct Developers, Authors & Contributors
// Copyright (C) 2016-2018 Jason Volk <jason@zemos.net>
//
// Permission to use, copy, modify, and/or distribute this software for any
// purpose with or without fee is hereby granted, provided that the above
// copyright notice and this permission notice is present in all copies. The
// full license for this software is available in the LICENSE file.

using namespace ircd;

void sender_worker();
ircd::context sender_context
{
	"sender",
	256_KiB,
	&sender_worker,
	ircd::context::POST,
};

const auto on_unload{[]
{
	sender_context.interrupt();
	sender_context.join();
}};

mapi::header IRCD_MODULE
{
	"federation send",
	nullptr,
	on_unload
};

struct send
:resource
{
	using resource::resource;
}
send_resource
{
	"/_matrix/federation/v1/send/",
	{
		"federation send",
		resource::DIRECTORY,
	}
};

void
handle_edu(client &client,
           const resource::request::object<m::txn> &request,
           const string_view &txn_id,
           const json::object &edu)
{
	//std::cout << edu << std::endl;
	log::debug("%s :%s | %s | %s",
	           txn_id,
	           at<"origin"_>(request),
	           edu.at("edu_type"),
	           edu.get("sender", string_view{"*"}));
}

void
handle_pdu(client &client,
           const resource::request::object<m::txn> &request,
           const string_view &txn_id,
           const m::event &event)
try
{
	//std::cout << event << std::endl;
	log::debug("%s %s",
	           txn_id,
	           pretty_oneline(event));

	m::vm::eval{event};
}
catch(const ed25519::bad_sig &e)
{
	throw m::BAD_SIGNATURE
	{
		":%s %s %s %s",
		at<"origin"_>(request),
		at<"room_id"_>(event),
		at<"event_id"_>(event),
		e.what()
	};
}

void
handle_pdu_failure(client &client,
                   const resource::request::object<m::txn> &request,
                   const string_view &txn_id,
                   const json::object &pdu_failure)
{
	log::debug("%s :%s | (pdu_failure) %s",
	           txn_id,
	           at<"origin"_>(request),
	           pdu_failure.get("sender", string_view{"*"}),
	           string_view{pdu_failure});
}

resource::response
handle_put(client &client,
           const resource::request::object<m::txn> &request)
{
	const auto txn_id
	{
		request.parv[0]
	};

	const string_view &origin
	{
		json::at<"origin"_>(request)
	};

	const json::array &edus
	{
		json::get<"edus"_>(request)
	};

	const json::array &pdus
	{
		json::get<"pdus"_>(request)
	};

	const json::array &pdu_failures
	{
		json::get<"pdu_failures"_>(request)
	};

	for(const auto &pdu_failure : pdu_failures)
		handle_pdu_failure(client, request, txn_id, pdu_failure);

	for(const auto &edu : edus)
		handle_edu(client, request, txn_id, edu);

	for(const auto &pdu : pdus)
		handle_pdu(client, request, txn_id, m::event{pdu});

	log::debug("%s :%s | %s --> edus:%zu pdus:%zu errors:%zu",
	           txn_id,
	           origin,
	           string(remote(client)),
	           edus.count(),
	           pdus.count(),
	           pdu_failures.count());

	return resource::response
	{
		client, http::OK
	};
}

resource::method method_put
{
	send_resource, "PUT", handle_put,
	{
		method_put.VERIFY_ORIGIN
	}
};

//
// Main worker stack
//

void sender_handle(const m::event &, const m::room::id &room_id);
void sender_handle(const m::event &);

void
sender_worker()
{
	while(1) try
	{
		std::unique_lock<decltype(m::vm::inserted)> lock
		{
			m::vm::inserted
		};

		// reference to the event on the inserter's stack
		const auto &event
		{
			m::vm::inserted.wait(lock)
		};

		sender_handle(event);
	}
	catch(const ircd::ctx::interrupted &e)
	{
		ircd::log::debug("sender worker interrupted");
		return;
	}
	catch(const timeout &e)
	{
		ircd::log::debug("sender worker: %s", e.what());
	}
	catch(const std::exception &e)
	{
		ircd::log::error("sender worker: %s", e.what());
	}
}

void
sender_handle(const m::event &event)
{
	const auto &room_id
	{
		json::get<"room_id"_>(event)
	};

	if(room_id)
	{
		sender_handle(event, room_id);
		return;
	}

	assert(0);
}

ssize_t txn_ctr
{
	8
};

void
sender_handle(const m::event &event,
              const m::room::id &room_id)
try
{
	const m::room room
	{
		room_id
	};

	const m::event::id &event_id
	{
		json::get<"event_id"_>(event)
	};
}
catch(const http::error &e)
{
	throw;
}
catch(const std::exception &e)
{
	throw;
}
