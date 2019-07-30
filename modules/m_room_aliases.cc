// Matrix Construct
//
// Copyright (C) Matrix Construct Developers, Authors & Contributors
// Copyright (C) 2016-2018 Jason Volk <jason@zemos.net>
//
// Permission to use, copy, modify, and/or distribute this software for any
// purpose with or without fee is hereby granted, provided that the above
// copyright notice and this permission notice is present in all copies. The
// full license for this software is available in the LICENSE file.

namespace ircd::m
{
	extern conf::item<seconds> alias_fetch_timeout;
	extern conf::item<seconds> alias_cache_ttl;
	extern const room::id::buf alias_room_id;
	extern const room alias_room;

	static void auth_room_aliases(const event &, event::auth::hookdata &);
	extern hookfn<event::auth::hookdata &> auth_room_aliases_hookfn;

	static void changed_room_aliases(const event &, vm::eval &);
	extern hookfn<vm::eval &> changed_room_aliases_hookfn;

	extern hookfn<m::vm::eval &> create_alias_room_hookfn;
}

ircd::mapi::header
IRCD_MODULE
{
	"Matrix m.room.aliases"
};

decltype(ircd::m::alias_room_id)
ircd::m::alias_room_id
{
	"alias", ircd::my_host()
};

decltype(ircd::m::alias_room)
ircd::m::alias_room
{
	alias_room_id
};

decltype(ircd::m::alias_cache_ttl)
ircd::m::alias_cache_ttl
{
	{ "name",    "ircd.m.room.aliases.cache.ttl" },
	{ "default", 604800L                         },
};

decltype(ircd::m::alias_fetch_timeout)
ircd::m::alias_fetch_timeout
{
	{ "name",    "ircd.m.room.aliases.fetch.timeout" },
	{ "default", 10L                                 },
};

//
// create the alias room as an effect of !ircd created on bootstrap
//

decltype(ircd::m::create_alias_room_hookfn)
ircd::m::create_alias_room_hookfn
{
	{
		{ "_site",       "vm.effect"      },
		{ "room_id",     "!ircd"          },
		{ "type",        "m.room.create"  },
	},

	[](const m::event &, m::vm::eval &)
	{
		create(alias_room_id, m::me.user_id);
	}
};

//
// an effect of room aliases changed
//

decltype(ircd::m::changed_room_aliases_hookfn)
ircd::m::changed_room_aliases_hookfn
{
	changed_room_aliases,
	{
		{ "_site",    "vm.effect"       },
		{ "type",     "m.room.aliases"  },
	}
};

void
ircd::m::changed_room_aliases(const m::event &event,
                              m::vm::eval &)
{
	const m::room::id &room_id
	{
		at<"room_id"_>(event)
	};

	const json::array &aliases
	{
		at<"content"_>(event).get("aliases")
	};

	for(const json::string &alias : aliases) try
	{
		m::room::aliases::cache::set(alias, room_id);

		log::info
		{
			m::log, "Updated aliases of %s by %s in %s with %s",
			string_view{room_id},
			json::get<"sender"_>(event),
			string_view{event.event_id},
			string_view{alias},
		};
	}
	catch(const std::exception &e)
	{
		log::error
		{
			m::log, "Updating aliases of %s by %s in %s with %s :%s",
			string_view{room_id},
			json::get<"sender"_>(event),
			string_view{event.event_id},
			string_view{alias},
			e.what(),
		};
	}
}

//
// auth handler
//

decltype(ircd::m::auth_room_aliases_hookfn)
ircd::m::auth_room_aliases_hookfn
{
	auth_room_aliases,
	{
		{ "_site",    "event.auth"      },
		{ "type",     "m.room.aliases"  },
	}
};

void
ircd::m::auth_room_aliases(const event &event,
                           event::auth::hookdata &data)
{
	using FAIL = m::event::auth::FAIL;
	using conforms = m::event::conforms;

	// 4. If type is m.room.aliases:
	assert(json::get<"type"_>(event) == "m.room.aliases");

	// a. If event has no state_key, reject.
	if(empty(json::get<"state_key"_>(event)))
		throw FAIL
		{
			"m.room.aliases event is missing a state_key."
		};

	// b. If sender's domain doesn't matches state_key, reject.
	if(json::get<"state_key"_>(event) != m::user::id(json::get<"sender"_>(event)).host())
		throw FAIL
		{
			"m.room.aliases event state_key is not the sender's domain."
		};

	// c. Otherwise, allow
	data.allow = true;
}

//
// m::room::aliases
//

bool
IRCD_MODULE_EXPORT
ircd::m::room::aliases::for_each(const m::room &room,
                                 const string_view &server,
                                 const closure_bool &closure)
{
	const room::state state
	{
		room
	};

	assert(server);
	const event::idx &event_idx
	{
		state.get(std::nothrow, "m.room.aliases", server)
	};

	if(!event_idx)
		return true;

	bool ret{true};
	m::get(std::nothrow, event_idx, "content", [&closure, &ret]
	(const json::object &content)
	{
		const json::array &aliases
		{
			content["aliases"]
		};

		for(auto it(begin(aliases)); it != end(aliases) && ret; ++it)
		{
			const json::string &alias(*it);
			if(!valid(m::id::ROOM_ALIAS, alias))
				continue;

			if(!closure(alias))
				ret = false;
		}
	});

	return ret;
}

//
// m::room::aliases::cache
//

bool
IRCD_MODULE_EXPORT
ircd::m::room::aliases::cache::del(const alias &alias)
{
	char swapbuf[m::id::room_alias::buf::SIZE];
	const string_view &key
	{
		alias.swap(swapbuf)
	};

	const auto &event_idx
	{
		alias_room.get(std::nothrow, "ircd.room.alias", key)
	};

	if(!event_idx)
		return false;

	const auto event_id
	{
		m::event_id(event_idx, std::nothrow)
	};

	if(!event_id)
		return false;

	const auto ret
	{
		redact(alias_room, m::me.user_id, event_id, "deleted")
	};

	return true;
}

bool
IRCD_MODULE_EXPORT
ircd::m::room::aliases::cache::set(const alias &alias,
                                   const id &id)
{
	char swapbuf[m::id::room_alias::buf::SIZE];
	const string_view &key
	{
		alias.swap(swapbuf)
	};

	const auto ret
	{
		send(alias_room, m::me.user_id, "ircd.room.alias", key,
		{
			{ "room_id", id }
		})
	};

	return true;
}

bool
IRCD_MODULE_EXPORT
ircd::m::room::aliases::cache::get(std::nothrow_t,
                                   const alias &alias,
                                   const id::closure &closure)
{
	char swapbuf[m::id::room_alias::buf::SIZE];
	const string_view &key
	{
		alias.swap(swapbuf)
	};

	m::event::idx event_idx
	{
		alias_room.get(std::nothrow, "ircd.room.alias", key)
	};

	if(!event_idx)
	{
		if(my_host(alias.host()))
			return false;

		if(!fetch(std::nothrow, alias, alias.host()))
			return false;

		event_idx = alias_room.get(std::nothrow, "ircd.room.alias", key);
	}

	time_t ts;
	if(!m::get(event_idx, "origin_server_ts", ts))
		return false;

	const seconds elapsed
	{
		(ircd::time<milliseconds>() - ts) / 1000L
	};

	const seconds &ttl
	{
		alias_cache_ttl
	};

	const bool expired
	{
		!my_host(alias.host()) && elapsed > ttl
	};

	if(expired)
	{
		log::dwarning
		{
			log, "Cached alias %s expired elapsed:%ld ttl:%ld",
			string_view{alias},
			elapsed.count(),
			ttl.count(),
		};

		fetch(std::nothrow, alias, alias.host());
		event_idx = alias_room.get(std::nothrow, "ircd.room.alias", key);
	}

	if(!event_idx)
		return false;

	bool ret{false};
	m::get(std::nothrow, event_idx, "content", [&closure, &ret]
	(const json::object &content)
	{
		const json::string &room_id
		{
			content.get("room_id")
		};

		if(!empty(room_id))
		{
			ret = true;
			closure(room_id);
		}
	});

	return ret;
}

namespace ircd::m
{
	thread_local char room_aliases_cache_fetch_hpbuf[384];
}

void
IRCD_MODULE_EXPORT
ircd::m::room::aliases::cache::fetch(const alias &alias,
                                     const net::hostport &hp)
try
{
	const unique_buffer<mutable_buffer> buf
	{
		16_KiB
	};

	m::v1::query::opts opts;
	opts.remote = hp;

	m::v1::query::directory request
	{
		alias, buf, std::move(opts)
    };

	request.wait(seconds(alias_fetch_timeout));
	const http::code &code
	{
		request.get()
	};

	const json::object response
	{
		request
	};

	if(!response.has("room_id"))
		throw m::NOT_FOUND
		{
			"Server '%s' does not know room_id for %s",
			string(room_aliases_cache_fetch_hpbuf, hp),
			string_view{alias},
		};

	const m::room::id &room_id
	{
		unquote(response["room_id"])
	};

	set(alias, room_id);
}
catch(const ctx::timeout &e)
{
	throw m::error
	{
		http::GATEWAY_TIMEOUT, "M_ROOM_ALIAS_TIMEOUT",
		"Server '%s' did not respond with a room_id for %s in time",
		string(room_aliases_cache_fetch_hpbuf, hp),
		string_view{alias},
	};
}
catch(const server::unavailable &e)
{
	throw m::error
	{
		http::BAD_GATEWAY, "M_ROOM_ALIAS_UNAVAILABLE",
		"Server '%s' is not available to query a room_id for %s",
		string(room_aliases_cache_fetch_hpbuf, hp),
		string_view{alias},
	};
}

bool
IRCD_MODULE_EXPORT
ircd::m::room::aliases::cache::has(const alias &alias)
{
	char swapbuf[m::id::room_alias::buf::SIZE];
	const string_view &key
	{
		alias.swap(swapbuf)
	};

	const auto &event_idx
	{
		alias_room.get(std::nothrow, "ircd.room.alias", key)
	};

	if(!event_idx)
		return false;

	time_t ts;
	if(!m::get(event_idx, "origin_server_ts", ts))
		return false;

	if(ircd::time() - ts > seconds(alias_cache_ttl).count())
		return false;

	bool ret{false};
	m::get(std::nothrow, event_idx, "content", [&ret]
	(const json::object &content)
	{
		const json::string &room_id
		{
			content.get("room_id")
		};

		ret = !empty(room_id);
	});

	return ret;
}

bool
IRCD_MODULE_EXPORT
ircd::m::room::aliases::cache::for_each(const string_view &server,
                                        const closure_bool &closure)
{
	const m::room::state state
	{
		alias_room
	};

	bool ret{true};
	const m::room::state::closure_bool reclosure{[&server, &closure, &ret]
	(const string_view &type, const string_view &state_key, const m::event::idx &event_idx)
	{
		thread_local char swapbuf[m::id::room_alias::buf::SIZE];
		const alias &alias
		{
			m::id::unswap(state_key, swapbuf)
		};

		if(server && alias.host() != server)
			return false;

		time_t ts;
		if(!m::get(event_idx, "origin_server_ts", ts))
			return true;

		if(ircd::time() - ts > seconds(alias_cache_ttl).count())
			return true;

		m::get(std::nothrow, event_idx, "content", [&closure, &ret, &alias]
		(const json::object &content)
		{
			const json::string &room_id
			{
				content.get("room_id")
			};

			if(!empty(room_id))
				ret = closure(alias, room_id);
		});

		return ret;
	}};

	state.for_each("ircd.room.alias", server, reclosure);
	return ret;
}
