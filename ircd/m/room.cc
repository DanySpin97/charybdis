// Matrix Construct
//
// Copyright (C) Matrix Construct Developers, Authors & Contributors
// Copyright (C) 2016-2018 Jason Volk <jason@zemos.net>
//
// Permission to use, copy, modify, and/or distribute this software for any
// purpose with or without fee is hereby granted, provided that the above
// copyright notice and this permission notice is present in all copies. The
// full license for this software is available in the LICENSE file.

#include <ircd/m/m.h>

const ircd::m::room::id::buf
init_room_id
{
	"init", ircd::my_host()
};

ircd::m::room
ircd::m::create(const id::room &room_id,
                const id::user &creator,
                const string_view &type)
{
	return create(room_id, creator, init_room_id, type);
}

ircd::m::room
ircd::m::create(const id::room &room_id,
                const id::user &creator,
                const id::room &parent,
                const string_view &type)
{
	json::iov event;
	json::iov content;
	const json::iov::push push[]
	{
		{ event,     { "sender",   creator  }},
		{ content,   { "creator",  creator  }},
	};

	const json::iov::add_if _parent
	{
		content, !parent.empty() && parent.local() != "init",
		{
			"parent", parent
		}
	};

	const json::iov::add_if _type
	{
		content, !type.empty() && type != "room",
		{
			"type", type
		}
	};

	json::iov::set _set[]
	{
		{ event,  { "depth",       0L               }},
		{ event,  { "type",        "m.room.create"  }},
		{ event,  { "state_key",   ""               }},
	};

	room room
	{
		room_id
	};

	commit(room, event, content);
	return room;
}

ircd::m::event::id::buf
ircd::m::join(const room &room,
              const m::id::user &user_id)
{
	return membership(room, user_id, "join");
}

ircd::m::event::id::buf
ircd::m::leave(const room &room,
               const m::id::user &user_id)
{
	return membership(room, user_id, "leave");
}

ircd::m::event::id::buf
ircd::m::membership(const room &room,
                    const m::id::user &user_id,
                    const string_view &membership)
{
	json::iov event;
	json::iov content;
	json::iov::push push[]
	{
		{ event,    { "type",       "m.room.member"  }},
		{ event,    { "sender",      user_id         }},
		{ event,    { "state_key",   user_id         }},
		{ event,    { "membership",  membership      }},
		{ content,  { "membership",  membership      }},
	};

/*
	if(this->membership(user_id, membership))
		throw m::ALREADY_MEMBER
		{
			"Member '%s' is already '%s'.", string_view{user_id}, membership
		};
*/
	return commit(room, event, content);
}

ircd::m::event::id::buf
ircd::m::message(const room &room,
                 const m::id::user &sender,
                 const string_view &body,
                 const string_view &msgtype)
{
	return message(room, sender,
	{
		{ "body",      body     },
		{ "msgtype",   msgtype  }
	});
}

ircd::m::event::id::buf
ircd::m::message(const room &room,
                 const m::id::user &sender,
                 const json::members &contents)
{
	return send(room, sender, "m.room.message", contents);
}

ircd::m::event::id::buf
ircd::m::send(const room &room,
              const m::id::user &sender,
              const string_view &type,
              const string_view &state_key,
              const json::members &contents)
{
	json::iov::push content[contents.size()];
	return send(room, sender, type, state_key, make_iov(content, contents.size(), contents));
}

ircd::m::event::id::buf
ircd::m::send(const room &room,
              const m::id::user &sender,
              const string_view &type,
              const string_view &state_key,
              const json::object &contents)
{
	json::iov::push content[contents.size()];
	return send(room, sender, type, state_key, make_iov(content, contents.size(), contents));
}

ircd::m::event::id::buf
ircd::m::send(const room &room,
              const m::id::user &sender,
              const string_view &type,
              const string_view &state_key,
              const json::iov &content)
{
	json::iov event;
	const json::iov::push push[]
	{
		{ event,    { "sender",     sender     }},
		{ event,    { "type",       type       }},
		{ event,    { "state_key",  state_key  }},
	};

	return commit(room, event, content);
}

ircd::m::event::id::buf
ircd::m::send(const room &room,
              const m::id::user &sender,
              const string_view &type,
              const json::members &contents)
{
	json::iov::push content[contents.size()];
	return send(room, sender, type, make_iov(content, contents.size(), contents));
}

ircd::m::event::id::buf
ircd::m::send(const room &room,
              const m::id::user &sender,
              const string_view &type,
              const json::object &contents)
{
	json::iov::push content[contents.count()];
	return send(room, sender, type, make_iov(content, contents.count(), contents));
}

ircd::m::event::id::buf
ircd::m::send(const room &room,
              const m::id::user &sender,
              const string_view &type,
              const json::iov &content)
{
	json::iov event;
	const json::iov::push push[]
	{
		{ event,    { "sender",  sender  }},
		{ event,    { "type",    type    }},
	};

	return commit(room, event, content);
}

ircd::m::event::id::buf
ircd::m::commit(const room &room,
                json::iov &event,
                const json::iov &contents)
{
	const json::iov::push room_id
	{
		event, { "room_id", room.room_id }
	};

	int64_t depth;
	const auto prev_event_id
	{
		head(room.room_id, depth)
	};

	//TODO: LCOCK
	const json::iov::set_if depth_
	{
		event, !event.has("depth"),
		{
			"depth", depth + 1
		}
	};

	const string_view auth_events{};
	const string_view prev_state{};

	json::value prev_event0[]
	{
		prev_event_id
	};

	json::value prev_events[]
	{
		{ prev_event0, 1 }
	};

	//TODO: LOLCK
	const json::iov::push prevs[]
	{
		{ event, { "auth_events",  auth_events  }},
		{ event, { "prev_state",   prev_state   }},
		{ event, { "prev_events",  { prev_events, 1 } } },
	};

	return m::vm::commit(event, contents);
}

int64_t
ircd::m::depth(const id::room &room_id)
{
	int64_t depth;
	head(room_id, depth);
	return depth;
}

ircd::m::id::event::buf
ircd::m::head(const id::room &room_id)
{
	int64_t depth;
	return head(room_id, depth);
}

ircd::m::id::event::buf
ircd::m::head(const id::room &room_id,
              int64_t &depth)
{
	const auto it
	{
		dbs::room_events.begin(room_id)
	};

	if(!it)
	{
		depth = -1;
		return {};
	}

	const auto &key{it->first};
	const auto part
	{
		dbs::room_events_key(key)
	};

	depth = std::get<0>(part);
	return std::get<1>(part);
}

bool
ircd::m::exists(const id::room &room_id)
{
	const auto it
	{
		dbs::room_events.begin(room_id)
	};

	return bool(it);
}

bool
ircd::m::my(const room &room)
{
	return my(room.room_id);
}

//
// room
//

ircd::m::room::room(const alias &alias,
                    const event::id &event_id)
:room_id{}
,event_id{event_id}
{
	assert(0); //TODO: translate
}

bool
ircd::m::room::membership(const m::id::user &user_id,
                          const string_view &membership)
const
{
	bool ret{false};
	static const string_view type{"m.room.member"};
	state{*this}.get(std::nothrow, type, user_id, [&membership, &ret]
	(const m::event &event)
	{
		assert(json::get<"type"_>(event) == "m.room.member");
		ret = at<"membership"_>(event) == membership;
	});

	return ret;
}

void
ircd::m::room::for_each(const event::closure &closure)
const
{
	auto it
	{
		dbs::room_events.begin(room_id)
	};

	event::fetch event;
	if(it) do
	{
		const auto &key{it->first};
		const auto part
		{
			dbs::room_events_key(key)
		};

		const auto event_id
		{
			std::get<1>(part)
		};

		if(seek(event, event_id, std::nothrow))
			closure(event);
	}
	while(++it);
}

bool
ircd::m::room::get(const event::closure &closure)
const try
{
	auto it
	{
		dbs::room_events.begin(room_id)
	};

	if(!it)
		return false;

	const auto &key{it->first};
	const auto part
	{
		dbs::room_events_key(key)
	};

	const auto event_id
	{
		std::get<1>(part)
	};

	const event::fetch event
	{
		event_id
	};

	closure(event);
	return true;
}
catch(const NOT_FOUND &)
{
	return false;
}

//
// room::state
//

void
ircd::m::room::state::get(const string_view &type,
                          const string_view &state_key,
                          const event::closure &closure)
const
{
	get(type, state_key, event::id::closure{[&closure]
	(const event::id &event_id)
	{
		event::fetch event
		{
			event_id
		};

		if(event.valid(event_id))
			closure(event);
	}});
}

void
ircd::m::room::state::get(const string_view &type,
                          const string_view &state_key,
                          const event::id::closure &closure)
const
{
	m::state::id_buffer buf;
	m::state::get(root(buf), type, state_key, [&closure]
	(const string_view &event_id)
	{
		closure(unquote(event_id));
	});
}

bool
ircd::m::room::state::get(std::nothrow_t,
                          const string_view &type,
                          const string_view &state_key,
                          const event::closure &closure)
const
{
	return get(std::nothrow, type, state_key, event::id::closure{[&closure]
	(const event::id &event_id)
	{
		event::fetch event
		{
			event_id, std::nothrow
		};

		if(event.valid(event_id))
			closure(event);
	}});
}

bool
ircd::m::room::state::get(std::nothrow_t,
                          const string_view &type,
                          const string_view &state_key,
                          const event::id::closure &closure)
const
{
	m::state::id_buffer buf;
	return m::state::get(std::nothrow, root(buf), type, state_key, [&closure]
	(const string_view &event_id)
	{
		closure(unquote(event_id));
	});
}

bool
ircd::m::room::state::has(const string_view &type)
const
{
	return test(type, event::id::closure_bool{[](const m::event::id &)
	{
		return true;
	}});
}

bool
ircd::m::room::state::has(const string_view &type,
                          const string_view &state_key)
const
{
	m::state::id_buffer buf;
	return m::state::get(std::nothrow, root(buf), type, state_key, []
	(const string_view &event_id)
	{
	});
}

bool
ircd::m::room::state::test(const event::closure_bool &closure)
const
{
	event::fetch event;
	return test(event::id::closure_bool{[&event, &closure]
	(const event::id &event_id)
	{
		if(seek(event, unquote(event_id), std::nothrow))
			if(closure(event))
				return true;

		return false;
	}});
}

bool
ircd::m::room::state::test(const event::id::closure_bool &closure)
const
{
	m::state::id_buffer buf;
	return m::state::test(root(buf), [&closure]
	(const json::array &key, const string_view &event_id)
	{
		return closure(unquote(event_id));
	});
}

bool
ircd::m::room::state::test(const string_view &type,
                           const event::closure_bool &closure)
const
{
	event::fetch event;
	return test(type, event::id::closure_bool{[&event, &closure]
	(const event::id &event_id)
	{
		if(seek(event, unquote(event_id), std::nothrow))
			if(closure(event))
				return true;

		return false;
	}});
}

bool
ircd::m::room::state::test(const string_view &type,
                           const event::id::closure_bool &closure)
const
{
	m::state::id_buffer buf;
	return m::state::test(root(buf), type, [&closure]
	(const json::array &key, const string_view &event_id)
	{
		return closure(unquote(event_id));
	});
}

void
ircd::m::room::state::for_each(const event::closure &closure)
const
{
	event::fetch event;
	for_each(event::id::closure{[&event, &closure]
	(const event::id &event_id)
	{
		if(seek(event, unquote(event_id), std::nothrow))
			closure(event);
	}});
}

void
ircd::m::room::state::for_each(const event::id::closure &closure)
const
{
	m::state::id_buffer buf;
	m::state::for_each(root(buf), [&closure]
	(const json::array &key, const string_view &event_id)
	{
		closure(unquote(event_id));
	});
}

void
ircd::m::room::state::for_each(const string_view &type,
                               const event::closure &closure)
const
{
	event::fetch event;
	for_each(type, event::id::closure{[&event, &closure]
	(const event::id &event_id)
	{
		if(seek(event, unquote(event_id), std::nothrow))
			closure(event);
	}});
}

void
ircd::m::room::state::for_each(const string_view &type,
                               const event::id::closure &closure)
const
{
	m::state::id_buffer buf;
	m::state::for_each(root(buf), type, [&closure]
	(const json::array &key, const string_view &event_id)
	{
		closure(unquote(event_id));
	});
}

ircd::string_view
ircd::m::room::state::root(m::state::id_buffer &buf)
const
{
	const event::id::buf event_id_buf
	{
		!room.event_id? head(room.room_id) : string_view{}
	};

	const event::id event_id
	{
		room.event_id? room.event_id : event_id_buf
	};

	return dbs::state_root(buf, room.room_id, event_id);
}

//
// room::members
//

void
ircd::m::room::members::for_each(const event::closure &view)
const
{
	test([&view](const m::event &event)
	{
		view(event);
		return false;
	});
}

void
ircd::m::room::members::for_each(const string_view &membership,
                                 const event::closure &view)
const
{
	test(membership, [&view](const m::event &event)
	{
		view(event);
		return false;
	});
}

bool
ircd::m::room::members::test(const event::closure_bool &view)
const
{
	const room::state state
	{
		room
	};

	static const string_view type{"m.room.member"};
	return state.test(type, event::closure_bool{[&view]
	(const m::event &event)
	{
		return view(event);
	}});
}

bool
ircd::m::room::members::test(const string_view &membership,
                             const event::closure_bool &view)
const
{
	//TODO: optimize with sequential column strat
	return test([&membership, &view](const event &event)
	{
		if(at<"membership"_>(event) == membership)
			if(view(event))
				return true;

		return false;
	});
}

//
// room::origins
//

void
ircd::m::room::origins::for_each(const closure &view)
const
{
	test([&view](const string_view &origin)
	{
		view(origin);
		return false;
	});
}

bool
ircd::m::room::origins::test(const closure_bool &view)
const
{
	db::index index
	{
		*dbs::events, "origin_joined in room_id"
	};

	auto it(index.begin(room.room_id));
	for(; bool(it); ++it)
	{
		const string_view &key(it->first);
		const string_view &origin(split(key, ":::").second); //TODO: XXX
		if(view(origin))
			return true;
	}

	return false;
}

//
// room::state::tuple
//

ircd::m::room::state::tuple::tuple(const m::room &room,
                                   const mutable_buffer &buf)
{

}

ircd::m::room::state::tuple::tuple(const json::array &pdus)
{
	for(const json::object &pdu : pdus)
	{
		const m::event &event{pdu};
		json::set(*this, at<"type"_>(event), event);
	}
}

std::string
ircd::m::pretty(const room::state::tuple &state)
{
	std::string ret;
	std::stringstream s;
	pubsetbuf(s, ret, 2048);

	const auto out{[&s]
	(const string_view &key, const auto &event)
	{
		if(!json::get<"event_id"_>(event))
			return;

		s << std::setw(28) << std::right << key
		  << " : " << at<"event_id"_>(event)
		  << " " << json::get<"sender"_>(event)
		  << " " << json::get<"depth"_>(event)
		  << " " << pretty_oneline(event::prev{event})
		  << std::endl;
	}};

	json::for_each(state, out);
	resizebuf(s, ret);
	return ret;
}

std::string
ircd::m::pretty_oneline(const room::state::tuple &state)
{
	std::string ret;
	std::stringstream s;
	pubsetbuf(s, ret, 1024);

	const auto out{[&s]
	(const string_view &key, const auto &event)
	{
		if(!json::get<"event_id"_>(event))
			return;

		s << key << " ";
	}};

	json::for_each(state, out);
	resizebuf(s, ret);
	return ret;
}
