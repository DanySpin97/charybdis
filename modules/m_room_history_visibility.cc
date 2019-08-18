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

mapi::header
IRCD_MODULE
{
	"Matrix m.room.history_visibility"
};

static bool
_visible_to_user(const m::event &event,
                 const m::user::id &user_id,
                 const m::room &room,
                 const string_view &history_visibility)
{
	char membership_buf[m::room::MEMBERSHIP_MAX_SIZE];
	string_view membership
	{
		m::membership(membership_buf, room, user_id)
	};

	if(membership == "join")
		return true;

	if(history_visibility == "joined")
		return false;

	if(history_visibility == "invited")
		return membership == "invite";

	assert(history_visibility == "shared");
	if(membership == "invite")
		return true;

	// If the room is not at the present event then we have to run another
	// test for membership here. Otherwise the "join" test already failed.
	if(room.event_id)
	{
		const m::room present{room.room_id};
		membership = m::membership(membership_buf, present, user_id);
		return membership == "join" || membership == "invite";
	}

	return false;
}

static bool
_visible_to_node(const m::event &event,
                 const string_view &node_id,
                 const m::room &room,
                 const string_view &history_visibility)
{
	const m::room::origins origins
	{
		room
	};

	// Allow joined servers
	if(origins.has(node_id))
		return true;

	// Allow auth chain events XXX: this is too broad
	if(m::room::auth::is_power_event(event))
		return true;

	// Allow any event where the state_key string is a user mxid and the server
	// is the host of that user. Note that applies to any type of event.
	if(m::valid(m::id::USER, json::get<"state_key"_>(event)))
		if(m::user::id(at<"state_key"_>(event)).host() == node_id)
			return true;

	return false;
}

static bool
_visible(const m::event &event,
         const string_view &mxid,
         const m::room &room,
         const string_view &history_visibility)
{
	if(history_visibility == "world_readable")
		return true;

	if(empty(mxid))
		return false;

	if(m::valid(m::id::USER, mxid))
		return _visible_to_user(event, mxid, room, history_visibility);

	if(rfc3986::valid_remote(std::nothrow, mxid))
		return _visible_to_node(event, mxid, room, history_visibility);

	throw m::UNSUPPORTED
	{
		"Cannot determine visibility for '%s'", mxid
	};
}

bool
IRCD_MODULE_EXPORT
ircd::m::visible(const m::event &event,
                 const string_view &mxid)
{
	const m::room room
	{
		at<"room_id"_>(event), event.event_id
	};

	static const m::event::fetch::opts fopts
	{
		m::event::keys::include{"content"}
	};

	const m::room::state state
	{
		room, &fopts
	};

	bool ret{false};
	const bool has_state_event
	{
		state.get(std::nothrow, "m.room.history_visibility", "", [&]
		(const m::event &visibility_event)
		{
			const json::object &content
			{
				json::get<"content"_>(visibility_event)
			};

			const string_view &history_visibility
			{
				unquote(content.get("history_visibility", "shared"))
			};

			ret = _visible(event, mxid, room, history_visibility);
		})
	};

	return !has_state_event?
		_visible(event, mxid, room, "shared"):
		ret;
}

static void
_changed_visibility(const m::event &event,
                    m::vm::eval &)
{
	log::info
	{
		m::log, "Changed visibility of %s to %s by %s => %s",
		json::get<"room_id"_>(event),
		json::get<"content"_>(event).get("history_visibility"),
		json::get<"sender"_>(event),
		string_view{event.event_id}
	};
}

m::hookfn<m::vm::eval &>
_changed_visibility_hookfn
{
	_changed_visibility,
	{
		{ "_site",    "vm.effect"                  },
		{ "type",     "m.room.history_visibility"  },
	}
};
