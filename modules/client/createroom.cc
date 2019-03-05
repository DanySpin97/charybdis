// Matrix Construct
//
// Copyright (C) Matrix Construct Developers, Authors & Contributors
// Copyright (C) 2016-2018 Jason Volk <jason@zemos.net>
//
// Permission to use, copy, modify, and/or distribute this software for any
// purpose with or without fee is hereby granted, provided that the above
// copyright notice and this permission notice is present in all copies. The
// full license for this software is available in the LICENSE file.

using namespace ircd::m;
using namespace ircd;

mapi::header
IRCD_MODULE
{
	"Client 7.1.1 :Create Room"
};

extern "C" room
createroom__parent_type(const id::room &room_id,
                        const id::user &creator,
                        const id::room &parent,
                        const string_view &type);

extern "C" room
createroom__type(const id::room &room_id,
                 const id::user &creator,
                 const string_view &type);

extern "C" room
createroom_(const id::room &room_id,
            const id::user &creator);

const room::id::buf
init_room_id
{
	"init", ircd::my_host()
};

resource
createroom_resource
{
	"/_matrix/client/r0/createRoom",
	{
		"(7.1.1) Create a new room with various configuration options."
	}
};

resource::response
post__createroom(client &client,
                 const resource::request::object<m::createroom> &request)
try
{
	const id::user &sender_id
	{
		request.user_id
	};

	const id::room::buf room_id
	{
		id::generate, my_host()
	};

	const room room
	{
		createroom_(room_id, sender_id)
	};

	const event::id::buf join_event_id
	{
		join(room, sender_id)
	};

	// Takes precedence over events set by preset, but gets overriden by name
	// and topic keys.
	for(const json::object &event : json::get<"initial_state"_>(request))
	{
		const json::string &type(event["type"]);
		if(empty(type))
			continue;

		const json::string &state_key(event["state_key"]);
		const json::object &content(event["content"]);
		send(room, sender_id, type, state_key, content);
	}

	if(json::get<"guest_can_join"_>(request))
	{
		send(room, sender_id, "m.room.guest_access", "",
		{
			{ "guest_access", "can_join" }
		});
	}

	if(json::get<"name"_>(request))
	{
		static const size_t name_max_len
		{
			// 14.2.1.3: The name of the room. This MUST NOT exceed 255 bytes.
			255
		};

		const auto name
		{
			trunc(json::get<"name"_>(request), name_max_len)
		};

		send(room, sender_id, "m.room.name", "",
		{
			{ "name", name }
		});
	}

	if(json::get<"topic"_>(request))
	{
		send(room, sender_id, "m.room.topic", "",
		{
			{ "topic", json::get<"topic"_>(request) }
		});
	}

	for(const json::string &_user_id : json::get<"invite"_>(request))
	{
		const m::user::id &user_id{_user_id};
		invite(room, user_id, sender_id);
	}

	return resource::response
	{
		client, http::CREATED,
		{
			{ "room_id", room_id },
		}
	};
}
catch(const db::not_found &e)
{
	throw m::error
	{
		http::CONFLICT, "M_ROOM_IN_USE", "The desired room name is in use."
	};

	throw m::error
	{
		http::CONFLICT, "M_ROOM_ALIAS_IN_USE", "An alias of the desired room is in use."
	};
}

resource::method
post_method
{
	createroom_resource, "POST", post__createroom,
	{
		post_method.REQUIRES_AUTH
	}
};

room
createroom_(const id::room &room_id,
            const id::user &creator)
{
	return createroom__type(room_id, creator, string_view{});
}

room
createroom__type(const id::room &room_id,
                 const id::user &creator,
                 const string_view &type)
{
	return createroom__parent_type(room_id, creator, init_room_id, type);
}

room
createroom__parent_type(const id::room &room_id,
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

	const json::iov::add _parent
	{
		content, !parent.empty() && parent.local() != "init",
		{
			"parent", [&parent]() -> json::value
			{
				return parent;
			}
		}
	};

	const json::iov::add _type
	{
		content, !type.empty() && type != "room",
		{
			"type", [&type]() -> json::value
			{
				return type;
			}
		}
	};

	const json::iov::push _push[]
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
