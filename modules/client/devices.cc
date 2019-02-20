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
	"Client 11.9 Device Management"
};

ircd::resource
devices_resource
{
	"/_matrix/client/r0/devices/",
	{
		"(11.9) Device Management",
		resource::DIRECTORY,
	}
};

ircd::resource::redirect::permanent
devices_resource__unstable
{
	"/_matrix/client/unstable/devices/",
	"/_matrix/client/r0/devices/",
	{
		"(11.9) Device Management",
		resource::DIRECTORY,
	}
};

static resource::response
get__devices_all(client &client,
                 const resource::request &request,
                 const m::room user_room)
{
	resource::response::chunked response
	{
		client, http::OK
	};

	json::stack out
	{
		response.buf, response.flusher()
	};

	json::stack::object top
	{
		out
	};

	json::stack::array devices
	{
		top, "devices"
	};

	m::device::for_each(request.user_id, [&devices]
	(const m::device &device)
	{
		devices.append(device);
		return true;
	});

	return {};
}

resource::response
get__devices(client &client,
             const resource::request &request)
{
	const m::user::room user_room
	{
		request.user_id
	};

	if(request.parv.size() < 1)
		return get__devices_all(client, request, user_room);

	m::id::device::buf device_id
	{
		url::decode(device_id, request.parv[1])
	};

	std::string buf;
	m::device::get(request.user_id, device_id, [&buf]
	(const m::device &device)
	{
		buf = json::strung{device};
	});

	return resource::response
	{
		client, http::OK, json::object{buf}
	};
}

resource::method
method_get
{
	devices_resource, "GET", get__devices,
	{
		method_get.REQUIRES_AUTH
	}
};

resource::response
put__devices(client &client,
              const resource::request &request)
{
	if(request.parv.size() < 1)
		throw m::NEED_MORE_PARAMS
		{
			"device_id required"
		};

	const m::user::room user_room
	{
		request.user_id
	};

	m::id::device::buf device_id
	{
		url::decode(device_id, request.parv[1])
	};

	user_room.get("ircd.device", device_id, [&]
	(const m::event &event)
	{
		const json::object &content
		{
			at<"content"_>(event)
		};

		//TODO: where is your Object.update()??
		throw m::UNSUPPORTED{};
	});

	send(user_room, request.user_id, "ircd.device", device_id, request.content);

	return resource::response
	{
		client, http::OK
	};
}

resource::method
method_put
{
	devices_resource, "PUT", put__devices,
	{
		method_put.REQUIRES_AUTH
	}
};

resource::response
delete__devices(client &client,
                const resource::request &request)
{
	if(request.parv.size() < 1)
		throw m::NEED_MORE_PARAMS
		{
			"device_id required"
		};

	m::id::device::buf device_id
	{
		url::decode(device_id, request.parv[1])
	};

	m::device::del(request.user_id, device_id);

	return resource::response
	{
		client, http::OK
	};
}

resource::method
method_delete
{
	devices_resource, "DELETE", delete__devices,
	{
		method_delete.REQUIRES_AUTH
	}
};
