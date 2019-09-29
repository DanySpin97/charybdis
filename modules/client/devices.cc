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

static void
_get_device(json::stack::object &obj,
            const m::user &user,
            const string_view &device_id);

static m::resource::response
get__devices_all(client &client,
                 const m::resource::request &request,
                 const m::room user_room);

static m::resource::response
get__devices(client &client,
             const m::resource::request &request);

static m::resource::response
put__devices(client &client,
             const m::resource::request &request);

static m::resource::response
delete__devices(client &client,
                const m::resource::request &request);

extern const std::string flows;

mapi::header
IRCD_MODULE
{
	"Client 11.9 Device Management"
};

ircd::m::resource
devices_resource
{
	"/_matrix/client/r0/devices/",
	{
		"(11.9) Device Management",
		resource::DIRECTORY,
	}
};

ircd::m::resource
devices_resource__unstable
{
	"/_matrix/client/unstable/devices/",
	{
		"(11.9) Device Management",
		resource::DIRECTORY,
	}
};

m::resource::method
method_get
{
	devices_resource, "GET", get__devices,
	{
		method_get.REQUIRES_AUTH
	}
};

m::resource::method
method_get__unstable
{
	devices_resource__unstable, "GET", get__devices,
	{
		method_get.REQUIRES_AUTH
	}
};

m::resource::method
method_delete
{
	devices_resource, "DELETE", delete__devices,
	{
		method_delete.REQUIRES_AUTH
	}
};

m::resource::method
method_delete__unstable
{
	devices_resource__unstable, "DELETE", delete__devices,
	{
		method_delete.REQUIRES_AUTH
	}
};

m::resource::method
method_put
{
	devices_resource, "PUT", put__devices,
	{
		method_put.REQUIRES_AUTH
	}
};

m::resource::method
method_put__unstable
{
	devices_resource__unstable, "PUT", put__devices,
	{
		method_put.REQUIRES_AUTH
	}
};

m::resource::response
get__devices(client &client,
             const m::resource::request &request)
{
	const m::user::room user_room
	{
		request.user_id
	};

	if(request.parv.size() < 1)
		return get__devices_all(client, request, user_room);

	m::id::device::buf device_id
	{
		url::decode(device_id, request.parv[0])
	};

	if(!m::device::has(request.user_id, device_id))
		throw m::NOT_FOUND
		{
			"Device ID '%s' not found", device_id
		};

	m::resource::response::chunked response
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

	_get_device(top, request.user_id, device_id);
	return {};
}

m::resource::response
put__devices(client &client,
             const m::resource::request &request)
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

	m::device data{request.content};
	json::get<"device_id"_>(data) = device_id;

	m::device::set(request.user_id, data);

	return m::resource::response
	{
		client, http::OK
	};
}

m::resource::response
delete__devices(client &client,
                const m::resource::request &request)
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

	const json::object &auth
	{
		request["auth"]
	};

	// 14.10.2 Security considerations
	const json::string &type{auth["type"]};
	if(type != "m.login.password")
		return m::resource::response
		{
			client, http::UNAUTHORIZED, json::object{flows}
		};

	const json::string &password{auth["password"]};
	if(!m::user(request.user_id).is_password(password))
		throw m::ACCESS_DENIED
		{
			"Incorrect password."
		};

	m::device::del(request.user_id, device_id);

	return m::resource::response
	{
		client, http::OK
	};
}

m::resource::response
get__devices_all(client &client,
                 const m::resource::request &request,
                 const m::room user_room)
{
	m::resource::response::chunked response
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

	m::device::for_each(request.user_id, [&request, &devices]
	(const string_view &device_id)
	{
		json::stack::object obj{devices};
		_get_device(obj, request.user_id, device_id);
		return true;
	});

	return {};
}

void
_get_device(json::stack::object &obj,
            const m::user &user,
            const string_view &device_id)
{
	json::stack::member
	{
		obj, "device_id", device_id
	};

	m::device::get(std::nothrow, user, device_id, "display_name", [&obj]
	(const string_view &value)
	{
		json::stack::member
		{
			obj, "display_name", unquote(value)
		};
	});

	m::device::get(std::nothrow, user, device_id, "last_seen_ip", [&obj]
	(const string_view &value)
	{
		json::stack::member
		{
			obj, "last_seen_ip", unquote(value)
		};
	});

	m::device::get(std::nothrow, user, device_id, "last_seen_ts", [&obj]
	(const string_view &value)
	{
		json::stack::member
		{
			obj, "last_seen_ts", value
		};
	});
}

const std::string
flows
{
	ircd::string(512 | SHRINK_TO_FIT, [](const mutable_buffer &buf)
	{
		json::stack out{buf};
		{
			json::stack::object top{out};
			json::stack::array flows{top, "flows"};
			json::stack::object flow{flows};
			json::stack::array stages{flow, "stages"};
			stages.append("m.login.password");
		}

		return out.completed();
	})
};
