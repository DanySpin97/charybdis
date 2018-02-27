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
#define HAVE_IRCD_M_V1_SEND_JOIN_H

namespace ircd::m::v1
{
	struct send_join;
};

struct ircd::m::v1::send_join
:server::request
{
	struct opts;

	operator json::array() const
	{
		return json::array{in.content};
	}

	send_join(const room::id &, const user::id &, const const_buffer &, const mutable_buffer &, opts);
};

struct ircd::m::v1::send_join::opts
{
	net::hostport remote;
	m::request request;
	server::out out;
	server::in in;
	const struct server::request::opts *sopts {nullptr};
};
