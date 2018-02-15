// Matrix Construct
//
// Copyright (C) Matrix Construct Developers, Authors & Contributors
// Copyright (C) 2016-2018 Jason Volk <jason@zemos.net>
//
// Permission to use, copy, modify, and/or distribute this software for any
// purpose with or without fee is hereby granted, provided that the above
// copyright notice and this permission notice is present in all copies. The
// full license for this software is available in the LICENSE file.

#include "rooms.h"

using namespace ircd;

resource::response
put__typing(client &client,
            const resource::request &request,
            const m::room::id &room_id)
{
	if(request.parv.size() < 3)
		throw m::NEED_MORE_PARAMS
		{
			"user_id parameter missing"
		};

	m::user::id::buf user_id
	{
		url::decode(request.parv[2], user_id)
	};

	static const milliseconds timeout_default
	{
		30 * 1000
	};

	const auto timeout
	{
		request.get("timeout", timeout_default)
	};

	const auto typing
	{
		request.at<bool>("typing")
	};

	log::debug("%s typing: %d timeout: %ld",
	           user_id,
	           typing,
	           timeout.count());

	return resource::response
	{
		client, http::OK
	};
}
