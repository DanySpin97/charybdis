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
post__receipt(client &client,
              const resource::request &request,
              const m::room::id &room_id)
{
	if(request.parv.size() < 4)
		throw m::BAD_REQUEST
		{
			"receipt type and event_id required"
		};

	const string_view &receipt_type
	{
		request.parv[2]
	};

	m::event::id::buf event_id
	{
		url::decode(request.parv[3], event_id)
	};

	return resource::response
	{
		client, http::OK
	};
}
