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
#define HAVE_IRCD_M_DBS_H

namespace ircd::m::dbs
{
	struct init;
	struct cursor;

	// Database instance
	extern std::shared_ptr<db::database> events;

	// Event property columns
	constexpr const auto event_columns{event::size()};
	extern std::array<db::column, event_columns> event_column;

	// Event metadata columns
	extern db::column state_node;
	extern db::index room_events;

	// Lowlevel util
	string_view room_events_key(const mutable_buffer &out, const id::room &, const uint64_t &depth, const id::event &);
	std::tuple<uint64_t, string_view> room_events_key(const string_view &amalgam);

	// Get the state root for an event (with as much information as you have)
	string_view state_root(const mutable_buffer &out, const id::room &, const id::event &, const uint64_t &depth);
	string_view state_root(const mutable_buffer &out, const id::room &, const id::event &);
	string_view state_root(const mutable_buffer &out, const id::event &);
	string_view state_root(const mutable_buffer &out, const event &);

	// [GET] Query suite
	bool exists(const event::id &);

	// [SET (txn)] Basic write suite
	string_view write(db::txn &, const mutable_buffer &rootout, const string_view &rootin, const event &);
}

namespace ircd::m::dbs::desc
{
	// Full description
	extern const database::description events;

	// Direct columns
	extern const database::descriptor events_auth_events;
	extern const database::descriptor events_content;
	extern const database::descriptor events_depth;
	extern const database::descriptor events_event_id;
	extern const database::descriptor events_hashes;
	extern const database::descriptor events_membership;
	extern const database::descriptor events_origin;
	extern const database::descriptor events_origin_server_ts;
	extern const database::descriptor events_prev_events;
	extern const database::descriptor events_prev_state;
	extern const database::descriptor events_room_id;
	extern const database::descriptor events_sender;
	extern const database::descriptor events_signatures;
	extern const database::descriptor events_state_key;
	extern const database::descriptor events_type;

	// Metadata columns
	extern const database::descriptor events__state_node;
	extern const db::prefix_transform events__room_events__pfx;
	extern const db::comparator events__room_events__cmp;
	extern const database::descriptor events__room_events;
}

struct ircd::m::dbs::init
{
	init();
	~init() noexcept;
};
