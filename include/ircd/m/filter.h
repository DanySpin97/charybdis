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
#define HAVE_IRCD_M_FILTER_H
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wsubobject-linkage"

namespace ircd::m
{
	struct filter;
	struct room_filter;
	struct event_filter;
	struct room_event_filter;
}

struct ircd::m::event_filter
:json::tuple
<
	json::property<name::limit, uint>,
	json::property<name::types, json::array>,
	json::property<name::senders, json::array>,
	json::property<name::not_types, json::array>,
	json::property<name::not_senders, json::array>
>
{
	using super_type::tuple;
	using super_type::operator=;
};

struct ircd::m::room_event_filter
:json::tuple
<
	json::property<name::limit, uint>,
	json::property<name::types, json::array>,
	json::property<name::rooms, json::array>,
	json::property<name::senders, json::array>,
	json::property<name::not_types, json::array>,
	json::property<name::not_rooms, json::array>,
	json::property<name::not_senders, json::array>
>
{
	using super_type::tuple;
	using super_type::operator=;
};

struct ircd::m::room_filter
:json::tuple
<
	json::property<name::rooms, json::array>,
	json::property<name::not_rooms, json::array>,
	json::property<name::state, room_event_filter>,
	json::property<name::timeline, room_event_filter>,
	json::property<name::ephemeral, room_event_filter>,
	json::property<name::account_data, room_event_filter>,
	json::property<name::include_leave, bool>
>
{
	using super_type::tuple;
	using super_type::operator=;
};

struct ircd::m::filter
:json::tuple
<
	json::property<name::event_fields, json::array>,
	json::property<name::event_format, string_view>,
	json::property<name::account_data, event_filter>,
	json::property<name::room, room_filter>,
	json::property<name::presence, event_filter>
>
{
	using super_type::tuple;
	using super_type::operator=;

	using closure = std::function<void (const json::object &)>;

	static bool get(std::nothrow_t, const user &, const string_view &filter_id, const closure &);
	static void get(const user &, const string_view &filter_id, const closure &);
	static string_view set(const mutable_buffer &id, const user &, const json::object &filter);

	filter(const user &, const string_view &filter_id, const mutable_buffer &);
};

#pragma GCC diagnostic pop
