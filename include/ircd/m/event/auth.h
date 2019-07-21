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
#define HAVE_IRCD_M_EVENT_AUTH_H

struct ircd::m::event::auth
{
	struct refs;
	struct chain;
	struct hookdata;
	using passfail = std::tuple<bool, std::exception_ptr>;
	IRCD_M_EXCEPTION(error, FAIL, http::UNAUTHORIZED)

	static bool is_power_event(const event &);
	static passfail check(std::nothrow_t, const event &);
	static void check(const event &);
};

struct ircd::m::event::auth::hookdata
{
	event::prev prev;
	vector_view<const event *> auth_events;
	const event *auth_create {nullptr};
	const event *auth_power {nullptr};
	const event *auth_join_rules {nullptr};
	const event *auth_member_target {nullptr};
	const event *auth_member_sender {nullptr};

	bool allow {false};
	std::exception_ptr fail;

	hookdata(const event &, const vector_view<const event *> &auth_events);
};

/// Interface to the references made by other power events to this power
/// event in the `auth_events`. This interface only deals with power events,
/// it doesn't care if a non-power event referenced a power event. This does
/// not contain the auth-chain or state resolution algorithm here, those are
/// later constructed out of this data.
struct ircd::m::event::auth::refs
{
	event::idx idx;

  public:
	using closure_bool = event::closure_idx_bool;

	bool for_each(const string_view &type, const closure_bool &) const;
	bool for_each(const closure_bool &) const;

	bool has(const string_view &type) const noexcept;
	bool has(const event::idx &) const noexcept;

	size_t count(const string_view &type) const noexcept;
	size_t count() const noexcept;

	refs(const event::idx &idx)
	:idx{idx}
	{}
};

struct ircd::m::event::auth::chain
{
	using closure_bool = event::closure_idx_bool;
	using closure = event::closure_idx;

	event::idx idx;

	static bool for_each(const auth::chain &, const closure_bool &);

  public:
	bool for_each(const closure_bool &) const;
	bool for_each(const closure &) const;

	bool has(const string_view &type) const noexcept;
	size_t depth() const noexcept;

	chain(const event::idx &idx)
	:idx{idx}
	{}
};
