// The Construct
//
// Copyright (C) The Construct Developers, Authors & Contributors
// Copyright (C) 2016-2020 Jason Volk <jason@zemos.net>
//
// Permission to use, copy, modify, and/or distribute this software for any
// purpose with or without fee is hereby granted, provided that the above
// copyright notice and this permission notice is present in all copies. The
// full license for this software is available in the LICENSE file.

namespace ircd::m::acquire
{
	struct result;
	using list = std::list<result>;

	static void start(const opts &, list &);
	static bool handle(const opts &, ctx::future<m::fetch::result> &);
	static bool handle(const opts &, list &);
	static void fetch_head(const opts &, list &);
	static bool fetch_missing(const opts &, list &);
	static void submit(const opts &, list &);
};

struct ircd::m::acquire::result
{
	ctx::future<m::fetch::result> future;
	event::id::buf event_id;

	result(ctx::future<m::fetch::result> &&future, const event::id &event_id)
	:future{std::move(future)}
	,event_id{event_id}
	{}
};

decltype(ircd::m::acquire::log)
ircd::m::acquire::log
{
	"m.acquire"
};

//
// execute::execute
//

ircd::m::acquire::execute::execute(const opts &opts)
{
	list fetching;

	// Branch to acquire head
	if(opts.head)
		fetch_head(opts, fetching);

	// Branch to acquire missing
	if(opts.missing)
		for(size_t i(0); i < opts.rounds; ++i)
			if(!fetch_missing(opts, fetching))
				break;

	// Complete all work before returning, otherwise everything
	// will be cancelled on unwind.
	while(handle(opts, fetching));
}

bool
ircd::m::acquire::fetch_missing(const opts &opts,
                                list &fetching)
{
	const auto top
	{
		m::top(opts.room.room_id)
	};

	m::room::events::missing missing
	{
		opts.room
	};

	bool ret(false);
	missing.for_each(opts.depth, [&opts, &fetching, &top, &ret]
	(const event::id &event_id, const int64_t &ref_depth, const event::idx &ref_idx)
	{
		// Bail if interrupted
		if(ctx::interruption_requested())
			return false;

		// Branch if we have to measure the viewportion
		if(opts.viewport_size)
		{
			const m::event::idx_range range
			{
				std::min(ref_idx, std::get<event::idx>(top)),
				std::max(ref_idx, std::get<event::idx>(top)),
			};

			// Bail if this event sits above the viewport.
			if(m::room::events::count(opts.room, range) > opts.viewport_size)
				return false;
		}

		const auto ref_id
		{
			m::event_id(ref_idx)
		};

		const m::room ref_room
		{
			opts.room.room_id, ref_id
		};

		const auto &[sound_depth, sound_idx]
		{
			m::sounding(ref_room)
		};

		const auto &[twain_depth, _twain_idx]
		{
			sound_idx == ref_idx?
				m::twain(ref_room):
				std::make_pair(0L, 0UL)
		};

		auto _opts(opts);
		_opts.room.event_id = event_id;
		_opts.hint = opts.hint;
		_opts.viewport_size = twain_depth?
			std::clamp(sound_depth - twain_depth, 1L, 48L):
			1UL;

		submit(_opts, fetching);
		ret = true;
		log::debug
		{
			log, "Fetch %s miss prev of %s @%lu in %s @%lu sound:%lu twain:%ld fetching:%zu",
			string_view{event_id},
			string_view{ref_id},
			ref_depth,
			string_view{ref_room.room_id},
			std::get<int64_t>(top),
			sound_depth,
			twain_depth,
			fetching.size(),
		};

		return true;
	});

	return ret;
}

void
ircd::m::acquire::fetch_head(const opts &opts,
                             list &fetching)
{
	const auto handle_head{[&opts, &fetching]
	(const m::event &result)
	{
		// Bail if interrupted
		if(ctx::interruption_requested())
			return false;

		// Bail if the depth is below the window
		if(json::get<"depth"_>(result) < opts.depth.first)
			return false;

		auto _opts(opts);
		_opts.room.event_id = result.event_id;
		_opts.hint = json::get<"origin"_>(result);
		_opts.hint_only = true;
		_opts.viewport_size = 1; //XXX
		submit(_opts, fetching);
		return true;
	}};

	const auto top
	{
		m::top(opts.room.room_id)
	};

	m::room::head::fetch::opts hfopts;
	hfopts.room_id = opts.room.room_id;
	hfopts.top = top;
	m::room::head::fetch
	{
		hfopts, handle_head
	};
}

void
ircd::m::acquire::submit(const opts &opts,
                         list &fetching)
{
	start(opts, fetching);
	while(!fetching.empty())
		if(!handle(opts, fetching))
			break;
}

void
ircd::m::acquire::start(const opts &opts,
                        list &fetching)
{
	fetch::opts fopts;
	fopts.op = fetch::op::backfill;
	fopts.room_id = opts.room.room_id;
	fopts.event_id = opts.room.event_id;
	fopts.backfill_limit = opts.viewport_size;
	fopts.hint = opts.hint;
	fopts.attempt_limit = opts.hint_only;
	fetching.emplace_back(fetch::start(fopts), opts.room.event_id);
}

bool
ircd::m::acquire::handle(const opts &opts,
                         list &fetching)
{
	const bool full
	{
		fetching.size() >= opts.fetch_width
	};

	auto next
	{
		ctx::when_any(std::begin(fetching), std::end(fetching), []
		(auto &it) -> ctx::future<m::fetch::result> &
		{
			return it->future;
		})
	};

	const milliseconds timeout
	{
		full? 5000: 50
	};

	if(!next.wait(timeout, std::nothrow))
		return full;

	const unique_iterator it
	{
		fetching, next.get()
	};

	assert(it.it != std::end(fetching));

	auto _opts(opts);
	_opts.room.event_id = it.it->event_id;
	return handle(_opts, it.it->future);
}

bool
ircd::m::acquire::handle(const opts &opts,
                         ctx::future<m::fetch::result> &future)
try
{
	auto result
	{
		future.get()
	};

	const json::object response
	{
		result
	};

	const json::array pdus
	{
		response["pdus"]
	};

	log::debug
	{
		log, "Eval %zu for %s in %s",
		pdus.size(),
		string_view{opts.room.event_id},
		string_view{opts.room.room_id},
	};

	m::vm::opts vmopts;
	vmopts.infolog_accept = true;
	vmopts.warnlog &= ~vm::fault::EXISTS;
	//vmopts.phase.set(m::vm::phase::NOTIFY, false);
	vmopts.phase.set(m::vm::phase::FETCH_PREV, false);
	vmopts.phase.set(m::vm::phase::FETCH_STATE, false);
	vmopts.wopts.appendix.set(dbs::appendix::ROOM_HEAD, false);
	ctx::interruption_point();
	m::vm::eval
	{
		pdus, vmopts
	};

	return true;
}
catch(const ctx::interrupted &e)
{
	throw;
}
catch(const std::exception &e)
{
	log::error
	{
		log, "Eval %s in %s :%s",
		string_view{opts.room.event_id},
		string_view{opts.room.room_id},
		e.what(),
	};

	return true;
}
