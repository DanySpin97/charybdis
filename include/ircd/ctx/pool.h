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
#define HAVE_IRCD_CTX_POOL_H

namespace ircd::ctx
{
	struct pool;
}

struct ircd::ctx::pool
{
	using closure = std::function<void ()>;

  private:
	const char *name;
	size_t stack_size;
	size_t running;
	size_t working;
	queue<closure> q;
	std::vector<context> ctxs;

	void next();
	void main() noexcept;

  public:
	// indicators
	auto size() const                            { return ctxs.size();                             }
	auto queued() const                          { return q.size();                                }
	auto active() const                          { return working;                                 }
	auto avail() const                           { return running - working;                       }
	auto pending() const                         { return active() + queued();                     }

	// control panel
	void add(const size_t & = 1);
	void del(const size_t & = 1);
	void set(const size_t &);
	void terminate();
	void interrupt();
	void join();

	// dispatch function to pool
	void operator()(closure);

	// dispatch function std async style
	template<class F, class... A> future_void<F, A...> async(F&&, A&&...);
	template<class F, class... A> future_value<F, A...> async(F&&, A&&...);

	pool(const char *const &name    = "<unnamed pool>",
	     const size_t &stack_size   = DEFAULT_STACK_SIZE,
	     const size_t &             = 0);

	pool(pool &&) = delete;
	pool(const pool &) = delete;
	pool &operator=(pool &&) = delete;
	pool &operator=(const pool &) = delete;
	~pool() noexcept;

	friend void debug_stats(const pool &);
};

template<class F,
         class... A>
ircd::ctx::future_value<F, A...>
ircd::ctx::pool::async(F&& f,
                       A&&... a)
{
	using R = typename std::result_of<F (A...)>::type;

	auto func
	{
		std::bind(std::forward<F>(f), std::forward<A>(a)...)
	};

	promise<R> p;
	future<R> ret{p};
	(*this)([p(std::move(p)), func(std::move(func))]
	() -> void
	{
		p.set_value(func());
	});

	return ret;
}

template<class F,
         class... A>
ircd::ctx::future_void<F, A...>
ircd::ctx::pool::async(F&& f,
                       A&&... a)
{
	using R = typename std::result_of<F (A...)>::type;

	auto func
	{
		std::bind(std::forward<F>(f), std::forward<A>(a)...)
	};

	promise<R> p;
	future<R> ret{p};
	(*this)([p(std::move(p)), func(std::move(func))]
	() -> void
	{
		func();
		p.set_value();
	});

	return ret;
}
