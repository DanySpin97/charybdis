/*
 * charybdis: oh just a little chat server
 * ctx.h: userland context switching (stackful coroutines)
 *
 * Copyright (C) 2016 Charybdis Development Team
 * Copyright (C) 2016 Jason Volk <jason@zemos.net>
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice is present in all copies.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
 * IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

#pragma once
#define HAVE_IRCD_CTX_H

/// Userspace Contexts: cooperative threading from stackful coroutines.
///
/// This is the public interface to the userspace context system. No 3rd party
/// symbols are included from here. This file is included automatically in stdinc.h
/// and you do not have to include it manually.
///
/// There are two primary objects at work in the context system:
///
/// `struct context` <ircd/ctx/context.h>
/// Public interface emulating std::thread; included automatically from here.
/// To spawn and manipulate contexts, deal with this object.
///
/// `struct ctx` (ircd/ctx.cc)
/// Internal implementation of the context. This is not included here.
/// Several low-level functions are exposed for library creators. This file is usually
/// included when boost/asio.hpp is also included and calls are actually made into boost.
///
/// boost::asio is not included from here. To access that include boost in a
/// definition file with #include <ircd/asio.h>. That include contains some
/// devices we use to yield a context to asio.
///
namespace ircd::ctx
{
	IRCD_EXCEPTION(ircd::error, error)
	IRCD_EXCEPTION(error, interrupted)
	IRCD_EXCEPTION(error, timeout)

	using std::chrono::steady_clock;
	using time_point = steady_clock::time_point;

	struct ctx;

	const uint64_t &id(const ctx &);             // Unique ID for context
	string_view name(const ctx &);               // User's optional label for context
	const int64_t &notes(const ctx &);           // Peeks at internal semaphore count
	bool interruption(const ctx &);              // Context was marked for interruption
	bool finished(const ctx &);                  // Context function returned (or exception).
	bool started(const ctx &);                   // Context was ever entered.

	IRCD_OVERLOAD(threadsafe)
	void interrupt(ctx &);                       // Interrupt the context for termination.
	void signal(ctx &, std::function<void ()>);  // Post function to context strand
	void notify(ctx &, threadsafe_t);            // Notify context with threadsafety.
	bool notify(ctx &);                          // Queue a context switch to arg
	void yield(ctx &);                           // Direct context switch to arg
}

/// Interface to the currently running context
namespace ircd::ctx { inline namespace this_ctx
{
	struct critical_assertion;                   // Assert no yielding for a section

	// Always set to the currently running context or null for main stack
	extern __thread struct ctx *current;

	ctx &cur();                                  // Assumptional reference to *current

	void wait();                                 // Returns when context is woken up.
	void yield();                                // Allow other contexts to run before returning.

	void interruption_point();                   // throws interrupted if interruption_requested()
	bool interruption_requested();               // interruption(cur())

	// Return remaining time if notified; or <= 0 if not, and timeout thrown on throw overloads
	microseconds wait(const microseconds &, const std::nothrow_t &);
	template<class E, class duration> nothrow_overload<E, duration> wait(const duration &);
	template<class E = timeout, class duration> throw_overload<E, duration> wait(const duration &);

	// Returns false if notified; true if time point reached, timeout thrown on throw_overloads
	bool wait_until(const time_point &tp, const std::nothrow_t &);
	template<class E> nothrow_overload<E, bool> wait_until(const time_point &tp);
	template<class E = timeout> throw_overload<E> wait_until(const time_point &tp);

	// Ignores notes. Throws if interrupted.
	void sleep_until(const time_point &tp);
	template<class duration> void sleep(const duration &);
	void sleep(const int &secs);
}}

#include "ctx/context.h"
#include "ctx/prof.h"
#include "ctx/dock.h"
#include "ctx/view.h"
#include "ctx/queue.h"
#include "ctx/mutex.h"
#include "ctx/shared_mutex.h"
#include "ctx/shared_state.h"
#include "ctx/promise.h"
#include "ctx/future.h"
#include "ctx/async.h"
#include "ctx/pool.h"
#include "ctx/ole.h"
#include "ctx/fault.h"

namespace ircd
{
	//using yield = boost::asio::yield_context;
	using ctx::timeout;
	using ctx::context;
	using ctx::sleep;
}

/// An instance of critical_assertion detects an attempt to context switch
/// when the developer specifically does not want any yielding in that section
/// or anywhere up the stack from it. This device does not prevent a switch
/// and may carry no meaning outside of debug-mode compilation. It is good
/// practice to use this device even when it appears obvious the section's
/// callgraph has no chance of yielding; code changes, and everything up
/// up the graph can change without taking notice of your section.
///
class ircd::ctx::this_ctx::critical_assertion
{
	bool theirs;

  public:
	critical_assertion();
	~critical_assertion() noexcept;
};

/// This overload matches ::sleep() and acts as a drop-in for ircd contexts.
/// interruption point.
inline void
ircd::ctx::this_ctx::sleep(const int &secs)
{
	sleep(seconds(secs));
}

/// Yield the context for a period of time and ignore notifications. sleep()
/// is like wait() but it only returns after the timeout and not because of a
/// note.
/// interruption point.
template<class duration>
void
ircd::ctx::this_ctx::sleep(const duration &d)
{
	sleep_until(steady_clock::now() + d);
}

/// Wait for a notification until a point in time. If there is a notification
/// then context continues normally. If there's never a notification then an
/// exception (= timeout) is thrown.
/// interruption point.
template<class E>
ircd::throw_overload<E>
ircd::ctx::this_ctx::wait_until(const time_point &tp)
{
	if(wait_until<std::nothrow_t>(tp))
		throw E();
}

/// Wait for a notification until a point in time. If there is a notification
/// then returns true. If there's never a notification then returns false.
/// interruption point. this is not noexcept.
template<class E>
ircd::nothrow_overload<E, bool>
ircd::ctx::this_ctx::wait_until(const time_point &tp)
{
	return wait_until(tp, std::nothrow);
}

/// Wait for a notification for at most some amount of time. If the duration is
/// reached without a notification then E (= timeout) is thrown. Otherwise,
/// returns the time remaining on the duration.
/// interruption point
template<class E,
         class duration>
ircd::throw_overload<E, duration>
ircd::ctx::this_ctx::wait(const duration &d)
{
	const auto ret(wait<std::nothrow_t>(d));
	return ret <= duration(0)? throw E() : ret;
}

/// Wait for a notification for some amount of time. This function returns
/// when a context is notified. It always returns the duration remaining which
/// will be <= 0 to indicate a timeout without notification.
/// interruption point. this is not noexcept.
template<class E,
         class duration>
ircd::nothrow_overload<E, duration>
ircd::ctx::this_ctx::wait(const duration &d)
{
	using std::chrono::duration_cast;

	const auto ret(wait(duration_cast<microseconds>(d), std::nothrow));
	return duration_cast<duration>(ret);
}

/// Reference to the currently running context. Call if you expect to be in a
/// context. Otherwise use the ctx::current pointer.
inline ircd::ctx::ctx &
ircd::ctx::cur()
{
	assert(current);
	return *current;
}
