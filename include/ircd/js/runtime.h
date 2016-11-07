/*
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
#define HAVE_IRCD_JS_RUNTIME_H

namespace ircd {
namespace js   {

class runtime
:custom_ptr<JSRuntime>
{
	static void handle_error(JSContext *, const char *msg, JSErrorReport *) noexcept;
	static void handle_out_of_memory(JSContext *, void *) noexcept;
	static void handle_large_allocation_failure(void *) noexcept;
	static void handle_gc(JSRuntime *, JSGCStatus, void *) noexcept;
	static void handle_finalize(JSFreeOp *, JSFinalizeStatus, bool is_compartment, void *) noexcept;
	static void handle_telemetry(int id, uint32_t sample, const char *key) noexcept;
	static void handle_zone_sweep(JS::Zone *) noexcept;
	static void handle_zone_destroy(JS::Zone *) noexcept;
	static void handle_compartment_name(JSRuntime *, JSCompartment *, char *buf, size_t) noexcept;
	static void handle_compartment_destroy(JSFreeOp *, JSCompartment *) noexcept;
	static bool handle_context(JSContext *, uint op, void *) noexcept;
	static void handle_activity(void *priv, bool active) noexcept;
	static bool handle_interrupt(JSContext *) noexcept;

  public:
	struct opts
	{
		size_t max_bytes             = 64_MiB;
		size_t max_nursery_bytes     = 16_MiB;
		size_t code_stack_max        = 0;
		size_t trusted_stack_max     = 0;
		size_t untrusted_stack_max   = 0;
	};

	struct opts opts;                            // We keep a copy of the given opts here
	std::thread::id tid;                         // This is recorded for assertions/logging.

	operator JSRuntime *() const                 { return get();                                   }
	operator JSRuntime &() const                 { return custom_ptr<JSRuntime>::operator*();      }
	bool operator!() const                       { return !custom_ptr<JSRuntime>::operator bool(); }
	auto ptr() const                             { return get();                                   }
	auto ptr()                                   { return get();                                   }

	runtime(const struct opts &, runtime *const &parent = nullptr);
	runtime() = default;
	runtime(runtime &&) noexcept;
	runtime(const runtime &) = delete;
	runtime &operator=(runtime &&) noexcept;
	runtime &operator=(const runtime &) = delete;
	~runtime() noexcept;

	friend void interrupt(runtime &);
};

// Current thread_local runtime. Runtimes/Contexts are single-threaded and we maintain
// one per thread (or only one will be relevant on the given thread you are on).
// Do not construct more than one runtime on the same thread- this is overwritten.
extern __thread runtime *rt;

// Get to our `struct runtime` from any upstream JSRuntime
const runtime &our(const JSRuntime *const &);
runtime &our(JSRuntime *const &);

// Get to our runtime from any JSObject
const runtime &our_runtime(const JSObject &);
runtime &our_runtime(JSObject &);

// Get to our runtime from any JSFreeOp
JSFreeOp *default_freeop(runtime &);
const runtime &our_runtime(const JSFreeOp &);
runtime &our_runtime(JSFreeOp &);

// Do not call interrupt() unless you know what you're doing; see context.h.
void interrupt(runtime &r);
bool run_gc(runtime &r) noexcept;


inline void
interrupt(runtime &r)
{
	JS_RequestInterruptCallback(r);
}

inline runtime &
our_runtime(JSFreeOp &o)
{
	return our(o.runtime());
}

inline const runtime &
our_runtime(const JSFreeOp &o)
{
	return our(o.runtime());
}

inline JSFreeOp *
default_freeop(runtime &r)
{
	return JS_GetDefaultFreeOp(r);
}

inline runtime &
our_runtime(JSObject &o)
{
	return our(JS_GetObjectRuntime(&o));
}

inline const runtime &
our_runtime(const JSObject &o)
{
	return our(JS_GetObjectRuntime(const_cast<JSObject *>(&o)));
}

inline runtime &
our(JSRuntime *const &c)
{
	return *static_cast<runtime *>(JS_GetRuntimePrivate(c));
}

inline const runtime &
our(const JSRuntime *const &c)
{
	return *static_cast<const runtime *>(JS_GetRuntimePrivate(const_cast<JSRuntime *>(c)));
}


} // namespace js
} // namespace ircd
