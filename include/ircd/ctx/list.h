// Copyright (C) Matrix Construct Developers, Authors & Contributors
// Copyright (C) 2016-2018 Jason Volk
//
// Permission to use, copy, modify, and/or distribute this software for any
// purpose with or without fee is hereby granted, provided that the above
// copyright notice and this permission notice is present in all copies.
// 
// THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT,
// INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
// (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
// HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
// STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
// IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.

#pragma once
#define HAVE_IRCD_CTX_LIST_H

namespace ircd::ctx
{
	struct list;
}

/// A special linked-list for contexts. Each ircd::ctx has space for one and
/// only one node on its internal ctx::ctx structure. It can only participate
/// in one ctx::list at a time. This forms the structural basis for mutexes,
/// condition variables and other interleaving primitives which form queues
/// of contexts.
///
/// This device is strictly for context switching purposes. It is minimal,
/// usage is specific to this purpose, and not a general list to be used
/// elsewhere. Furthermore, this is too lightweight for even the
/// ircd::allocator::node strategy. Custom operations are implemented for
/// maximum space efficiency in both the object instance and the ctx::ctx.
///
class ircd::ctx::list
{
	ctx *head {nullptr};
	ctx *tail {nullptr};

	// Get next or prev entry in ctx
	static const ctx *next(const ctx *const &);
	static const ctx *prev(const ctx *const &);
	static ctx *next(ctx *const &);
	static ctx *prev(ctx *const &);

  public:
	const ctx *front() const;
	const ctx *back() const;
	ctx *front();
	ctx *back();

	// until convention
	bool until(const std::function<bool (const ctx &)> &) const;
	bool until(const std::function<bool (ctx &)> &);

	// reverse until convention
	bool runtil(const std::function<bool (const ctx &)> &) const;
	bool runtil(const std::function<bool (ctx &)> &);

	// iteration
	void for_each(const std::function<void (const ctx &)> &) const;
	void for_each(const std::function<void (ctx &)> &);

	// reverse iteration
	void rfor_each(const std::function<void (const ctx &)> &) const;
	void rfor_each(const std::function<void (ctx &)> &);

	bool empty() const;
	size_t size() const;

	void push_front(ctx *const & = current);
	void push_back(ctx *const & = current);
	void push(ctx *const & = current);       // push_back

	ctx *pop_front();
	ctx *pop_back();
	ctx *pop();                              // pop_front

	void remove(ctx *const & = current);

	list() = default;
	list(list &&) noexcept;
	list(const list &) = delete;
	list &operator=(list &&) noexcept;
	list &operator=(const list &) = delete;
	~list() noexcept;
};

inline
ircd::ctx::list::list(list &&o)
noexcept
:head{std::move(o.head)}
,tail{std::move(o.tail)}
{
	o.head = nullptr;
	o.tail = nullptr;
}

inline
ircd::ctx::list &
ircd::ctx::list::operator=(list &&o)
noexcept
{
	this->~list();
	std::swap(head, o.head);
	std::swap(tail, o.tail);
	return *this;
}

inline
ircd::ctx::list::~list()
noexcept
{
	assert(empty());
}

inline ircd::ctx::ctx *
ircd::ctx::list::pop()
{
	return pop_front();
}

inline void
ircd::ctx::list::push(ctx *const &c)
{
	push_back(c);
}

inline size_t
ircd::ctx::list::size()
const
{
	size_t i{0};
	for_each([&i](const ctx &)
	{
		++i;
	});

	return i;
}

inline bool
ircd::ctx::list::empty()
const
{
	assert((!head && !tail) || (head && tail));
	return !head;
}

inline ircd::ctx::ctx *
ircd::ctx::list::back()
{
	return tail;
}

inline ircd::ctx::ctx *
ircd::ctx::list::front()
{
	return head;
}

inline const ircd::ctx::ctx *
ircd::ctx::list::back()
const
{
	return tail;
}

inline const ircd::ctx::ctx *
ircd::ctx::list::front()
const
{
	return head;
}
