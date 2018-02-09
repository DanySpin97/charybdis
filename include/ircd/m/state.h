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
#define HAVE_IRCD_M_STATE_H

/// Matrix machine state unit and bus.
///
/// This section deals specifically with the aspect of Matrix called "state"
/// providing tools and utilities as well as local databasing. IO is done for
/// reads, and indirect into db::txn's for writes. No network activities are
/// conducted here.
///
/// These tools allow the user to query aspects of the "state" of a room at
/// the point of any event. Composed out of these queries are a suite of more
/// utilities to efficiently aid the Matrix virtual machine with the rest of
/// its tasks.
///
namespace ircd::m::state
{
	struct node;

	constexpr size_t ID_MAX_SZ { 64 };
	constexpr size_t KEY_MAX_SZ { 256 + 256 + 16 };
	constexpr size_t VAL_MAX_SZ { 256 + 16 };
	constexpr size_t NODE_MAX_SZ { 4_KiB };
	constexpr size_t NODE_MAX_KEY { 2 }; // tmp for now
	constexpr size_t NODE_MAX_VAL { NODE_MAX_KEY };
	constexpr size_t NODE_MAX_DEG { NODE_MAX_KEY + 1 }; // tmp for now
	constexpr int8_t MAX_HEIGHT { 16 }; // good for few mil at any degree :)

	using id_buffer = fixed_buffer<mutable_buffer, ID_MAX_SZ>;
	using id_closure = std::function<void (const string_view &)>;
	using val_closure = std::function<void (const string_view &)>;
	using node_closure = std::function<void (const json::object &)>;
	using search_closure = std::function<bool (const json::array &, const string_view &, const uint &, const uint &)>;
	using iter_closure = std::function<void (const json::array &, const string_view &)>;
	using iter_bool_closure = std::function<bool (const json::array &, const string_view &)>;

	int keycmp(const json::array &a, const json::array &b);
	bool prefix_eq(const json::array &a, const json::array &b);
	json::array make_key(const mutable_buffer &out, const string_view &type, const string_view &state_key);
	json::array make_key(const mutable_buffer &out, const string_view &type);

	string_view set_node(db::txn &txn, const mutable_buffer &id, const json::object &node);
	void get_node(const string_view &id, const node_closure &);

	string_view insert(db::txn &, const mutable_buffer &rootout, const string_view &rootin, const json::array &key, const id::event &);
	string_view insert(db::txn &, const mutable_buffer &rootout, const string_view &rootin, const string_view &type, const string_view &state_key, const id::event &);
	string_view insert(db::txn &, const mutable_buffer &rootout, const string_view &rootin, const event &);

	bool dfs(const string_view &root, const json::array &key, const search_closure &);
	bool dfs(const string_view &root, const search_closure &);

	bool each(const string_view &root, const iter_bool_closure &);
	bool each(const string_view &root, const string_view &type, const iter_bool_closure &);
	size_t count(const string_view &root, const iter_bool_closure &);
	size_t count(const string_view &root);

	bool get(std::nothrow_t, const string_view &root, const json::array &key, const val_closure &);
	void get(const string_view &root, const json::array &key, const val_closure &);

	bool get(std::nothrow_t, const string_view &root, const string_view &type, const string_view &state_key, const val_closure &);
	void get(const string_view &root, const string_view &type, const string_view &state_key, const val_closure &);
}

/// JSON property name strings specifically for use in m::state
namespace ircd::m::state::name
{
	constexpr const char *const k {"k"};
	constexpr const char *const v {"v"};
	constexpr const char *const c {"c"};
}

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wsubobject-linkage"

/// Format for node: Node is plaintext and not binary at this time. In fact,
/// *evil chuckle*, node might as well be JSON and can easily become content
/// of another event sent to other rooms over network *snorts*. (important:
/// database is well compressed).
///
/// {                                                ;
///     "k":                                         ; Key array
///     [                                            ;
///         ["m.room.member", "@ar4an:matrix.org"],  ; Left key
///         ["m.room.member", "@jzk:matrix.org"]     ; Right key
///     ],                                           ;
///     "v":                                         ; Value array
///     [                                            ;
///         "$14961836116kXQRA:matrix.org",          ; Left accept
///         "$15018692261xPQDB:matrix.org",          ; Right accept
///     ]                                            ;
///     "c":                                         ; Child array
///     [                                            ;
///         "nPKN9twTF9a8k5dD7AApFcaraHTX",          ; Left child
///         "PcxAAACvkvyUMz19AZcCfrC3S84s",          ; Center child
///         "2jVYKIMKErJ6w6BLMhfVjsXearhB",          ; Right child
///     ]                                            ;
/// }                                                ;
///
/// Elements are ordered based on type+state_key lexical sort. The type and
/// the state_key strings are literally concatenated to this effect. They're
/// not hashed. We can have some more control over data locality this way. Any
/// number of values may be in a key array, not just type+state_key. The
/// concatenation involves the string with its surrounding quotes as to not
/// allow the user to mess about conflicting values:
/// ```
/// "m.room.member""@jzk" > "m.room.create"""
/// ```
/// Unlike traditional trees of such variety, the number of elements is not
/// really well defined and not even fixed. There just can be one more value
/// in the "child" list than there are keys in the "key" list. We have an
/// opportunity to vary the degree for different levels in different areas.
struct ircd::m::state::node
:json::tuple
<
	json::property<name::k, json::array>,
	json::property<name::v, json::array>,
	json::property<name::c, json::array>
>
{
	struct rep;

	size_t keys() const;
	size_t vals() const;
	size_t childs() const;

	json::array key(const size_t &) const;
	string_view val(const size_t &) const;
	string_view child(const size_t &) const;

	size_t keys(json::array *const &out, const size_t &max) const;
	size_t vals(string_view *const &out, const size_t &max) const;
	size_t childs(string_view *const &out, const size_t &max) const;

	size_t find(const json::array &key) const;
	bool has_key(const json::array &key) const;
	bool has_child(const size_t &) const;

	using super_type::tuple;
	using super_type::operator=;
};
#pragma GCC diagnostic pop

/// Internal representation of a node for manipulation purposes. This is
/// because json::tuple's (like most of json::) are oriented around the
/// dominant use-case of reading const datas. These arrays could be
/// vectors optimized with a local allocator but the size_t members are
/// used to count active elements instead. One more element than the node
/// maximum is provided so that insertions and sorts can safely take place
/// before splits.
struct ircd::m::state::node::rep
{
	std::array<json::array, NODE_MAX_KEY + 1> keys;
	std::array<string_view, NODE_MAX_VAL + 1> vals;
	std::array<string_view, NODE_MAX_DEG + 1> chld;
	size_t kn {0};
	size_t vn {0};
	size_t cn {0};

	bool full() const;
	bool overfull() const;
	bool duplicates() const;
	size_t childs() const;
	size_t find(const json::array &key) const;

	void shr(const size_t &pos);

	json::object write(const mutable_buffer &out);
	string_view write(db::txn &, const mutable_buffer &id);

	rep(const node &node);
	rep() = default;
};

static_assert
(
	ircd::m::state::NODE_MAX_KEY == ircd::m::state::NODE_MAX_VAL
);
