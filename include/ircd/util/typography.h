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
#define HAVE_IRCD_UTIL_TYPOGRAPHY_H

namespace ircd {
namespace util {

//
// Overloading macros
//

/// Macro to arrange a function overload scheme based on the following
/// convention: An available `name` is chosen, from this name a strong type
/// is created by appending `_t`. The name itself becomes a static constexpr
/// instance of this `name_t`. Functions can be declared with an argument
/// accepting `name_t`, and called by passing `name`
///
/// IRCD_OVERLOAD(foo)                             // declare overload
/// void function(int, foo_t) {}                   // overloaded version
/// void function(int) { function(0, foo); }       // calls overloaded version
/// function(0);                                   // calls regular version
///
#define IRCD_OVERLOAD(NAME) \
    static constexpr struct NAME##_t {} NAME {};

/// Imports an overload scheme from elsewhere without redeclaring the type_t.
#define IRCD_USING_OVERLOAD(ALIAS, ORIGIN) \
    static constexpr const auto &ALIAS{ORIGIN}

//
// Typedef macros
//

/// Creates a type `NAME` from original type `TYPE` by inheriting from `TYPE`
/// and passing through construction to `TYPE`. These implicit conversions
/// we consider to be a "weak" typedef
#define IRCD_WEAK_TYPEDEF(TYPE, NAME)                       \
struct NAME                                                 \
:TYPE                                                       \
{                                                           \
    using TYPE::TYPE;                                       \
};

/// Creates a type `NAME` by wrapping instance of `TYPE` as a member and
/// providing explicit conversions to `TYPE` and aggregate construction only. We
/// consider this a "strong" typedef which is useful for wrapping POD types
/// for overloaded functions, etc.
#define IRCD_STRONG_TYPEDEF(TYPE, NAME)                       \
struct NAME                                                   \
{                                                             \
    TYPE val;                                                 \
                                                              \
    explicit operator const TYPE &() const  { return val;  }  \
    explicit operator TYPE &()              { return val;  }  \
};

/// Convenience for weak typedef statements
#define IRCD_WEAK_T(TYPE) \
    IRCD_WEAK_TYPEDEF(TYPE, IRCD_UNIQUE(weak_t))

/// Convenience for strong typedef statements
/// ex: using foo_t = IRCD_STRONG_T(int)
#define IRCD_STRONG_T(TYPE) \
    IRCD_STRONG_TYPEDEF(TYPE, IRCD_UNIQUE(strong_t))

//
// Debug sizeof structure at compile time
//

/// Output the sizeof a structure at compile time.
/// This stops the compiler with an error (good) containing the size of the target
/// in the message.
///
/// example: struct foo {}; IRCD_TEST_SIZEOF(foo)
///
#define IRCD_TEST_SIZEOF(name) \
	ircd::util::_TEST_SIZEOF_<sizeof(name)> _test_;

/// Internal use only
template<size_t SIZE>
struct _TEST_SIZEOF_;

//
// Test if type is forward declared or complete
//

template<class T,
         class = void>
struct is_complete
:std::false_type
{};

template<class T>
struct is_complete<T, decltype(void(sizeof(T)))>
:std::true_type
{};

//
// Test if type is a specialization of a template
//

template<class,
         template<class...>
                  class>
struct is_specialization_of
:std::false_type
{};

template<template<class...>
                  class T,
                  class... args>
struct is_specialization_of<T<args...>, T>
:std::true_type
{};

//
// Test if type is shared_from_this
//

/// Tests if type inherits from std::enable_shared_from_this<>
template<class T>
constexpr typename std::enable_if<is_complete<T>::value, bool>::type
is_shared_from_this()
{
	return std::is_base_of<std::enable_shared_from_this<T>, T>();
}

/// Unconditional failure for fwd-declared incomplete types, which
/// obviously don't inherit from std::enable_shared_from_this<>
template<class T>
constexpr typename std::enable_if<!is_complete<T>::value, bool>::type
is_shared_from_this()
{
	return false;
}

//
// Misc type testing boilerplates
//

template<class T>
constexpr bool
is_bool()
{
	using type = typename std::remove_reference<T>::type;
	return std::is_same<type, bool>::value;
}

template<class T>
constexpr bool
is_number()
{
	using type = typename std::remove_reference<T>::type;
	return std::is_arithmetic<type>::value;
}

template<class T>
constexpr bool
is_floating()
{
	using type = typename std::remove_reference<T>::type;
	return is_number<T>() && std::is_floating_point<type>();
}

template<class T>
constexpr bool
is_integer()
{
	return is_number<T>() && !is_floating<T>();
}

//
// Convenience constexprs for iterators
//

template<class It>
constexpr auto
is_iterator()
{
	return std::is_base_of<typename std::iterator_traits<It>::value_type, It>::value;
}

template<class It>
constexpr auto
is_forward_iterator()
{
	return std::is_base_of<std::forward_iterator_tag, typename std::iterator_traits<It>::iterator_category>::value;
}

template<class It>
constexpr auto
is_input_iterator()
{
	return std::is_base_of<std::forward_iterator_tag, typename std::iterator_traits<It>::iterator_category>::value;
}

/// Convenience loop to test std::is* on a character sequence
template<int (&test)(int) = std::isprint>
ssize_t
ctype(const char *begin,
      const char *const &end)
{
	size_t i(0);
	for(; begin != end; ++begin, ++i)
		if(!test(static_cast<unsigned char>(*begin)))
			return i;

	return -1;
}

/// ctype test for a string_view. Returns the character position where the
/// test fails. Returns -1 on success. The test is a function specified in
/// the template simply as `ctype<std::isprint>(string_view{"hi"});`
template<int (&test)(int)>
ssize_t
ctype(const string_view &s)
{
	return ctype<test>(begin(s), end(s));
}

/// convenience wrapper for ctype(string_view) to avoid the -1 test for
/// success in any intuitive boolean statement.
template<int (&test)(int)>
bool
valid(const string_view &s)
{
	return ctype<test>(s) == -1L;
}

/// Zero testing functor (work in progress)
///
struct is_zero
{
	template<class T>
	typename std::enable_if
	<
		is_bool<T>(),
	bool>::type
	test(const bool &value)
	const
	{
		return !value;
	}

	template<class T>
	typename std::enable_if
	<
		is_integer<T>() &&
		!is_bool<T>(),
	bool>::type
	test(const size_t &value)
	const
	{
		return value == 0;
	}

	template<class T>
	typename std::enable_if
	<
		is_floating<T>(),
	bool>::type
	test(const double &value)
	const
	{
		return !(value > 0.0 || value < 0.0);
	}

	template<class T>
	bool operator()(T&& t)
	const
	{
		return test<T>(std::forward<T>(t));
	}
};

} // namespace util
} // namespace ircd
