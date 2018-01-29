/*
 *  charybdis: an advanced ircd.
 *  inline/stringops.h: inlined string operations used in a few places
 *
 *  Copyright (C) 2005-2016 Charybdis Development Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
 *  USA
 */

/// !!! NOTE !!!
///
/// Many functions implemented here need to be replaced with karma generators
/// similar to ircd::fmt. Both the boost and std lexical conversions are an
/// order of magnitude slower than the formal generators. Some tokenizations
/// can also be replaced.
///

#include <RB_INC_BOOST_TOKENIZER_HPP
#include <RB_INC_BOOST_LEXICAL_CAST_HPP

///////////////////////////////////////////////////////////////////////////////
//
// ircd/tokens.h
//

ircd::string_view
ircd::tokens_after(const string_view &str,
                   const char &sep,
                   const size_t &i)
{
	const char ssep[2] { sep, '\0' };
	return tokens_after(str, ssep, i);
}

ircd::string_view
ircd::tokens_after(const string_view &str,
                   const char *const &sep,
                   const size_t &i)
{
	using type = string_view;
	using iter = typename type::const_iterator;
	using delim = boost::char_separator<char>;

	const delim d(sep);
	const boost::tokenizer<delim, iter, type> view(str, d);

	auto it(begin(view));
	for(size_t j(0); it != end(view); ++it, j++)
		if(j > i)
			return string_view{it->data(), str.data() + str.size()};

	return {};
}

ircd::string_view
ircd::token_first(const string_view &str,
                  const char &sep)
{
	const char ssep[2] { sep, '\0' };
	return token(str, ssep, 0);
}

ircd::string_view
ircd::token_first(const string_view &str,
                  const char *const &sep)
{
	return token(str, sep, 0);
}

ircd::string_view
ircd::token_last(const string_view &str,
                 const char &sep)
{
	const char ssep[2] { sep, '\0' };
	return token_last(str, ssep);
}

ircd::string_view
ircd::token_last(const string_view &str,
                 const char *const &sep)
{
	using type = string_view;
	using iter = typename type::const_iterator;
	using delim = boost::char_separator<char>;

	const delim d(sep);
	const boost::tokenizer<delim, iter, type> view(str, d);

	auto it(begin(view));
	if(it == end(view))
		return str.empty()? str : throw std::out_of_range("token out of range");

	string_view ret(*it);
	for(++it; it != end(view); ++it)
		ret = *it;

	return ret;
}

ircd::string_view
ircd::token(const string_view &str,
            const char &sep,
            const size_t &i)
{
	const char ssep[2] { sep, '\0' };
	return token(str, ssep, i);
}

ircd::string_view
ircd::token(const string_view &str,
            const char *const &sep,
            const size_t &i)
{
	using type = string_view;
	using iter = typename type::const_iterator;
	using delim = boost::char_separator<char>;

	const delim d(sep);
	const boost::tokenizer<delim, iter, type> view(str, d);
	const auto it(at(begin(view), end(view), i));
	return *it;
}

size_t
ircd::token_count(const string_view &str,
                  const char &sep)
{
	const char ssep[2] { sep, '\0' };
	return token_count(str, ssep);
}

size_t
ircd::token_count(const string_view &str,
                  const char *const &sep)
{
	using type = string_view;
	using iter = typename type::const_iterator;
	using delim = boost::char_separator<char>;

	const delim d(sep);
	const boost::tokenizer<delim, iter, type> view(str, d);
	return std::distance(begin(view), end(view));
}

size_t
ircd::tokens(const string_view &str,
             const char &sep,
             const mutable_buffer &buf,
             const token_view &closure)
{
	const char ssep[2] { sep, '\0' };
	return tokens(str, ssep, buf, closure);
}

size_t
ircd::tokens(const string_view &str,
             const char *const &sep,
             const mutable_buffer &buf,
             const token_view &closure)
{
	char *ptr(data(buf));
	char *const stop(data(buf) + size(buf));
	tokens(str, sep, [&closure, &ptr, &stop]
	(const string_view &token)
	{
		const size_t terminated_size(token.size() + 1);
		const size_t remaining(std::distance(ptr, stop));
		if(remaining < terminated_size)
			return;

		char *const dest(ptr);
		ptr += strlcpy(dest, token.data(), terminated_size);
		closure(string_view(dest, token.size()));
	});

	return std::distance(data(buf), ptr);
}

size_t
ircd::tokens(const string_view &str,
             const char &sep,
             const size_t &limit,
             const token_view &closure)
{
	const char ssep[2] { sep, '\0' };
	return tokens(str, ssep, limit, closure);
}

size_t
ircd::tokens(const string_view &str,
             const char *const &sep,
             const size_t &limit,
             const token_view &closure)
{
	using type = string_view;
	using iter = typename type::const_iterator;
	using delim = boost::char_separator<char>;

	const delim d(sep);
	const boost::tokenizer<delim, iter, type> view(str, d);

	size_t i(0);
	for(auto it(begin(view)); i < limit && it != end(view); ++it, i++)
		closure(*it);

	return i;
}

void
ircd::tokens(const string_view &str,
             const char &sep,
             const token_view &closure)
{
	const char ssep[2] { sep, '\0' };
	tokens(str, ssep, closure);
}

void
ircd::tokens(const string_view &str,
             const char *const &sep,
             const token_view &closure)
{
	using type = string_view;
	using iter = typename type::const_iterator;
	using delim = boost::char_separator<char>;

	const delim d(sep);
	const boost::tokenizer<delim, iter, type> view(str, d);
	std::for_each(begin(view), end(view), closure);
}

///////////////////////////////////////////////////////////////////////////////
//
// ircd/lex_cast.h
//

namespace ircd
{
	/// The static lex_cast ring buffers are each LEX_CAST_BUFSIZE bytes;
	/// Consider increasing if some lex_cast<T>(str) has more characters.
	const size_t LEX_CAST_BUFSIZE {64};

	/// This is a static "ring buffer" to simplify a majority of lex_cast uses.
	/// If the lex_cast has binary input and string output, and no user buffer
	/// is supplied, the next buffer here will be used instead. The returned
	/// string_view of data from this buffer is only valid for several more
	/// calls to lex_cast before it is overwritten.
	thread_local char lex_cast_buf[LEX_CAST_BUFS][LEX_CAST_BUFSIZE];
	thread_local uint lex_cast_cur;

	template<size_t N, class T> static string_view _lex_cast(const T &i, mutable_buffer buf);
	template<class T> static T _lex_cast(const string_view &s);
}

/// Internal template providing conversions from a number to a string;
/// potentially using the ring buffer if no user buffer is supplied.
template<size_t N,
         class T>
ircd::string_view
ircd::_lex_cast(const T &i,
                mutable_buffer buf)
try
{
	using array = std::array<char, N>;

	if(!buf)
	{
		buf = lex_cast_buf[lex_cast_cur++];
		lex_cast_cur %= LEX_CAST_BUFS;
	}

	assert(size(buf) >= N);
	auto &a(*reinterpret_cast<array *>(data(buf)));
	a = boost::lexical_cast<array>(i);
	return { data(buf), strnlen(data(buf), size(buf)) };
}
catch(const boost::bad_lexical_cast &e)
{
	throw ircd::bad_lex_cast("%s", e.what());
}

/// Internal template providing conversions from a string to a number;
/// the native object is returned directly; no ring buffer is consumed.
template<class T>
T
ircd::_lex_cast(const string_view &s)
try
{
	return boost::lexical_cast<T>(s);
}
catch(const boost::bad_lexical_cast &e)
{
	throw ircd::bad_lex_cast("%s", e.what());
}

template<> ircd::string_view
ircd::lex_cast(bool i,
               const mutable_buffer &buf)
{
	static const size_t MAX(8);
	return _lex_cast<MAX>(i, buf);
}

template<> ircd::string_view
ircd::lex_cast(int8_t i,
               const mutable_buffer &buf)
{
	static const size_t MAX(8);
	return _lex_cast<MAX>(i, buf);
}

template<> ircd::string_view
ircd::lex_cast(uint8_t i,
               const mutable_buffer &buf)
{
	static const size_t MAX(8);
	return _lex_cast<MAX>(i, buf);
}

template<> ircd::string_view
ircd::lex_cast(short i,
               const mutable_buffer &buf)
{
	static const size_t MAX(8);
	return _lex_cast<MAX>(i, buf);
}

template<> ircd::string_view
ircd::lex_cast(ushort i,
               const mutable_buffer &buf)
{
	static const size_t MAX(8);
	return _lex_cast<MAX>(i, buf);
}

template<> ircd::string_view
ircd::lex_cast(int i,
               const mutable_buffer &buf)
{
	static const size_t MAX(16);
	return _lex_cast<MAX>(i, buf);
}

template<> ircd::string_view
ircd::lex_cast(uint i,
               const mutable_buffer &buf)
{
	static const size_t MAX(16);
	return _lex_cast<MAX>(i, buf);
}

template<> ircd::string_view
ircd::lex_cast(long i,
               const mutable_buffer &buf)
{
	static const size_t MAX(32);
	return _lex_cast<MAX>(i, buf);
}

template<> ircd::string_view
ircd::lex_cast(ulong i,
               const mutable_buffer &buf)
{
	static const size_t MAX(32);
	return _lex_cast<MAX>(i, buf);
}

template<> ircd::string_view
ircd::lex_cast(double i,
               const mutable_buffer &buf)
{
	static const size_t MAX(64);
	return _lex_cast<MAX>(i, buf);
}

template<> ircd::string_view
ircd::lex_cast(long double i,
               const mutable_buffer &buf)
{
	static const size_t MAX(64);
	return _lex_cast<MAX>(i, buf);
}

template<> ircd::string_view
ircd::lex_cast(nanoseconds i,
               const mutable_buffer &buf)
{
	static const size_t MAX(64);
	return _lex_cast<MAX>(i.count(), buf);
}

template<> ircd::string_view
ircd::lex_cast(microseconds i,
               const mutable_buffer &buf)
{
	static const size_t MAX(64);
	return _lex_cast<MAX>(i.count(), buf);
}

template<> ircd::string_view
ircd::lex_cast(milliseconds i,
               const mutable_buffer &buf)
{
	static const size_t MAX(64);
	return _lex_cast<MAX>(i.count(), buf);
}

template<> ircd::string_view
ircd::lex_cast(seconds i,
               const mutable_buffer &buf)
{
	static const size_t MAX(64);
	return _lex_cast<MAX>(i.count(), buf);
}

template<> bool
ircd::lex_cast(const string_view &s)
{
	return s == "true"? true:
	       s == "false"? false:
	       _lex_cast<bool>(s);
}

template<> int8_t
ircd::lex_cast(const string_view &s)
{
	return _lex_cast<char>(s);
}

template<> uint8_t
ircd::lex_cast(const string_view &s)
{
	return _lex_cast<unsigned char>(s);
}

template<> short
ircd::lex_cast(const string_view &s)
{
	return _lex_cast<short>(s);
}

template<> unsigned short
ircd::lex_cast(const string_view &s)
{
	return _lex_cast<unsigned short>(s);
}

template<> int
ircd::lex_cast(const string_view &s)
{
	return _lex_cast<int>(s);
}

template<> unsigned int
ircd::lex_cast(const string_view &s)
{
	return _lex_cast<unsigned int>(s);
}

template<> long
ircd::lex_cast(const string_view &s)
{
	return _lex_cast<long>(s);
}

template<> unsigned long
ircd::lex_cast(const string_view &s)
{
	return _lex_cast<unsigned long>(s);
}

template<> double
ircd::lex_cast(const string_view &s)
{
	return _lex_cast<double>(s);
}

template<> long double
ircd::lex_cast(const string_view &s)
{
	return _lex_cast<long double>(s);
}

template<> ircd::nanoseconds
ircd::lex_cast(const string_view &s)
{
	return std::chrono::duration<time_t, std::ratio<1L, 1000000000L>>(_lex_cast<time_t>(s));
}

template<> ircd::microseconds
ircd::lex_cast(const string_view &s)
{
	return std::chrono::duration<time_t, std::ratio<1L, 1000000L>>(_lex_cast<time_t>(s));
}

template<> ircd::milliseconds
ircd::lex_cast(const string_view &s)
{
	return std::chrono::duration<time_t, std::ratio<1L, 1000L>>(_lex_cast<time_t>(s));
}

template<> ircd::seconds
ircd::lex_cast(const string_view &s)
{
	return std::chrono::duration<time_t, std::ratio<1L, 1L>>(_lex_cast<time_t>(s));
}

template<> bool
ircd::try_lex_cast<bool>(const string_view &s)
{
	bool i;
	return boost::conversion::try_lexical_convert(s, i);
}

template<> bool
ircd::try_lex_cast<int8_t>(const string_view &s)
{
	int8_t i;
	return boost::conversion::try_lexical_convert(s, i);
}

template<> bool
ircd::try_lex_cast<uint8_t>(const string_view &s)
{
	uint8_t i;
	return boost::conversion::try_lexical_convert(s, i);
}

template<> bool
ircd::try_lex_cast<short>(const string_view &s)
{
	short i;
	return boost::conversion::try_lexical_convert(s, i);
}

template<> bool
ircd::try_lex_cast<ushort>(const string_view &s)
{
	ushort i;
	return boost::conversion::try_lexical_convert(s, i);
}

template<> bool
ircd::try_lex_cast<int>(const string_view &s)
{
	int i;
	return boost::conversion::try_lexical_convert(s, i);
}

template<> bool
ircd::try_lex_cast<unsigned int>(const string_view &s)
{
	unsigned int i;
	return boost::conversion::try_lexical_convert(s, i);
}

template<> bool
ircd::try_lex_cast<long>(const string_view &s)
{
	long i;
	return boost::conversion::try_lexical_convert(s, i);
}

template<> bool
ircd::try_lex_cast<unsigned long>(const string_view &s)
{
	unsigned long i;
	return boost::conversion::try_lexical_convert(s, i);
}

template<> bool
ircd::try_lex_cast<double>(const string_view &s)
{
	double i;
	return boost::conversion::try_lexical_convert(s, i);
}

template<> bool
ircd::try_lex_cast<long double>(const string_view &s)
{
	long double i;
	return boost::conversion::try_lexical_convert(s, i);
}

template<> bool
ircd::try_lex_cast<ircd::nanoseconds>(const string_view &s)
{
	time_t i; //TODO: XXX
	return boost::conversion::try_lexical_convert(s, i);
}

template<> bool
ircd::try_lex_cast<ircd::microseconds>(const string_view &s)
{
	time_t i; //TODO: XXX
	return boost::conversion::try_lexical_convert(s, i);
}

template<> bool
ircd::try_lex_cast<ircd::milliseconds>(const string_view &s)
{
	time_t i; //TODO: XXX
	return boost::conversion::try_lexical_convert(s, i);
}

template<> bool
ircd::try_lex_cast<ircd::seconds>(const string_view &s)
{
	time_t i; //TODO: XXX
	return boost::conversion::try_lexical_convert(s, i);
}

///////////////////////////////////////////////////////////////////////////////
//
// ircd/stringops.h
//

std::string
ircd::replace(const string_view &s,
              const char &before,
              const string_view &after)
{
	const auto occurs
	{
		std::count(begin(s), end(s), before)
	};

	const size_t size
	{
		occurs? s.size() + (occurs * after.size()):
		        s.size() - occurs
	};

	return string(size, [&s, &before, &after]
	(const mutable_buffer &buf)
	{
		char *p{begin(buf)};
		std::for_each(begin(s), end(s), [&before, &after, &p]
		(const char &c)
		{
			if(c == before)
			{
				memcpy(p, after.data(), after.size());
				p += after.size();
			}
			else *p++ = c;
		});

		return std::distance(begin(buf), p);
	});
}

std::string
ircd::u2a(const const_raw_buffer &in)
{
	return string(size(in) * 2, [&in]
	(const mutable_buffer &out)
	{
		return u2a(out, in);
	});
}

ircd::string_view
ircd::u2a(const mutable_buffer &out,
          const const_raw_buffer &in)
{
	char *p(data(out));
	for(size_t i(0); i < size(in); ++i)
		p += snprintf(p, size(out) - (p - data(out)), "%02x", in[i]);

	return { data(out), size_t(p - data(out)) };
}

ircd::const_raw_buffer
ircd::a2u(const mutable_raw_buffer &out,
          const const_buffer &in)
{
	const size_t len{size(in) / 2};
	for(size_t i(0); i < len; ++i)
	{
		const char gl[3]
		{
			in[i * 2],
			in[i * 2 + 1],
			'\0'
		};

		out[i] = strtol(gl, nullptr, 16);
	}

	return { data(out), len };
}
