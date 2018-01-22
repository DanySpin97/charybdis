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
#define HAVE_IRCD_BASE_H

namespace ircd
{
	// Binary -> Base58 encode suite
	constexpr size_t b58encode_size(const size_t &);
	size_t b58encode_size(const const_raw_buffer &in);
	string_view b58encode(const mutable_buffer &out, const const_raw_buffer &in);
	std::string b58encode(const const_raw_buffer &in);

	// Base58 -> Binary decode suite
	constexpr size_t b58decode_size(const size_t &);
	size_t b58decode_size(const string_view &in);
	const_raw_buffer b58decode(const mutable_raw_buffer &out, const string_view &in);
	std::string b58decode(const string_view &in);

	// Binary -> Base64 conversion suite
	string_view b64encode(const mutable_buffer &out, const const_raw_buffer &in);
	std::string b64encode(const const_raw_buffer &in);

	// Binary -> Base64 conversion without padding
	string_view b64encode_unpadded(const mutable_buffer &out, const const_raw_buffer &in);
	std::string b64encode_unpadded(const const_raw_buffer &in);

	// Base64 -> Binary conversion (padded or unpadded)
	const_raw_buffer b64decode(const mutable_raw_buffer &out, const string_view &in);
	std::string b64decode(const string_view &in);
}

inline size_t
ircd::b58decode_size(const string_view &in)
{
	return b58decode_size(size(in));
}

constexpr size_t
ircd::b58decode_size(const size_t &in)
{
	return in * 733UL / 1000UL + 1UL; // log(58) / log(256), rounded up
}

inline size_t
ircd::b58encode_size(const const_raw_buffer &in)
{
	return b58encode_size(size(in));
}

constexpr size_t
ircd::b58encode_size(const size_t &in)
{
	return in * 138UL / 100UL + 1UL; // log(256) / log(58), rounded up
}
