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
#define HAVE_IRCD_FS_WRITE_H

namespace ircd::fs
{
	struct write_opts extern const write_opts_default;

	// Yields ircd::ctx for write from buffer; returns view of written portion
	const_buffer write(const fd &, const const_buffer &, const write_opts & = write_opts_default);
	const_buffer write(const string_view &path, const const_buffer &, const write_opts & = write_opts_default);

	// Yields ircd::ctx to append to file from buffer; returns view of written portion
	const_buffer append(const fd &, const const_buffer &, const write_opts & = write_opts_default);
	const_buffer append(const string_view &path, const const_buffer &, const write_opts & = write_opts_default);

	// Yields ircd::ctx to overwrite (trunc) file from buffer; returns view of written portion
	const_buffer overwrite(const fd &, const const_buffer & = {}, const write_opts & = write_opts_default);
	const_buffer overwrite(const string_view &path, const const_buffer & = {}, const write_opts & = write_opts_default);

	// Truncate file to explicit size
	void truncate(const fd &, const size_t &, const write_opts & = write_opts_default);
	void truncate(const string_view &path, const size_t &, const write_opts & = write_opts_default);

	// Allocate
	void allocate(const fd &, const size_t &size, const write_opts & = write_opts_default);
}

/// Options for a write operation
struct ircd::fs::write_opts
{
	write_opts() = default;
	write_opts(const off_t &);

	/// Offset in the file to start the write from. For append() if this zero
	/// then it will be internally set to the end of the file; otherwise if
	/// this is set it will write to that offset, even for append(), unless
	/// the host system later ignores the offset due to the file's openmode.
	off_t offset {0};

	/// Request priority. Higher value request will take priority over lower
	/// value. Lowest value is zero. Negative value will receive a contextual
	/// value internally (generally just zero). Default is -1.
	int8_t priority {-1};

	/// for allocate()
	bool keep_size {false};
};

inline
ircd::fs::write_opts::write_opts(const off_t &offset)
:offset{offset}
{}
