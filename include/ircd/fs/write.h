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
	string_view write(const string_view &path, const const_buffer &, const write_opts & = write_opts_default);

	// Yields ircd::ctx to overwrite (trunc) file from buffer; returns view of written portion
	string_view overwrite(const string_view &path, const const_buffer &, const write_opts & = write_opts_default);

	// Yields ircd::ctx to append to file from buffer; returns view of written portion
	string_view append(const string_view &path, const const_buffer &, const write_opts & = write_opts_default);
}

/// Options for a write operation
struct ircd::fs::write_opts
{
	write_opts() = default;
	write_opts(const off_t &);

	/// Overwrite (trunc) the file
	bool overwrite {false};

	/// Append to the file (offset is ignored)
	bool append {false};

	/// Create the file if necessary (default)
	bool create {true};

	/// Offset in the file to start the write from.
	off_t offset {0};

	/// Request priority (this option may be improved, avoid for now)
	int16_t priority {0};
};

inline
ircd::fs::write_opts::write_opts(const off_t &offset)
:offset{offset}
{}

inline ircd::string_view
ircd::fs::overwrite(const string_view &path,
                    const const_buffer &buf,
                    const write_opts &opts_)
{
	auto opts(opts_);
	opts.overwrite = true;
	return write(path, buf, opts);
}

inline ircd::string_view
ircd::fs::append(const string_view &path,
                 const const_buffer &buf,
                 const write_opts &opts_)
{
	auto opts(opts_);
	opts.append = true;
	return write(path, buf, opts);
}
