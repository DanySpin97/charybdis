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
#define HAVE_IRCD_FS_H

/*
 * Directory paths and filenames for UNIX systems.
 * IRCD_PREFIX is set using ./configure --prefix, see INSTALL.
 * Do not change these without corresponding changes in the build system.
 *
 * IRCD_PREFIX = prefix for all directories,
 * DPATH       = root directory of installation,
 * BINPATH     = directory for binary files,
 * ETCPATH     = directory for configuration files,
 * LOGPATH     = directory for logfiles,
 * MODPATH     = directory for modules,
 * AUTOMODPATH = directory for autoloaded modules
 */

/// Tools for working with the local filesystem.
///
/// IRCd has wrapped operations for the local filesystem to maintain a
/// cross-platform implementation of asynchronous file IO in conjunction with
/// the ircd::ctx userspace context system. These operations will yield your
/// ircd::ctx when necessary to not block the event loop on the main thread
/// during IOs.
namespace ircd::fs
{
	struct aio;
	struct init;

	IRCD_EXCEPTION(ircd::error, error)
	IRCD_EXCEPTION(error, filesystem_error)

	constexpr auto DPATH = IRCD_PREFIX;
	constexpr auto BINPATH = IRCD_PREFIX "/bin";
	constexpr auto ETCPATH = RB_ETC_DIR;
	constexpr auto LOGPATH = RB_LOG_DIR;
	constexpr auto MODPATH = RB_MODULE_DIR;
	constexpr auto CPATH = RB_ETC_DIR "/ircd.conf";              // ircd.conf file
	constexpr auto SPATH = RB_BIN_DIR "/" BRANDING_NAME;         // ircd executable
	constexpr auto DBPATH = PKGLOCALSTATEDIR "/db";              // database prefix

	// Below are the elements for default paths.
	enum index
	{
		PREFIX,
		BIN,
		ETC,
		LOG,
		LIBEXEC,
		MODULES,
		IRCD_CONF,
		IRCD_EXEC,
		DB,

		_NUM_
	};

	const char *get(index) noexcept;
	const char *name(index) noexcept;

	std::string make_path(const std::initializer_list<std::string> &);

	bool exists(const std::string &path);
	bool is_dir(const std::string &path);
	bool is_reg(const std::string &path);

	size_t size(const std::string &path);
	size_t size(const string_view &path);

	std::vector<std::string> ls(const std::string &path);
	std::vector<std::string> ls_recursive(const std::string &path);

	std::string cwd();
	void chdir(const std::string &path);
	bool mkdir(const std::string &path);

	// This suite of IO functions may yield your context.
	bool write(const std::string &name, const const_buffer &buf);
	bool append(const std::string &name, const const_buffer &buf);
	bool overwrite(const std::string &name, const const_buffer &buf);
	bool overwrite(const string_view &name, const const_buffer &buf);

	extern aio *aioctx;
}

#include "read.h"

struct ircd::fs::init
{
	init();
	~init() noexcept;
};
