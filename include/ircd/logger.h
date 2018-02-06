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
#define HAVE_IRCD_LOGGER_H

// windows.h #define conflicts with our facility
#ifdef HAVE_WINDOWS_H
#undef ERROR
#endif

namespace ircd
{
	const char *smalldate(const time_t &);
}

/// Logging system
namespace ircd::log
{
	enum facility :int;
	const char *reflect(const facility &);

	struct log;
	struct console_quiet;

	void vlog(const facility &, const std::string &name, const char *const &fmt, const va_rtti &ap);
	void vlog(const facility &, const char *const &fmt, const va_rtti &ap);
	void mark(const facility &, const char *const &msg = nullptr);
	void mark(const char *const &msg = nullptr);

	struct critical;
	struct error;
	struct warning;
	struct notice;
	struct info;
	struct debug;

	void flush();
	void close();
	void open();

	void init();
	void fini();
}

enum ircd::log::facility
:int
{
	CRITICAL = 0,
	ERROR    = 1,
	WARNING  = 2,
	NOTICE   = 3,
	INFO     = 4,
	DEBUG    = 5,
	_NUM_
};

class ircd::log::log
{
	std::string name;

  public:
	template<class... args> void operator()(const facility &, const char *const &fmt, args&&...);

	template<class... args> void critical(const char *const &fmt, args&&...);
	template<class... args> void error(const char *const &fmt, args&&...);
	template<class... args> void warning(const char *const &fmt, args&&...);
	template<class... args> void notice(const char *const &fmt, args&&...);
	template<class... args> void info(const char *const &fmt, args&&...);
	template<class... args> void debug(const char *const &fmt, args&&...);

	log(const std::string &name, const char &snote);
	log(const std::string &name);
};

struct ircd::log::console_quiet
{
	console_quiet(const bool &showmsg = true);
	~console_quiet();
};

struct ircd::log::debug
{
	template<class... args>
	debug(const char *const &fmt, args&&... a)
	{
		// got DCE?
		#ifdef RB_DEBUG
		vlog(facility::DEBUG, fmt, va_rtti{std::forward<args>(a)...});
		#endif
	}
};

struct ircd::log::info
{
	template<class... args>
	info(const char *const &fmt, args&&... a)
	{
		vlog(facility::INFO, fmt, va_rtti{std::forward<args>(a)...});
	}
};

struct ircd::log::notice
{
	template<class... args>
	notice(const char *const &fmt, args&&... a)
	{
		vlog(facility::NOTICE, fmt, va_rtti{std::forward<args>(a)...});
	}
};

struct ircd::log::warning
{
	template<class... args>
	warning(const char *const &fmt, args&&... a)
	{
		vlog(facility::WARNING, fmt, va_rtti{std::forward<args>(a)...});
	}
};

struct ircd::log::error
{
	template<class... args>
	error(const char *const &fmt, args&&... a)
	{
		vlog(facility::ERROR, fmt, va_rtti{std::forward<args>(a)...});
	}
};

struct ircd::log::critical
{
	template<class... args>
	critical(const char *const &fmt, args&&... a)
	{
		vlog(facility::CRITICAL, fmt, va_rtti{std::forward<args>(a)...});
	}
};

template<class... args>
void
ircd::log::log::debug(const char *const &fmt,
                      args&&... a)
{
	#ifdef RB_DEBUG
	operator()(facility::DEBUG, fmt, va_rtti{std::forward<args>(a)...});
	#endif
}

template<class... args>
void
ircd::log::log::info(const char *const &fmt,
                     args&&... a)
{
	operator()(facility::INFO, fmt, va_rtti{std::forward<args>(a)...});
}

template<class... args>
void
ircd::log::log::notice(const char *const &fmt,
                       args&&... a)
{
	operator()(facility::NOTICE, fmt, va_rtti{std::forward<args>(a)...});
}

template<class... args>
void
ircd::log::log::warning(const char *const &fmt,
                        args&&... a)
{
	operator()(facility::WARNING, fmt, va_rtti{std::forward<args>(a)...});
}

template<class... args>
void
ircd::log::log::error(const char *const &fmt,
                      args&&... a)
{
	operator()(facility::ERROR, fmt, va_rtti{std::forward<args>(a)...});
}

template<class... args>
void
ircd::log::log::critical(const char *const &fmt,
                         args&&... a)
{
	operator()(facility::CRITICAL, fmt, va_rtti{std::forward<args>(a)...});
}

template<class... args>
void
ircd::log::log::operator()(const facility &f,
                           const char *const &fmt,
                           args&&... a)
{
	vlog(f, name, fmt, va_rtti{std::forward<args>(a)...});
}
