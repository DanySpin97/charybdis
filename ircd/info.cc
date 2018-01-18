//
// Matrix Construct
//
// Copyright (C) Matrix Construct Developers, Authors & Contributors
// Copyright (C) 2016-2018 Jason Volk <jason@zemos.net>
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

#include <ircd/asio.h>

void
ircd::info::init()
{
	// This message flashes information about IRCd itself for this execution.
	log::info("%s %ld %s. configured: %s; compiled: %s; executed: %s; %s",
	          BRANDING_VERSION,
	          __cplusplus,
	          __VERSION__,
	          configured,
	          compiled,
	          startup,
	          RB_DEBUG_LEVEL? "(DEBUG MODE)" : "");

	// This message flashes information about our dependencies which are being
	// assumed for this execution.
	log::info("%s. boost %u.%u.%u. rocksdb %s. sodium %s. %s.",
	          PACKAGE_STRING,
	          boost_version[0],
	          boost_version[1],
	          boost_version[2],
	          db::version,
	          nacl::version(),
	          openssl::version());

	// This message flashes information about our dependencies which are being
	// assumed for this execution.
	log::debug("max_align=%zu hardware_concurrency=%zu destructive_interference=%zu constructive_interference=%zu",
	           max_align,
	           hardware_concurrency,
	           destructive_interference,
	           constructive_interference);
}

/* XXX: integrate CREDITS text again somehow */
decltype(ircd::info::credits)
ircd::info::credits
{{

	"Inspired by the original Internet Relay Chat daemon from Jarkko Oikarinen",
	" ",
	"This - is The Construct",
	" ",
	"Internet Relay Chat daemon: Matrix Construct",
	" ",
	"Copyright (C) 2016-2018 Matrix Construct Developers, Authors & Contributors",
	"Permission to use, copy, modify, and/or distribute this software for any",
	"purpose with or without fee is hereby granted, provided that the above",
	"copyright notice and this permission notice is present in all copies.",
	" ",
}};

decltype(ircd::info::configured_time)
ircd::info::configured_time
{
	RB_TIME_CONFIGURED
};

decltype(ircd::info::compiled_time)
ircd::info::compiled_time
{
	RB_TIME_COMPILED
};

decltype(ircd::info::startup_time)
ircd::info::startup_time
{
	std::time(nullptr)
};

decltype(ircd::info::configured)
ircd::info::configured
{
	ctime(&configured_time)
};

decltype(ircd::info::compiled)
ircd::info::compiled
{
	//ctime(&compiled_time)
	__TIMESTAMP__
};

decltype(ircd::info::startup)
ircd::info::startup
{
	ctime(&startup_time)
};

decltype(ircd::info::serno)
ircd::info::serno
{
	//TODO: XXX: compile counter?
	// RB_SERNO
	0
};

decltype(ircd::info::version)
ircd::info::version
{
	RB_VERSION
};

decltype(ircd::info::ircd_version)
ircd::info::ircd_version
{
	RB_VERSION
};

/// Boost version indicator for compiled header files.
decltype(ircd::info::boost_version)
ircd::info::boost_version
{
	BOOST_VERSION / 100000,
	BOOST_VERSION / 100 % 1000,
	BOOST_VERSION % 100,
};

/// Provides tcmalloc version information if tcmalloc is linked in to IRCd.
struct ircd::info::tc_version
{
	int major{0}, minor{0};
	char patch[64] {0};
	std::string version {"unavailable"};
}
const ircd::info::tc_version;

/*
const char* tc_version(int* major, int* minor, const char** patch);
ircd::tc_version::tc_version()
:version{::tc_version(&major, &minor, reinterpret_cast<const char **>(&patch))}
{}
*/

decltype(ircd::info::myinfo)
ircd::info::myinfo
{{
	#ifdef CPATH
	{"CPATH", CPATH, 0, "Path to Main Configuration File"},
	#else
	{"CPATH", "NONE", 0, "Path to Main Configuration File"},
	#endif

	#ifdef DPATH
	{"DPATH", DPATH, 0, "Directory Containing Configuration Files"},
	#else
	{"DPATH", "NONE", 0, "Directory Containing Configuration Files"},
	#endif

	#ifdef HPATH
	{"HPATH", HPATH, 0, "Path to Operator Help Files"},
	#else
	{"HPATH", "NONE", 0, "Path to Operator Help Files"},
	#endif

	#ifdef UHPATH
	{"UHPATH", UHPATH, 0, "Path to User Help Files"},
	#else
	{"UHPATH", "NONE", 0, "Path to User Help Files"},
	#endif

	#ifdef RB_IPV6
	{"IPV6", "ON", 0, "IPv6 Support"},
	#else
	{"IPV6", "OFF", 0, "IPv6 Support"},
	#endif

	#ifdef JOIN_LEAVE_COUNT_EXPIRE_TIME
	{"JOIN_LEAVE_COUNT_EXPIRE_TIME", "", JOIN_LEAVE_COUNT_EXPIRE_TIME, "Anti SpamBot Parameter"},
	#endif

	#ifdef KILLCHASETIMELIMIT
	{"KILLCHASETIMELIMIT", "", KILLCHASETIMELIMIT, "Nick Change Tracker for KILL"},
	#endif

	#ifdef LPATH
	{"LPATH", LPATH, 0, "Path to Log File"},
	#else
	{"LPATH", "NONE", 0, "Path to Log File"},
	#endif

	#ifdef MAX_BUFFER
	{"MAX_BUFFER", "", MAX_BUFFER, "Maximum Buffer Connections Allowed"},
	#endif

	#ifdef MAX_JOIN_LEAVE_COUNT
	{"MAX_JOIN_LEAVE_COUNT", "", MAX_JOIN_LEAVE_COUNT, "Anti SpamBot Parameter"},
	#endif

	#ifdef MAX_JOIN_LEAVE_TIME
	{"MIN_JOIN_LEAVE_TIME", "", MIN_JOIN_LEAVE_TIME, "Anti SpamBot Parameter"},
	#endif

	#ifdef MPATH
	{"MPATH", MPATH, 0, "Path to MOTD File"},
	#else
	{"MPATH", "NONE", 0, "Path to MOTD File"},
	#endif

	#ifdef NICKNAMEHISTORYLENGTH
	{"NICKNAMEHISTORYLENGTH", "", NICKNAMEHISTORYLENGTH, "Size of WHOWAS Array"},
	#endif

	#ifdef OPATH
	{"OPATH", OPATH, 0, "Path to Operator MOTD File"},
	#else
	{"OPATH", "NONE", 0, "Path to Operator MOTD File"},
	#endif

	#ifdef OPER_SPAM_COUNTDOWN
	{"OPER_SPAM_COUNTDOWN", "", OPER_SPAM_COUNTDOWN, "Anti SpamBot Parameter"},
	#endif

	#ifdef HAVE_LIBCRYPTO
	{"HAVE_LIBCRYPTO", "ON", 0, "Enable OpenSSL CHALLENGE Support"},
	#else
	{"HAVE_LIBCRYPTO", "OFF", 0, "Enable OpenSSL CHALLENGE Support"},
	#endif

	#ifdef HAVE_LIBZ
	{"HAVE_LIBZ", "YES", 0, "zlib (ziplinks) support"},
	#else
	{"HAVE_LIBZ", "NO", 0, "zlib (ziplinks)  support"},
	#endif

	#ifdef PPATH
	{"PPATH", PPATH, 0, "Path to Pid File"},
	#else
	{"PPATH", "NONE", 0, "Path to Pid File"},
	#endif

	#ifdef SPATH
	{"SPATH", SPATH, 0, "Path to Server Executable"},
	#else
	{"SPATH", "NONE", 0, "Path to Server Executable"},
	#endif

	#ifdef TS_MAX_DELTA_DEFAULT
	{"TS_MAX_DELTA_DEFAULT", "", TS_MAX_DELTA_DEFAULT, "Maximum Allowed TS Delta from another Server"},
	#endif

	#ifdef TS_WARN_DELTA_DEFAULT
	{"TS_WARN_DELTA_DEFAULT", "", TS_WARN_DELTA_DEFAULT, "Maximum TS Delta before Sending Warning"},
	#endif

	#ifdef USE_IODEBUG_HOOKS
	{"USE_IODEBUG_HOOKS", "YES", 0, "IO Debugging support"},
	#else
	{"USE_IODEBUG_HOOKS", "NO", 0, "IO Debugging support"},
	#endif
}};

decltype(ircd::info::max_align)
ircd::info::max_align
{
	alignof(std::max_align_t)
};

decltype(ircd::info::hardware_concurrency)
ircd::info::hardware_concurrency
{
	std::thread::hardware_concurrency()
};

decltype(ircd::info::destructive_interference)
ircd::info::destructive_interference
{
	#ifdef __cpp_lib_hardware_interference_size
		std::hardware_destructive_interference_size
	#else
		0
	#endif
};

decltype(ircd::info::constructive_interference)
ircd::info::constructive_interference
{
	#ifdef __cpp_lib_hardware_interference_size
		std::hardware_constructive_interference_size
	#else
		0
	#endif
};
