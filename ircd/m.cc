// Matrix Construct
//
// Copyright (C) Matrix Construct Developers, Authors & Contributors
// Copyright (C) 2016-2018 Jason Volk <jason@zemos.net>
//
// Permission to use, copy, modify, and/or distribute this software for any
// purpose with or without fee is hereby granted, provided that the above
// copyright notice and this permission notice is present in all copies. The
// full license for this software is available in the LICENSE file.

decltype(ircd::m::log)
ircd::m::log
{
	"m", 'm'
};

//
// init
//

ircd::conf::item<std::string>
me_online_status_msg
{
	{ "name",     "ircd.me.online.status_msg"          },
	{ "default",  "Wanna chat? IRCd at your service!"  }
};

ircd::conf::item<std::string>
me_offline_status_msg
{
	{ "name",     "ircd.me.offline.status_msg"     },
	{ "default",  "Catch ya on the flip side..."   }
};

//
// init::init
//

ircd::m::init::init(const string_view &origin,
                    const string_view &servername)
try
:_self
{
	origin, servername
}
,_dbs
{
	self::servername, std::string{}
}
,_modules
{
	std::make_unique<modules>()
}
{
	if(!ircd::write_avoid && vm::sequence::retired != 0)
		presence::set(me, "online", me_online_status_msg);
}
catch(const m::error &e)
{
	log::error
	{
		log, "Failed to start matrix (%u) %s :%s :%s",
		uint(e.code),
		http::status(e.code),
		e.errcode(),
		e.errstr(),
	};

	throw;
}
catch(const std::exception &e)
{
	log::error
	{
		log, "Failed to start matrix :%s", e.what()
	};

	throw;
}

ircd::m::init::~init()
noexcept try
{
	if(m::sync::pool.size())
		m::sync::pool.join();

	if(!std::uncaught_exceptions() && !ircd::write_avoid)
		presence::set(me, "offline", me_offline_status_msg);
}
catch(const m::error &e)
{
	log::critical
	{
		log, "%s %s", e.what(), e.content
	};

	ircd::terminate();
}

void
ircd::m::init::close()
{
	mods::imports.erase("m_listen"s);
}

//
// init::modules
//

namespace ircd::m
{
	extern const std::vector<string_view> module_names;
	extern const std::vector<string_view> module_names_optional;
}

ircd::m::init::modules::modules()
{
	const unwind::exceptional unload{[this]
	{
		this->fini_imports();
	}};

	init_imports();
}

ircd::m::init::modules::~modules()
noexcept
{
	fini_imports();
}

void
ircd::m::init::modules::init_imports()
{
	if(!bool(ircd::mods::autoload))
	{
		log::warning
		{
			"Not loading modules because noautomod flag is set. "
			"You may still load modules manually."
		};

		return;
	}

	for(const auto &name : module_names) try
	{
		mods::imports.emplace(name, name);
	}
	catch(...)
	{
		const auto &optional(module_names_optional);
		if(std::count(begin(optional), end(optional), name))
			continue;

		throw;
	}

	if(vm::sequence::retired == 0)
	{
		log::notice
		{
			log, "This appears to be your first time running IRCd because the events "
			"database is empty. I will be bootstrapping it with initial events now..."
		};

		const module m_init_bootstrap
		{
			"m_init_bootstrap"
		};

		const mods::import<void ()> bootstrap
		{
			m_init_bootstrap, "ircd::m::init::bootstrap"
		};

		bootstrap();
	}
}

void
ircd::m::init::modules::fini_imports()
noexcept
{
	// Stop the vm (unload) this first even though it loads first.
	mods::imports.erase("m_vm");

	for(auto it(module_names.rbegin()); it != module_names.rend(); ++it)
		mods::imports.erase(*it);
}

/// This is an ordered list for loading and unloading modules. This is not the
/// solution I really want at all so consider it temporary. Modules are loaded
/// in the order of the lines and unloaded in reverse order.
decltype(ircd::m::module_names)
ircd::m::module_names
{
	"m_noop",
	"m_node",
	"m_keys",
	"m_event",
	"m_fetch",
	"m_feds",
	"m_user",
	"m_user_filter",
	"m_user_rooms",
	"m_user_mitsein",
	"m_user_servers",
	"m_user_events",
	"m_user_highlight",
	"m_user_profile",
	"m_user_account_data",
	"m_user_room_account_data",
	"m_user_room_tags",
	"m_room",
	"m_room_auth",
	"m_room_head",
	"m_room_timeline",
	"m_room_aliases",
	"m_room_canonical_alias",
	"m_room_create",
	"m_room_history_visibility",
	"m_room_join_rules",
	"m_room_member",
	"m_room_name",
	"m_room_third_party_invite",
	"m_room_message",
	"m_room_power_levels",
	"m_room_server_acl",
	"m_room_redaction",
	"m_room_bootstrap",
	"m_room_join",
	"m_room_leave",
	"m_events",
	"m_rooms",
	"m_rooms_summary",
	"m_users",
	"m_presence",
	"m_receipt",
	"m_typing",
	"m_device_list_update",
	"m_device",
	"m_direct",
	"m_direct_to_device",
	"m_breadcrumb_rooms",
	"m_ignored_user_list",
	"m_event_append",
	"m_command",
	"m_control",
	"key_query",
	"key_server",
	"identity_pubkey",
	"identity_v1",
	"media_media",
	"conf",
	"net_dns",
	"federation_backfill_ids",
	"federation_backfill",
	"federation_event_auth",
	"federation_event",
	"federation_get_groups_publicised",
	"federation_get_missing_events",
	"federation_invite",
	"federation_invite2",
	"federation_make_join",
	"federation_make_leave",
	"federation_publicrooms",
	"federation_query_auth",
	"federation_query",
	"federation_sender",
	"federation_send_join",
	"federation_send_leave",
	"federation_send",
	"federation_state_ids",
	"federation_state",
	"federation_user_devices",
	"federation_user_keys_claim",
	"federation_user_keys_query",
	"federation_version",
	"client_user",
	"client_rooms",
	"client_createroom",
	"client_join",
	"client_account",
	"client_profile",
	"client_notifications",
	"client_devices",
	"client_delete_devices",
	"client_send_to_device",
	"client_keys_query",
	"client_keys_claim",
	"client_keys_changes",
	"client_keys_upload",
	"client_presence",
	"client_joined_groups",
	"client_publicised_groups",
	"client_login",
	"client_logout",
	"client_register_available",
	"client_register_email",
	"client_register",
	"client_directory_list_appservice",
	"client_directory_list_room",
	"client_directory_room",
	"client_directory_user",
	"client_publicrooms",
	"client_search",
	"client_pushers",
	"client_pushrules",
	"client_events",
	"client_initialsync",
	"client_sync",
	"client_sync_account_data",
	"client_sync_device_lists",
	"client_sync_device_one_time_keys_count",
	"client_sync_presence",
	"client_sync_to_device",
	"client_sync_rooms_account_data",
	"client_sync_rooms_ephemeral_receipt",
	"client_sync_rooms_ephemeral",
	"client_sync_rooms_ephemeral_typing",
	"client_sync_rooms",
	"client_sync_rooms_state",
	"client_sync_rooms_timeline",
	"client_sync_rooms_unread_notifications",
	"client_sync_rooms_summary",
	"client_voip_turnserver",
	"client_thirdparty_protocols",
	"client_versions",
	"client_capabilities",
	"well_known",
	"web_root",
	"web_hook",
	"m_listen",
	"m_vm",
	"m_init_backfill",
	"stats",
};

/// This is a list of modules that are considered "optional" and any loading
/// error for them will not propagate and interrupt m::init.
decltype(ircd::m::module_names_optional)
ircd::m::module_names_optional
{
	"web_hook",
};

///////////////////////////////////////////////////////////////////////////////
//
// m/self.h
//

std::string
ircd::m::self::origin
{};

std::string
ircd::m::self::servername
{};

ircd::ed25519::sk
ircd::m::self::secret_key
{};

ircd::ed25519::pk
ircd::m::self::public_key
{};

std::string
ircd::m::self::public_key_b64
{};

std::string
ircd::m::self::public_key_id
{};

std::string
ircd::m::self::tls_cert_der
{};

std::string
ircd::m::self::tls_cert_der_sha256_b64
{};

//
// my user
//

ircd::m::user::id::buf
ircd_user_id
{
	"ircd", "localhost"  // gets replaced after conf init
};

ircd::m::user
ircd::m::me
{
	ircd_user_id
};

//
// my room
//

ircd::m::room::id::buf
ircd_room_id
{
	"ircd", "localhost" // replaced after conf init
};

ircd::m::room
ircd::m::my_room
{
	ircd_room_id
};

//
// my node
//

std::array<char, ircd::rfc3986::DOMAIN_BUFSIZE>
ircd_node_id
{
	"localhost"  // replaced after conf init
};

ircd::m::node
ircd::m::my_node
{
	ircd_node_id
};

bool
ircd::m::self::host(const string_view &other)
{
	// port() is 0 when the origin has no port (and implies 8448)
	const auto port
	{
		me.user_id.port()
	};

	// If my_host has a port number, then the argument must also have the
	// same port number.
	if(port)
		return host() == other;

	/// If my_host has no port number, then the argument can have port
	/// 8448 or no port number, which will initialize net::hostport.port to
	/// the "canon_port" of 8448.
	assert(net::canon_port == 8448);
	const net::hostport _other{other};
	if(net::port(_other) != net::canon_port)
		return false;

	if(host() != host(_other))
		return false;

	return true;
}

ircd::string_view
ircd::m::self::host()
{
	return me.user_id.host();
}

//
// init
//

//TODO: XXX
extern ircd::m::room::id::buf users_room_id;
extern ircd::m::room::id::buf tokens_room_id;

ircd::m::self::init::init(const string_view &origin,
                          const string_view &servername)
{
	// Sanity check that these are valid hostname strings. This was likely
	// already checked, so these validators will simply throw without very
	// useful error messages if invalid strings ever make it this far.
	rfc3986::valid_host(origin);
	rfc3986::valid_host(servername);

	self::origin = origin;
	self::servername = servername;

	m::my_node = string_view{strlcpy
	{
		ircd_node_id, origin
	}};

	ircd_user_id = {"ircd", origin};
	m::me = {ircd_user_id};

	ircd_room_id = {"ircd", origin};
	m::my_room = {ircd_room_id};

	tokens_room_id = {"tokens", origin};
	m::user::tokens = {tokens_room_id};

	if(origin == "localhost")
		log::warning
		{
			"The origin is configured or has defaulted to 'localhost'"
		};

	// Loading the keys module in runlevel::START will do further
	// inits of m::self::globals. Calling the inits directly from
	// here makes the module dependent on libircd and unloadable.
	assert(ircd::run::level == run::level::START);
	mods::imports.emplace("m_keys"s, "m_keys"s);
}

///////////////////////////////////////////////////////////////////////////////
//
// m/sync.h
//

decltype(ircd::m::sync::log)
ircd::m::sync::log
{
	"m.sync", 's'
};

namespace ircd::m::sync
{
	const ctx::pool::opts pool_opts
	{
		ctx::DEFAULT_STACK_SIZE,
		0,
		-1,
		0
	};
}

decltype(ircd::m::sync::pool)
ircd::m::sync::pool
{
	"m.sync", pool_opts
};

decltype(ircd::m::sync::stats_info)
ircd::m::sync::stats_info
{
	{ "name",     "ircd.m.sync.stats.info" },
	{ "default",  false                    },
};

template<>
decltype(ircd::util::instance_multimap<std::string, ircd::m::sync::item, std::less<>>::map)
ircd::util::instance_multimap<std::string, ircd::m::sync::item, std::less<>>::map
{};

template<>
decltype(ircd::util::instance_list<ircd::m::sync::data>::allocator)
ircd::util::instance_list<ircd::m::sync::data>::allocator
{};

template<>
decltype(ircd::util::instance_list<ircd::m::sync::data>::list)
ircd::util::instance_list<ircd::m::sync::data>::list
{
	allocator
};

bool
ircd::m::sync::for_each(const item_closure_bool &closure)
{
	auto it(begin(item::map));
	for(; it != end(item::map); ++it)
		if(!closure(*it->second))
			return false;

	return true;
}

bool
ircd::m::sync::for_each(const string_view &prefix,
                        const item_closure_bool &closure)
{
	const auto depth
	{
		token_count(prefix, '.')
	};

	auto it
	{
		item::map.lower_bound(prefix)
	};

	for(; it != end(item::map); ++it)
	{
		const auto item_depth
		{
			token_count(it->first, '.')
		};

		if(item_depth > depth + 1)
			continue;

		if(it->first == prefix)
			continue;

		if(item_depth < depth + 1)
			break;

		if(!closure(*it->second))
			return false;
	}

	return true;
}

bool
ircd::m::sync::apropos(const data &d,
                       const event &event)
{
	return apropos(d, index(event, std::nothrow));
}

bool
ircd::m::sync::apropos(const data &d,
                       const event::id &event_id)
{
	return apropos(d, index(event_id, std::nothrow));
}

bool
ircd::m::sync::apropos(const data &d,
                       const event::idx &event_idx)
{
	return d.phased ||
	       (event_idx >= d.range.first && event_idx < d.range.second);
}

ircd::string_view
ircd::m::sync::loghead(const data &data)
{
	thread_local char headbuf[256], rembuf[128], iecbuf[2][64], tmbuf[32];

	const auto remstr
	{
		data.client?
			string(rembuf, ircd::remote(*data.client)):
			string_view{}
	};

	const auto flush_bytes
	{
		data.stats?
			data.stats->flush_bytes:
			0U
	};

	const auto flush_count
	{
		data.stats?
			data.stats->flush_count:
			0U
	};

	const auto tmstr
	{
		data.stats?
			ircd::pretty(tmbuf, data.stats->timer.at<milliseconds>(), true):
			string_view{}
	};

	return fmt::sprintf
	{
		headbuf, "%s %s %ld:%lu|%lu%s chunk:%zu sent:%s of %s in %s",
		remstr,
		string_view{data.user.user_id},
		data.range.first,
		data.range.second,
		vm::sequence::retired,
		data.phased?
			"|P"_sv : ""_sv,
		flush_count,
		ircd::pretty(iecbuf[1], iec(flush_bytes)),
		data.out?
			ircd::pretty(iecbuf[0], iec(flush_bytes + size(data.out->completed()))):
			string_view{},
		tmstr
	};
}

//
// item
//

//
// item::item
//

ircd::m::sync::item::item(std::string name,
                          handle polylog,
                          handle linear,
                          const json::members &feature)
:instance_multimap
{
	std::move(name)
}
,conf_name
{
	fmt::snstringf{128, "ircd.m.sync.%s.enable", this->name()},
	fmt::snstringf{128, "ircd.m.sync.%s.stats.debug", this->name()},
}
,enable
{
	{ "name",     conf_name[0] },
	{ "default",  true         },
}
,stats_debug
{
	{ "name",     conf_name[1] },
	{ "default",  false        },
}
,_polylog
{
	std::move(polylog)
}
,_linear
{
	std::move(linear)
}
,feature
{
	feature
}
,opts
{
	this->feature
}
,phased
{
	opts.get<bool>("phased", false)
}
{
	log::debug
	{
		log, "Registered sync item(%p) '%s' (%zu features)",
		this,
		this->name(),
		opts.size(),
	};
}

ircd::m::sync::item::~item()
noexcept
{
	log::debug
	{
		log, "Unregistered sync item(%p) '%s'",
		this,
		this->name()
	};
}

bool
ircd::m::sync::item::polylog(data &data)
try
{
	// Skip the item if disabled by configuration
	if(!enable)
		return false;

	if(data.phased && !phased && int64_t(data.range.first) < 0)
		return false;

	#ifdef RB_DEBUG
	sync::stats stats
	{
		data.stats && (stats_info || stats_debug)?
			*data.stats:
			sync::stats{}
	};

	if(data.stats && (stats_info || stats_debug))
		stats.timer = {};
	#endif

	const bool ret
	{
		_polylog(data)
	};

	#ifdef RB_DEBUG
	if(data.stats && (stats_info || stats_debug))
	{
		//data.out.flush();
		thread_local char tmbuf[32];
		log::debug
		{
			log, "polylog %s commit:%b '%s' %s",
			loghead(data),
			ret,
			name(),
			ircd::pretty(tmbuf, stats.timer.at<microseconds>(), true)
		};
	}
	#endif

	this_ctx::interruption_point();
	return ret;
}
catch(const std::bad_function_call &e)
{
	log::dwarning
	{
		log, "polylog %s '%s' missing handler :%s",
		loghead(data),
		name(),
		e.what()
	};

	return false;
}
catch(const m::error &e)
{
	log::derror
	{
		log, "polylog %s '%s' :%s %s",
		loghead(data),
		name(),
		e.what(),
		e.content
	};

	return false;
}
catch(const std::exception &e)
{
	log::derror
	{
		log, "polylog %s '%s' :%s",
		loghead(data),
		name(),
		e.what()
	};

	throw;
}

bool
ircd::m::sync::item::linear(data &data)
try
{
	if(!enable)
		return false;

	return _linear(data);
}
catch(const std::bad_function_call &e)
{
	thread_local char rembuf[128];
	log::dwarning
	{
		log, "linear %s '%s' missing handler :%s",
		loghead(data),
		name(),
		e.what()
	};

	return false;
}
catch(const m::error &e)
{
	log::derror
	{
		log, "linear %s '%s' :%s %s",
		loghead(data),
		name(),
		e.what(),
		e.content
	};

	return false;
}
catch(const std::exception &e)
{
	log::derror
	{
		log, "linear %s '%s' :%s",
		loghead(data),
		name(),
		e.what()
	};

	throw;
}

size_t
ircd::m::sync::item::children()
const
{
	size_t ret(0);
	sync::for_each(this->name(), [&ret]
	(auto &item)
	{
		++ret;
		return true;
	});

	return ret;
}

ircd::string_view
ircd::m::sync::item::member_name()
const
{
	return token_last(name(), '.');
}

ircd::string_view
ircd::m::sync::item::name()
const
{
	return this->instance_multimap::it->first;
}

///////////////////////////////////////////////////////////////////////////////
//
// m/app.h
//

decltype(ircd::m::app::log)
ircd::m::app::log
{
	"m.app"
};

std::string
ircd::m::app::config::get(const string_view &id)
{
	using prototype = std::string (const string_view &);

	static mods::import<prototype> call
	{
		"app_app", "ircd::m::app::config::get"
	};

	return call(id);
}

std::string
ircd::m::app::config::get(std::nothrow_t,
                       const string_view &id)
{
	using prototype = std::string (std::nothrow_t, const string_view &);

	static mods::import<prototype> call
	{
		"app_app", "ircd::m::app::config::get"
	};

	return call(std::nothrow, id);
}

void
ircd::m::app::config::get(const string_view &id,
                       const event::fetch::view_closure &closure)
{
	using prototype = void (const string_view &, const event::fetch::view_closure &);

	static mods::import<prototype> call
	{
		"app_app", "ircd::m::app::config::get"
	};

	return call(id, closure);
}

bool
ircd::m::app::config::get(std::nothrow_t,
                          const string_view &id,
                          const event::fetch::view_closure &closure)
{
	using prototype = bool (std::nothrow_t, const string_view &, const event::fetch::view_closure &);

	static mods::import<prototype> call
	{
		"app_app", "ircd::m::app::config::get"
	};

	return call(std::nothrow, id, closure);
}

ircd::m::event::idx
ircd::m::app::config::idx(const string_view &id)
{
	using prototype = event::idx (const string_view &);

	static mods::import<prototype> call
	{
		"app_app", "ircd::m::app::config::idx"
	};

	return call(id);
}

ircd::m::event::idx
ircd::m::app::config::idx(std::nothrow_t,
                          const string_view &id)
{
	using prototype = event::idx (std::nothrow_t, const string_view &);

	static mods::import<prototype> call
	{
		"app_app", "ircd::m::app::config::idx"
	};

	return call(std::nothrow, id);
}

bool
ircd::m::app::exists(const string_view &id)
{
	using prototype = bool (const string_view &);

	static mods::import<prototype> call
	{
		"app_app", "exists"
	};

	return call(id);
}

///////////////////////////////////////////////////////////////////////////////
//
// m/feds.h
//

ircd::m::feds::execute::execute(const opts &o,
                                const closure &c)
:execute
{
	vector_view<const opts>{&o, 1}, c
}
{
}

ircd::m::feds::execute::execute(const vector_view<const opts> &o,
                                const closure &c)
{
	using prototype = bool (const vector_view<const opts> &, const closure &);

	static mods::import<prototype> call
	{
		"m_feds", "ircd::m::feds::execute"
	};

	call(o, c);
}

///////////////////////////////////////////////////////////////////////////////
//
// m/vm.h
//

decltype(ircd::m::vm::default_opts)
ircd::m::vm::default_opts;

decltype(ircd::m::vm::default_copts)
ircd::m::vm::default_copts;

decltype(ircd::m::vm::log)
ircd::m::vm::log
{
	"m.vm", 'v'
};

decltype(ircd::m::vm::dock)
ircd::m::vm::dock;

decltype(ircd::m::vm::ready)
ircd::m::vm::ready;

ircd::string_view
ircd::m::vm::loghead(const eval &eval)
{
	thread_local char buf[128];
	return loghead(buf, eval);
}

ircd::string_view
ircd::m::vm::loghead(const mutable_buffer &buf,
                     const eval &eval)
{
	return fmt::sprintf
	{
		buf, "vm:%lu:%lu:%lu eval:%lu seq:%lu share:%lu:%lu %s",
		sequence::uncommitted,
		sequence::committed,
		sequence::retired,
		eval.id,
		sequence::get(eval),
		eval.sequence_shared[0],
		eval.sequence_shared[1],
		eval.event_?
			string_view{eval.event_->event_id}:
			"<unidentified>"_sv,
	};
}

ircd::http::code
ircd::m::vm::http_code(const fault &code)
{
	switch(code)
	{
		case fault::ACCEPT:       return http::OK;
		case fault::EXISTS:       return http::CONFLICT;
		case fault::INVALID:      return http::BAD_REQUEST;
		case fault::GENERAL:      return http::UNAUTHORIZED;
		case fault::AUTH:         return http::FORBIDDEN;
		case fault::STATE:        return http::NOT_FOUND;
		case fault::EVENT:        return http::NOT_FOUND;
		default:                  return http::INTERNAL_SERVER_ERROR;
	}
}

ircd::string_view
ircd::m::vm::reflect(const enum fault &code)
{
	switch(code)
	{
		case fault::ACCEPT:       return "#ACCEPT";
		case fault::EXISTS:       return "#EXISTS";
		case fault::GENERAL:      return "#GENERAL";
		case fault::INVALID:      return "#INVALID";
		case fault::AUTH:         return "#AUTH";
		case fault::EVENT:        return "#EVENT";
		case fault::STATE:        return "#STATE";
		case fault::INTERRUPT:    return "#INTERRUPT";
	}

	return "??????";
}

//
// Eval
//
// Processes any event from any place from any time and does whatever is
// necessary to validate, reject, learn from new information, ignore old
// information and advance the state of IRCd as best as possible.

/// Instance list linkage for all of the evaluations.
template<>
decltype(ircd::util::instance_list<ircd::m::vm::eval>::allocator)
ircd::util::instance_list<ircd::m::vm::eval>::allocator
{};

template<>
decltype(ircd::util::instance_list<ircd::m::vm::eval>::list)
ircd::util::instance_list<ircd::m::vm::eval>::list
{
	allocator
};

decltype(ircd::m::vm::eval::id_ctr)
ircd::m::vm::eval::id_ctr;

decltype(ircd::m::vm::eval::executing)
ircd::m::vm::eval::executing;

decltype(ircd::m::vm::eval::injecting)
ircd::m::vm::eval::injecting;

void
ircd::m::vm::eval::seqsort()
{
	eval::list.sort([]
	(const auto *const &a, const auto *const &b)
	{
		if(sequence::get(*a) == 0)
			return false;

		if(sequence::get(*b) == 0)
			return true;

		return sequence::get(*a) < sequence::get(*b);
	});
}

ircd::m::vm::eval *
ircd::m::vm::eval::seqmin()
{
	const auto it
	{
		std::min_element(begin(eval::list), end(eval::list), []
		(const auto *const &a, const auto *const &b)
		{
			if(sequence::get(*a) == 0)
				return false;

			if(sequence::get(*b) == 0)
				return true;

			return sequence::get(*a) < sequence::get(*b);
		})
	};

	if(it == end(eval::list))
		return nullptr;

	if(sequence::get(**it) == 0)
		return nullptr;

	return *it;
}

ircd::m::vm::eval *
ircd::m::vm::eval::seqmax()
{
	const auto it
	{
		std::max_element(begin(eval::list), end(eval::list), []
		(const auto *const &a, const auto *const &b)
		{
			return sequence::get(*a) < sequence::get(*b);
		})
	};

	if(it == end(eval::list))
		return nullptr;

	if(sequence::get(**it) == 0)
		return nullptr;

	return *it;
}

ircd::m::vm::eval *
ircd::m::vm::eval::seqnext(const uint64_t &seq)
{
	eval *ret{nullptr};
	for(auto *const &eval : eval::list)
	{
		if(sequence::get(*eval) <= seq)
			continue;

		if(!ret || sequence::get(*eval) < sequence::get(*ret))
			ret = eval;
	}

	assert(!ret || sequence::get(*ret) > seq);
	return ret;
}

bool
ircd::m::vm::eval::sequnique(const uint64_t &seq)
{
	return 1 == std::count_if(begin(eval::list), end(eval::list), [&seq]
	(const auto *const &eval)
	{
		return sequence::get(*eval) == seq;
	});
}

ircd::m::vm::eval &
ircd::m::vm::eval::get(const event::id &event_id)
{
	auto *const ret
	{
		find(event_id)
	};

	if(unlikely(!ret))
		throw std::out_of_range
		{
			"eval::get(): event_id not being evaluated."
		};

	return *ret;
}

ircd::m::vm::eval *
ircd::m::vm::eval::find(const event::id &event_id)
{
	eval *ret{nullptr};
	for_each([&event_id, &ret](eval &e)
	{
		if(e.event_)
		{
			if(e.event_->event_id == event_id)
				ret = &e;
		}
		else if(e.issue)
		{
			if(e.issue->has("event_id"))
				if(string_view{e.issue->at("event_id")} == event_id)
					ret = &e;
		}
		else if(e.event_id == event_id)
			ret = &e;

		return ret == nullptr;
	});

	return ret;
}

size_t
ircd::m::vm::eval::count(const ctx::ctx *const &c)
{
	return std::count_if(begin(eval::list), end(eval::list), [&c]
	(const eval *const &eval)
	{
		return eval->ctx == c;
	});
}

bool
ircd::m::vm::eval::for_each(const std::function<bool (eval &)> &closure)
{
	for(eval *const &eval : eval::list)
		if(!closure(*eval))
			return false;

	return true;
}

bool
ircd::m::vm::eval::for_each(const ctx::ctx *const &c,
                            const std::function<bool (eval &)> &closure)
{
	for(eval *const &eval : eval::list)
		if(eval->ctx == c)
			if(!closure(*eval))
				return false;

	return true;
}

//
// eval::eval
//

ircd::m::vm::eval::eval(json::iov &event,
                        const json::iov &content,
                        const vm::copts &opts)
:eval{opts}
{
	operator()(event, content);
}

ircd::m::vm::eval::eval(const event &event,
                        const vm::opts &opts)
:eval{opts}
{
	operator()(event);
}

ircd::m::vm::eval::eval(const json::array &pdus,
                        const vm::opts &opts)
:opts{&opts}
{
	// Sort the events first to avoid complicating the evals; the events might
	// be from different rooms but it doesn't matter.
	std::vector<m::event> events(begin(pdus), end(pdus));
	std::sort(begin(events), end(events));
	this->pdus = events;

	// Conduct each eval without letting any one exception ruin things for the
	// others, including an interrupt. The only exception is a termination.
	for(auto it(begin(events)); it != end(events); ++it) try
	{
		auto &event{*it};
		if(!json::get<"event_id"_>(event))
			event.event_id = event::id::v4
			{
				this->event_id, event
			};

		// When a fault::EXISTS would not actually be revealed to the user in
		// any way we can elide a lot of grief by checking this here first and
		// skipping the event. The query path will be adequately cached anyway.
		if(~(opts.warnlog | opts.errorlog) & fault::EXISTS)
			if(m::exists(event.event_id))
				continue;

		operator()(event);
	}
	catch(const ctx::interrupted &e)
	{
		if(opts.nothrows & fault::INTERRUPT)
			continue;
		else
			throw;
	}
	catch(const std::exception &e)
	{
		continue;
	}
	catch(const ctx::terminated &)
	{
		throw;
	}
}

ircd::m::vm::eval::eval(const vm::copts &opts)
:opts{&opts}
,copts{&opts}
{
}

ircd::m::vm::eval::eval(const vm::opts &opts)
:opts{&opts}
{
}

ircd::m::vm::eval::~eval()
noexcept
{
}

ircd::m::vm::eval::operator
const event::id::buf &()
const
{
	return event_id;
}

const ircd::m::event *
ircd::m::vm::eval::find_pdu(const event::id &event_id)
{
	const m::event *ret{nullptr};
	for_each_pdu([&ret, &event_id]
	(const m::event &event)
	{
		if(event.event_id != event_id)
			return true;

		ret = std::addressof(event);
		return false;
	});

	return ret;
}

const ircd::m::event *
ircd::m::vm::eval::find_pdu(const eval &eval,
                            const event::id &event_id)
{
	const m::event *ret{nullptr};
	for(const auto &event : eval.pdus)
	{
		if(event.event_id != event_id)
			continue;

		ret = std::addressof(event);
		break;
	}

	return ret;
}

bool
ircd::m::vm::eval::for_each_pdu(const std::function<bool (const event &)> &closure)
{
	return for_each([&closure](eval &e)
	{
		if(!empty(e.pdus))
		{
			for(const auto &pdu : e.pdus)
				if(!closure(pdu))
					return false;
		}
		else if(e.event_)
		{
			if(!closure(*e.event_))
				return false;
		}

		return true;
	});
}

///
/// Figure 1:
///          in     .  <-- injection
///    ===:::::::==//
///    |  ||||||| //   <-- these functions
///    |   \\|// //|
///    |    ||| // |   |  acceleration
///    |    |||//  |   |
///    |    |||/   |   |
///    |    |||    |   V
///    |    !!!    |
///    |     *     |   <----- nozzle
///    | ///|||\\\ |
///    |/|/|/|\|\|\|   <---- propagation cone
///  _/|/|/|/|\|\|\|\_
///         out
///

/// Inject a new event originating from this server.
///
enum ircd::m::vm::fault
ircd::m::vm::eval::operator()(json::iov &event,
                              const json::iov &contents)
{
	using prototype = fault (eval &, json::iov &, const json::iov &);

	static mods::import<prototype> call
	{
		"m_vm", "ircd::m::vm::inject"
	};

	vm::dock.wait([]
	{
		return vm::ready;
	});

	return call(*this, event, contents);
}

enum ircd::m::vm::fault
ircd::m::vm::eval::operator()(const event &event)
{
	using prototype = fault (eval &, const m::event &);

	static mods::import<prototype> call
	{
		"m_vm", "ircd::m::vm::execute"
	};

	vm::dock.wait([]
	{
		return vm::ready;
	});

	return call(*this, event);
}

//
// sequence
//

decltype(ircd::m::vm::sequence::dock)
ircd::m::vm::sequence::dock;

decltype(ircd::m::vm::sequence::retired)
ircd::m::vm::sequence::retired;

decltype(ircd::m::vm::sequence::committed)
ircd::m::vm::sequence::committed;

decltype(ircd::m::vm::sequence::uncommitted)
ircd::m::vm::sequence::uncommitted;

uint64_t
ircd::m::vm::sequence::min()
{
	const auto *const e
	{
		eval::seqmin()
	};

	return e? get(*e) : 0;
}

uint64_t
ircd::m::vm::sequence::max()
{
	const auto *const e
	{
		eval::seqmax()
	};

	return e? get(*e) : 0;
}

uint64_t
ircd::m::vm::sequence::get(id::event::buf &event_id)
{
	static constexpr auto column_idx
	{
		json::indexof<event, "event_id"_>()
	};

	auto &column
	{
		dbs::event_column.at(column_idx)
	};

	const auto it
	{
		column.rbegin()
	};

	if(!it)
	{
		// If this iterator is invalid the events db should
		// be completely fresh.
		assert(db::sequence(*dbs::events) == 0);
		return 0;
	}

	const auto &ret
	{
		byte_view<uint64_t>(it->first)
	};

	event_id = it->second;
	return ret;
}

const uint64_t &
ircd::m::vm::sequence::get(const eval &eval)
{
	return eval.sequence;
}

///////////////////////////////////////////////////////////////////////////////
//
// m/redacted.h
//

ircd::m::redacted::redacted(const event::idx &event_idx)
:ret
{
	event_idx?
		event::refs(event_idx).has(dbs::ref::M_ROOM_REDACTION):
		false
}
{
}

///////////////////////////////////////////////////////////////////////////////
//
// m/visible.h
//

bool
ircd::m::visible(const event::id &event_id,
                 const string_view &mxid)
{
	m::room::id::buf room_id
	{
		get(event_id, "room_id", room_id)
	};

	const m::event event
	{
		json::members
		{
			{ "event_id",  event_id  },
			{ "room_id",   room_id   },
		}
	};

	return visible(event, mxid);
}

bool
ircd::m::visible(const event &event,
                 const string_view &mxid)
{
	using prototype = bool (const m::event &, const string_view &);

	static mods::import<prototype> call
	{
		"m_room_history_visibility", "ircd::m::visible"
	};

	return call(event, mxid);
}

///////////////////////////////////////////////////////////////////////////////
//
// m/receipt.h
//

ircd::m::event::id::buf
ircd::m::receipt::read(const id::room &room_id,
                       const id::user &user_id,
                       const id::event &event_id)
{
	return read(room_id, user_id, event_id, ircd::time<milliseconds>());
}

ircd::m::event::id::buf
ircd::m::receipt::read(const room::id &room_id,
                       const user::id &user_id,
                       const event::id &event_id,
                       const time_t &ms)
{
	using prototype = event::id::buf (const room::id &, const user::id &, const event::id &, const time_t &);

	static mods::import<prototype> function
	{
		"m_receipt", "ircd::m::receipt::read"
	};

	return function(room_id, user_id, event_id, ms);
}

ircd::m::event::id
ircd::m::receipt::read(event::id::buf &out,
                       const room::id &room_id,
                       const user::id &user_id)
{
	const event::id::closure copy{[&out]
	(const event::id &event_id)
	{
		out = event_id;
	}};

	return read(room_id, user_id, copy)?
		event::id{out}:
		event::id{};
}

bool
ircd::m::receipt::read(const room::id &room_id,
                       const user::id &user_id,
                       const event::id::closure &closure)
{
	using prototype = bool (const room::id &, const user::id &, const event::id::closure &);

	static mods::import<prototype> function
	{
		"m_receipt", "ircd::m::receipt::read"
	};

	return function(room_id, user_id, closure);
}

bool
ircd::m::receipt::ignoring(const user &user,
                           const room::id &room_id)
{
	using prototype = bool (const m::user &, const m::room::id &);

	static mods::import<prototype> function
	{
		"m_receipt", "ircd::m::receipt::ignoring"
	};

	return function(user, room_id);
}

bool
ircd::m::receipt::ignoring(const user &user,
                           const event::id &event_id)
{
	using prototype = bool (const m::user &, const m::event::id &);

	static mods::import<prototype> function
	{
		"m_receipt", "ircd::m::receipt::ignoring"
	};

	return function(user, event_id);
}

bool
ircd::m::receipt::freshest(const room::id &room_id,
                           const user::id &user_id,
                           const event::id &event_id)
{
	using prototype = bool (const m::room::id &, const m::user::id &, const m::event::id &);

	static mods::import<prototype> function
	{
		"m_receipt", "ircd::m::receipt::freshest"
	};

	return function(room_id, user_id, event_id);
}

bool
ircd::m::receipt::exists(const room::id &room_id,
                         const user::id &user_id,
                         const event::id &event_id)
{
	using prototype = bool (const m::room::id &, const m::user::id &, const m::event::id &);

	static mods::import<prototype> function
	{
		"m_receipt", "ircd::m::receipt::exists"
	};

	return function(room_id, user_id, event_id);
}

///////////////////////////////////////////////////////////////////////////////
//
// m/typing.h
//

//
// m::typing::commit::commit
//

ircd::m::typing::commit::commit(const m::typing &object)
:ircd::m::event::id::buf{[&object]
{
	using prototype = m::event::id::buf (const m::typing &);

	static mods::import<prototype> function
	{
		"m_typing", "commit"
	};

	return function(object);
}()}
{
}

///////////////////////////////////////////////////////////////////////////////
//
// m/presence.h
//

ircd::m::presence::presence(const user &user,
                            const mutable_buffer &buf)
:edu::m_presence{[&user, &buf]
{
	json::object ret;
	get(user, [&ret, &buf]
	(const json::object &content)
	{
		ret =
		{
			data(buf), copy(buf, string_view{content})
		};
	});

	return ret;
}()}
{
}

ircd::m::event::id::buf
ircd::m::presence::set(const user &user,
                       const string_view &presence,
                       const string_view &status_msg)
{
	return set(m::presence
	{
		{ "user_id",           user.user_id         },
		{ "presence",          presence             },
		{ "status_msg",        status_msg           },
		{ "currently_active",  presence == "online" },
	});
}

ircd::m::event::id::buf
ircd::m::presence::set(const presence &object)
{
	using prototype = event::id::buf (const presence &);

	static mods::import<prototype> function
	{
		"m_presence", "ircd::m::presence::set"
	};

	return function(object);
}

void
ircd::m::presence::get(const user &user,
                       const closure &closure)
{
	if(!get(std::nothrow, user, closure))
		throw m::NOT_FOUND
		{
			"No presence found for %s", string_view{user.user_id}
		};
}

bool
ircd::m::presence::get(std::nothrow_t,
                       const user &user,
                       const closure &closure)
{
	static const m::event::fetch::opts fopts
	{
		m::event::keys::include {"content"}
	};

	const auto reclosure{[&closure]
	(const m::event &event)
	{
		closure(json::get<"content"_>(event));
	}};

	return get(std::nothrow, user, reclosure, &fopts);
}

bool
ircd::m::presence::get(std::nothrow_t,
                       const user &user,
                       const closure_event &closure,
                       const event::fetch::opts *const &opts)
{
	using prototype = bool (std::nothrow_t, const m::user &, const closure_event &, const event::fetch::opts *const &);

	static mods::import<prototype> function
	{
		"m_presence", "ircd::m::presence::get"
	};

	return function(std::nothrow, user, closure, opts);
}

ircd::m::event::idx
ircd::m::presence::get(const user &user)
{
	const event::idx ret
	{
		get(std::nothrow, user)
	};

	if(!ret)
		throw m::NOT_FOUND
		{
			"No presence found for %s", string_view{user.user_id}
		};

	return ret;
}

ircd::m::event::idx
ircd::m::presence::get(std::nothrow_t,
                       const user &user)
{
	using prototype = event::idx (std::nothrow_t, const m::user &);

	static mods::import<prototype> function
	{
		"m_presence", "ircd::m::presence::get"
	};

	return function(std::nothrow, user);
}

bool
ircd::m::presence::valid_state(const string_view &state)
{
	using prototype = bool (const string_view &);

	static mods::import<prototype> function
	{
		"m_presence", "ircd::m::presence::valid_state"
	};

	return function(state);
}

///////////////////////////////////////////////////////////////////////////////
//
// m/device.h
//

bool
ircd::m::device::set(const m::user &user,
                     const device &device_)
{
	using prototype = bool (const m::user &, const device &);

	static mods::import<prototype> function
	{
		"m_device", "ircd::m::device::set"
	};

	return function(user, device_);
}

bool
ircd::m::device::set(const m::user &user,
                     const string_view &id,
                     const string_view &prop,
                     const string_view &val)
{
	using prototype = bool (const m::user &, const string_view &, const string_view &, const string_view &);

	static mods::import<prototype> function
	{
		"m_device", "ircd::m::device::set"
	};

	return function(user, id, prop, val);
}

bool
ircd::m::device::del(const m::user &user,
                     const string_view &id)
{
	using prototype = bool (const m::user &, const string_view &);

	static mods::import<prototype> function
	{
		"m_device", "ircd::m::device::del"
	};

	return function(user, id);
}

bool
ircd::m::device::has(const m::user &user,
                     const string_view &id)
{
	using prototype = bool (const m::user &, const string_view &id);

	static mods::import<prototype> function
	{
		"m_device", "ircd::m::device::has"
	};

	return function(user, id);
}

bool
ircd::m::device::has(const m::user &user,
                     const string_view &id,
                     const string_view &prop)
{
	using prototype = bool (const m::user &, const string_view &id, const string_view &prop);

	static mods::import<prototype> function
	{
		"m_device", "ircd::m::device::has"
	};

	return function(user, id, prop);
}

bool
ircd::m::device::get(const m::user &user,
                     const string_view &id,
                     const string_view &prop,
                     const closure &c)
{
	const bool ret
	{
		get(std::nothrow, user, id, prop, c)
	};

	if(!ret)
		throw m::NOT_FOUND
		{
			"Property '%s' for device '%s' for user %s not found",
			id,
			prop,
			string_view{user.user_id}
		};

	return ret;
}

bool
ircd::m::device::get(std::nothrow_t,
                     const m::user &user,
                     const string_view &id,
                     const string_view &prop,
                     const closure &c)
{
	using prototype = bool (std::nothrow_t,
	                        const m::user &,
	                        const string_view &,
	                        const string_view &,
	                        const closure &);

	static mods::import<prototype> function
	{
		"m_device", "ircd::m::device::get"
	};

	return function(std::nothrow, user, id, prop, c);
}

bool
ircd::m::device::for_each(const m::user &user,
                          const string_view &id,
                          const closure_bool &c)
{
	using prototype = bool (const m::user &, const string_view &id, const closure_bool &);

	static mods::import<prototype> function
	{
		"m_device", "ircd::m::device::for_each"
	};

	return function(user, id, c);
}

bool
ircd::m::device::for_each(const m::user &user,
                          const closure_bool &c)
{
	using prototype = bool (const m::user &, const closure_bool &);

	static mods::import<prototype> function
	{
		"m_device", "ircd::m::device::for_each"
	};

	return function(user, c);
}

///////////////////////////////////////////////////////////////////////////////
//
// m/node.h
//

//
// node
//

ircd::m::node::node(const string_view &node_id)
:node_id{node_id}
{
	rfc3986::valid_remote(node_id);
}

void
ircd::m::node::key(const string_view &key_id,
                   const ed25519_closure &closure)
const
{
	key(key_id, key_closure{[&closure]
	(const string_view &keyb64)
	{
		const ed25519::pk pk
		{
			[&keyb64](auto &buf)
			{
				b64decode(buf, unquote(keyb64));
			}
		};

		closure(pk);
	}});
}

void
ircd::m::node::key(const string_view &key_id,
                   const key_closure &closure)
const
{
	using prototype = void (const string_view &, const string_view &, const keys::closure &);

	//TODO: Remove this import once this callsite is outside of libircd.
	static mods::import<prototype> call
	{
		"m_keys", "ircd::m::keys::get"
	};

	call(node_id, key_id, [&closure, &key_id]
	(const json::object &keys)
	{
		const json::object &vks
		{
			keys.at("verify_keys")
		};

		const json::object &vkk
		{
			vks.at(key_id)
		};

		const string_view &key
		{
			vkk.at("key")
		};

		closure(key);
	});
}

/// Generates a node-room ID into buffer; see room_id() overload.
ircd::m::id::room::buf
ircd::m::node::room_id()
const
{
	ircd::m::id::room::buf buf;
	return buf.assigned(room_id(buf));
}

/// This generates a room mxid for the "node's room" essentially serving as
/// a database mechanism for this specific node. This room_id is a hash of
/// the node's full mxid.
///
ircd::m::id::room
ircd::m::node::room_id(const mutable_buffer &buf)
const
{
	assert(!empty(this->node_id));

	// for compatibility with hashing legacy node_id's
	thread_local char node_id_buf[m::id::MAX_SIZE];
	mutable_buffer mb{node_id_buf};
	consume(mb, copy(mb, "::"_sv));
	consume(mb, copy(mb, this->node_id));
	const string_view &node_id
	{
		node_id_buf, data(mb)
	};

	const sha256::buf hash
	{
		sha256{node_id}
	};

	thread_local char b58_buf[b58encode_size(size(hash))];
	const string_view b58
	{
		b58encode(b58_buf, hash)
	};

	return id::room
	{
		buf, b58, my_host()
	};
}

//
// node::room
//

ircd::m::node::room::room(const string_view &node_id)
:room{m::node{node_id}}
{}

ircd::m::node::room::room(const m::node &node)
:node{node}
,room_id{node.room_id()}
{
	static_cast<m::room &>(*this) = room_id;
}

///////////////////////////////////////////////////////////////////////////////
//
// m/filter.h
//

//TODO: globular expression
//TODO: tribool for contains_url; we currently ignore the false value.
bool
ircd::m::match(const room_event_filter &filter,
               const event &event)
{
	if(json::get<"contains_url"_>(filter) == true)
		if(!at<"content"_>(event).has("url"))
			return false;

	for(const auto &room_id : json::get<"not_rooms"_>(filter))
		if(at<"room_id"_>(event) == unquote(room_id))
			return false;

	if(empty(json::get<"rooms"_>(filter)))
		return match(event_filter{filter}, event);

	for(const auto &room_id : json::get<"rooms"_>(filter))
		if(at<"room_id"_>(event) == unquote(room_id))
			return match(event_filter{filter}, event);

	return false;
}

//TODO: globular expression
bool
ircd::m::match(const event_filter &filter,
               const event &event)
{
	for(const auto &type : json::get<"not_types"_>(filter))
		if(at<"type"_>(event) == unquote(type))
			return false;

	for(const auto &sender : json::get<"not_senders"_>(filter))
		if(at<"sender"_>(event) == unquote(sender))
			return false;

	if(empty(json::get<"senders"_>(filter)) && empty(json::get<"types"_>(filter)))
		return true;

	if(empty(json::get<"senders"_>(filter)))
	{
		for(const auto &type : json::get<"types"_>(filter))
			if(at<"type"_>(event) == unquote(type))
				return true;

		return false;
	}

	if(empty(json::get<"types"_>(filter)))
	{
		for(const auto &sender : json::get<"senders"_>(filter))
			if(at<"sender"_>(event) == unquote(sender))
				return true;

		return false;
	}

	return true;
}

//
// filter
//

/// Convenience interface for filters out of common `?filter=` query string
/// arguments. This function expects a raw urlencoded value of the filter
/// query parameter. It detects if the value is an "inline" filter by being
/// a valid JSON object; otherwise it considers the value an ID and fetches
/// the filter stored previously by the user.
std::string
ircd::m::filter::get(const string_view &val,
                     const m::user &user)
{
	if(!val)
		return {};

	const bool is_inline
	{
		startswith(val, "{") || startswith(val, "%7B")
	};

	if(is_inline)
		return util::string(val.size(), [&val]
		(const mutable_buffer &buf)
		{
			return url::decode(buf, val);
		});

	if(!user.user_id)
		return {};

	char idbuf[m::event::STATE_KEY_MAX_SIZE];
	const string_view &id
	{
		url::decode(idbuf, val)
	};

	const m::user::filter filter
	{
		user
	};

	return filter.get(id);
}

//
// filter::filter
//

ircd::m::filter::filter(const user &user,
                        const string_view &filter_id,
                        const mutable_buffer &buf)
{
	const json::object &obj
	{
		user::filter(user).get(buf, filter_id)
	};

	new (this) m::filter
	{
		obj
	};
}

//
// room_filter
//

ircd::m::room_filter::room_filter(const mutable_buffer &buf,
                                  const json::members &members)
:super_type::tuple
{
	json::stringify(mutable_buffer{buf}, members)
}
{
}

//
// state_filter
//

ircd::m::state_filter::state_filter(const mutable_buffer &buf,
                                    const json::members &members)
:super_type::tuple
{
	json::stringify(mutable_buffer{buf}, members)
}
{
}

//
// room_event_filter
//

ircd::m::room_event_filter::room_event_filter(const mutable_buffer &buf,
                                              const json::members &members)
:super_type::tuple
{
	json::stringify(mutable_buffer{buf}, members)
}
{
}

//
// event_filter
//

ircd::m::event_filter::event_filter(const mutable_buffer &buf,
                                    const json::members &members)
:super_type::tuple
{
	json::stringify(mutable_buffer{buf}, members)
}
{
}

///////////////////////////////////////////////////////////////////////////////
//
// m/user.h
//

/// ID of the room which stores ephemeral tokens (an instance of the room is
/// provided below).
ircd::m::room::id::buf
tokens_room_id
{
	"tokens", ircd::my_host()
};

/// The tokens room serves as a key-value lookup for various tokens to
/// users, etc. It primarily serves to store access tokens for users. This
/// is a separate room from the users room because in the future it may
/// have an optimized configuration as well as being more easily cleared.
///
ircd::m::room
ircd::m::user::tokens
{
	tokens_room_id
};

bool
ircd::m::exists(const user::id &user_id)
{
	// The way we know a user exists is testing if their room exists.
	const m::user::room user_room
	{
		user_id
	};

	return m::exists(user_room);
}

bool
ircd::m::exists(const user &user)
{
	return exists(user.user_id);
}

bool
ircd::m::my(const user &user)
{
	return my(user.user_id);
}

//
// user
//

/// Generates a user-room ID into buffer; see room_id() overload.
ircd::m::id::room::buf
ircd::m::user::room_id()
const
{
	ircd::m::id::room::buf buf;
	return buf.assigned(room_id(buf));
}

/// This generates a room mxid for the "user's room" essentially serving as
/// a database mechanism for this specific user. This room_id is a hash of
/// the user's full mxid.
///
ircd::m::id::room
ircd::m::user::room_id(const mutable_buffer &buf)
const
{
	assert(!empty(user_id));
	const ripemd160::buf hash
	{
		ripemd160{user_id}
	};

	char b58[size(hash) * 2];
	return
	{
		buf, b58encode(b58, hash), my_host()
	};
}

ircd::m::device::id::buf
ircd::m::user::get_device_from_access_token(const string_view &token)
{
	const event::idx event_idx
	{
		user::tokens.get("ircd.access_token", token)
	};

	device::id::buf ret;
	m::get(event_idx, "content", [&ret]
	(const json::object &content)
	{
		ret = unquote(content.at("device_id"));
	});

	return ret;
}

ircd::string_view
ircd::m::user::gen_access_token(const mutable_buffer &buf)
{
	static const size_t token_max{32};
	static const auto &token_dict{rand::dict::alpha};

	const mutable_buffer out
	{
		data(buf), std::min(token_max, size(buf))
	};

	return rand::string(token_dict, out);
}

ircd::m::event::id::buf
ircd::m::user::activate()
{
	using prototype = event::id::buf (const m::user &);

	static mods::import<prototype> function
	{
		"client_account", "activate__user"
	};

	return function(*this);
}

ircd::m::event::id::buf
ircd::m::user::deactivate()
{
	using prototype = event::id::buf (const m::user &);

	static mods::import<prototype> function
	{
		"client_account", "deactivate__user"
	};

	return function(*this);
}

bool
ircd::m::user::is_active()
const
{
	using prototype = bool (const m::user &);

	static mods::import<prototype> function
	{
		"client_account", "is_active__user"
	};

	return function(*this);
}

ircd::m::event::id::buf
ircd::m::user::password(const string_view &password)
{
	using prototype = event::id::buf (const m::user::id &, const string_view &) noexcept;

	static mods::import<prototype> function
	{
		"client_account", "set_password"
	};

	return function(user_id, password);
}

bool
ircd::m::user::is_password(const string_view &password)
const noexcept try
{
	using prototype = bool (const m::user::id &, const string_view &) noexcept;

	static mods::import<prototype> function
	{
		"client_account", "is_password"
	};

	return function(user_id, password);
}
catch(const std::exception &e)
{
	log::critical
	{
		"user::is_password(): %s %s",
		string_view{user_id},
		e.what()
	};

	return false;
}

//
// user::room
//

ircd::m::user::room::room(const m::user::id &user_id,
                          const vm::copts *const &copts,
                          const event::fetch::opts *const &fopts)
:room
{
	m::user{user_id}, copts, fopts
}
{
}

ircd::m::user::room::room(const m::user &user,
                          const vm::copts *const &copts,
                          const event::fetch::opts *const &fopts)
:user{user}
,room_id{user.room_id()}
{
	static_cast<m::room &>(*this) =
	{
		room_id, copts, fopts
	};
}

bool
ircd::m::user::room::is(const room::id &room_id,
                        const user::id &user_id)
{
	const user::room user_room{user_id};
	return user_room.room_id == room_id;
}

///////////////////////////////////////////////////////////////////////////////
//
// m/txn.h
//

/// Returns the serial size of the JSON this txn would consume. Note: this
/// creates a json::iov involving a timestamp to figure out the total size
/// of the txn. When the user creates the actual txn a different timestamp
/// is created which may be a different size. Consider using the lower-level
/// create(closure) or add some pad to be sure.
///
size_t
ircd::m::txn::serialized(const array &pdu,
                         const array &edu,
                         const array &pdu_failure)
{
	size_t ret;
	const auto closure{[&ret]
	(const json::iov &iov)
	{
		ret = json::serialized(iov);
	}};

	create(closure, pdu, edu, pdu_failure);
	return ret;
}

/// Stringifies a txn from the inputs into the returned std::string
///
std::string
ircd::m::txn::create(const array &pdu,
                     const array &edu,
                     const array &pdu_failure)
{
	std::string ret;
	const auto closure{[&ret]
	(const json::iov &iov)
	{
		ret = json::strung(iov);
	}};

	create(closure, pdu, edu, pdu_failure);
	return ret;
}

/// Stringifies a txn from the inputs into the buffer
///
ircd::string_view
ircd::m::txn::create(const mutable_buffer &buf,
                     const array &pdu,
                     const array &edu,
                     const array &pdu_failure)
{
	string_view ret;
	const auto closure{[&buf, &ret]
	(const json::iov &iov)
	{
		ret = json::stringify(mutable_buffer{buf}, iov);
	}};

	create(closure, pdu, edu, pdu_failure);
	return ret;
}

/// Forms a txn from the inputs into a json::iov and presents that iov
/// to the user's closure.
///
void
ircd::m::txn::create(const closure &closure,
                     const array &pdu,
                     const array &edu,
                     const array &pdu_failure)
{
	using ircd::size;

	json::iov iov;
	const json::iov::push push[]
	{
		{ iov, { "origin",            my_host()                   }},
		{ iov, { "origin_server_ts",  ircd::time<milliseconds>()  }},
	};

	const json::iov::add _pdus
	{
		iov, !empty(pdu),
		{
			"pdus", [&pdu]() -> json::value
			{
				return { data(pdu), size(pdu) };
			}
		}
	};

	const json::iov::add _edus
	{
		iov, !empty(edu),
		{
			"edus", [&edu]() -> json::value
			{
				return { data(edu), size(edu) };
			}
		}
	};

	const json::iov::add _pdu_failures
	{
		iov, !empty(pdu_failure),
		{
			"pdu_failures", [&pdu_failure]() -> json::value
			{
				return { data(pdu_failure), size(pdu_failure) };
			}
		}
	};

	closure(iov);
}

ircd::string_view
ircd::m::txn::create_id(const mutable_buffer &out,
                        const string_view &txn)
{
	const sha256::buf hash
	{
		sha256{txn}
	};

	const string_view txnid
	{
		b58encode(out, hash)
	};

	return txnid;
}


///////////////////////////////////////////////////////////////////////////////
//
// m/request.h
//

//
// request
//

ircd::m::request::request(const string_view &method,
                          const string_view &uri,
                          const mutable_buffer &body_buf,
                          const json::members &body)
:request
{
	my_host(),
	string_view{},
	method,
	uri,
	json::stringify(mutable_buffer{body_buf}, body)
}
{}

ircd::m::request::request(const string_view &method,
                          const string_view &uri)
:request
{
	my_host(),
	string_view{},
	method,
	uri,
	json::object{}
}
{}

ircd::m::request::request(const string_view &method,
                          const string_view &uri,
                          const json::object &content)
:request
{
	my_host(),
	string_view{},
	method,
	uri,
	content
}
{}

ircd::m::request::request(const string_view &origin,
                          const string_view &destination,
                          const string_view &method,
                          const string_view &uri,
                          const json::object &content)
{
	json::get<"origin"_>(*this) = origin;
	json::get<"destination"_>(*this) = destination;
	json::get<"method"_>(*this) = method;
	json::get<"uri"_>(*this) = uri;
	json::get<"content"_>(*this) = content;

	if(unlikely(origin && !rfc3986::valid_remote(std::nothrow, origin)))
		throw m::error
		{
			http::BAD_REQUEST, "M_REQUEST_INVALID_ORIGIN",
			"This origin string '%s' is not a valid remote.",
			origin
		};

	if(unlikely(destination && !rfc3986::valid_remote(std::nothrow, destination)))
		throw m::error
		{
			http::BAD_REQUEST, "M_REQUEST_INVALID_DESTINATION",
			"This destination string '%s' is not a valid remote.",
			destination
		};
}

decltype(ircd::m::request::headers_max)
ircd::m::request::headers_max
{
	32UL
};

ircd::string_view
ircd::m::request::operator()(const mutable_buffer &out,
                             const vector_view<const http::header> &addl_headers)
const
{
	thread_local http::header header[headers_max];
	const ctx::critical_assertion ca;
	size_t headers{0};

	header[headers++] =
	{
		"User-Agent", info::user_agent
	};

	thread_local char x_matrix[2_KiB];
	if(startswith(json::at<"uri"_>(*this), "/_matrix/federation"))
	{
		const auto &sk{self::secret_key};
		const auto &pkid{self::public_key_id};
		header[headers++] =
		{
			"Authorization", generate(x_matrix, sk, pkid)
		};
	}

	assert(headers <= headers_max);
	assert(headers + addl_headers.size() <= headers_max);
	for(size_t i(0); i < addl_headers.size() && headers < headers_max; ++i)
		header[headers++] = addl_headers.at(i);

	static const string_view content_type
	{
		"application/json; charset=utf-8"_sv
	};

	const auto content_length
	{
		string_view(json::get<"content"_>(*this)).size()
	};

	window_buffer sb{out};
	http::request
	{
		sb,
		json::at<"destination"_>(*this),
		json::at<"method"_>(*this),
		json::at<"uri"_>(*this),
		content_length,
		content_type,
		{ header, headers }
	};

	return sb.completed();
}

decltype(ircd::m::request::generate_content_max)
ircd::m::request::generate_content_max
{
	{ "name",    "ircd.m.request.generate.content_max" },
	{ "default",  long(1_MiB)                          },
};

ircd::string_view
ircd::m::request::generate(const mutable_buffer &out,
                           const ed25519::sk &sk,
                           const string_view &pkid)
const
{
	const ctx::critical_assertion ca;
	thread_local unique_buffer<mutable_buffer> buf
	{
		size_t(generate_content_max)
	};

	if(unlikely(json::serialized(*this) > buffer::size(buf)))
		throw m::error
		{
			"M_REQUEST_TOO_LARGE", "This server generated a request of %zu bytes; limit is %zu",
			json::serialized(*this),
			buffer::size(buf)
		};

	const json::object object
	{
		stringify(mutable_buffer{buf}, *this)
	};

	const ed25519::sig sig
	{
		self::secret_key.sign(object)
	};

	const auto &origin
	{
		unquote(string_view{json::at<"origin"_>(*this)})
	};

	thread_local char sigb64[1_KiB];
	return fmt::sprintf
	{
		out, "X-Matrix origin=%s,key=\"%s\",sig=\"%s\"",
		origin,
		pkid,
		b64encode_unpadded(sigb64, sig)
	};
}

bool
ircd::m::request::verify(const string_view &key,
                         const string_view &sig_)
const
{
	const ed25519::sig sig
	{
		[&sig_](auto &buf)
		{
			b64decode(buf, sig_);
		}
	};

	const json::string &origin
	{
		json::at<"origin"_>(*this)
	};

	const m::node node
	{
		origin
	};

	bool verified{false};
	node.key(key, [this, &verified, &sig]
	(const ed25519::pk &pk)
	{
		verified = verify(pk, sig);
	});

	return verified;
}

decltype(ircd::m::request::verify_content_max)
ircd::m::request::verify_content_max
{
	{ "name",    "ircd.m.request.verify.content_max" },
	{ "default",  long(1_MiB)                        },
};

bool
ircd::m::request::verify(const ed25519::pk &pk,
                         const ed25519::sig &sig)
const
{
	// Matrix spec sez that an empty content object {} is excluded entirely
	// from the verification. Our JSON only excludes members if they evaluate
	// to undefined i.e json::object{}/string_view{} but not json::object{"{}"}
	// or even json::object{""}; rather than burdening the caller with ensuring
	// their assignment conforms perfectly, we ensure correctness manually.
	auto _this(*this);
	if(empty(json::get<"content"_>(*this)))
		json::get<"content"_>(_this) = json::object{};

	const ctx::critical_assertion ca;
	thread_local unique_buffer<mutable_buffer> buf
	{
		size_t(verify_content_max)
	};

	const size_t request_size
	{
		json::serialized(_this)
	};

	if(unlikely(request_size > buffer::size(buf)))
		throw m::error
		{
			http::PAYLOAD_TOO_LARGE, "M_REQUEST_TOO_LARGE",
			"The request size %zu bytes exceeds maximum of %zu bytes",
			request_size,
			buffer::size(buf)
		};

	const json::object object
	{
		stringify(mutable_buffer{buf}, _this)
	};

	return verify(pk, sig, object);
}

bool
ircd::m::request::verify(const ed25519::pk &pk,
                         const ed25519::sig &sig,
                         const json::object &object)
{
	return pk.verify(object, sig);
}

//
// x_matrix
//

ircd::m::request::x_matrix::x_matrix(const string_view &input)
{
	string_view tokens[3];
	if(ircd::tokens(split(input, ' ').second, ',', tokens) != 3)
		throw std::out_of_range{"The x_matrix header is malformed"};

	for(const auto &token : tokens)
	{
		const auto &kv{split(token, '=')};
		const auto &val{unquote(kv.second)};
		switch(hash(kv.first))
		{
		    case hash("origin"):  origin = val;  break;
		    case hash("key"):     key = val;     break;
		    case hash("sig"):     sig = val;     break;
		}
	}

	if(empty(origin))
		throw std::out_of_range{"The x_matrix header is missing 'origin='"};

	if(empty(key))
		throw std::out_of_range{"The x_matrix header is missing 'key='"};

	if(empty(sig))
		throw std::out_of_range{"The x_matrix header is missing 'sig='"};
}

///////////////////////////////////////////////////////////////////////////////
//
// m/hook.h
//

// Internal utils
namespace ircd::m
{
	static bool _hook_match(const m::event &matching, const m::event &);
	static void _hook_fix_state_key(const json::members &, json::member &);
	static void _hook_fix_room_id(const json::members &, json::member &);
	static void _hook_fix_sender(const json::members &, json::member &);
	static json::strung _hook_make_feature(const json::members &);
}

/// Instance list linkage for all hook sites
template<>
decltype(ircd::util::instance_list<ircd::m::hook::base::site>::allocator)
ircd::util::instance_list<ircd::m::hook::base::site>::allocator
{};

template<>
decltype(ircd::util::instance_list<ircd::m::hook::base::site>::list)
ircd::util::instance_list<ircd::m::hook::base::site>::list
{
	allocator
};

/// Instance list linkage for all hooks
template<>
decltype(ircd::util::instance_list<ircd::m::hook::base>::allocator)
ircd::util::instance_list<ircd::m::hook::base>::allocator
{};

template<>
decltype(ircd::util::instance_list<ircd::m::hook::base>::list)
ircd::util::instance_list<ircd::m::hook::base>::list
{
	allocator
};

//
// hook::maps
//

struct ircd::m::hook::maps
{
	std::multimap<string_view, base *> origin;
	std::multimap<string_view, base *> room_id;
	std::multimap<string_view, base *> sender;
	std::multimap<string_view, base *> state_key;
	std::multimap<string_view, base *> type;
	std::vector<base *> always;

	size_t match(const event &match, const std::function<bool (base &)> &) const;
	size_t add(base &hook, const event &matching);
	size_t del(base &hook, const event &matching);

	maps();
	~maps() noexcept;
};

ircd::m::hook::maps::maps()
{
}

ircd::m::hook::maps::~maps()
noexcept
{
}

size_t
ircd::m::hook::maps::add(base &hook,
                         const event &matching)
{
	size_t ret{0};
	const auto map{[&hook, &ret]
	(auto &map, const string_view &value)
	{
		map.emplace(value, &hook);
		++ret;
	}};

	if(json::get<"origin"_>(matching))
		map(origin, at<"origin"_>(matching));

	if(json::get<"room_id"_>(matching))
		map(room_id, at<"room_id"_>(matching));

	if(json::get<"sender"_>(matching))
		map(sender, at<"sender"_>(matching));

	if(json::get<"state_key"_>(matching))
		map(state_key, at<"state_key"_>(matching));

	if(json::get<"type"_>(matching))
		map(type, at<"type"_>(matching));

	// Hook had no mappings which means it will match everything.
	// We don't increment the matcher count for this case.
	if(!ret)
		always.emplace_back(&hook);

	return ret;
}

size_t
ircd::m::hook::maps::del(base &hook,
                         const event &matching)
{
	size_t ret{0};
	const auto unmap{[&hook, &ret]
	(auto &map, const string_view &value)
	{
		auto pit{map.equal_range(value)};
		while(pit.first != pit.second)
			if(pit.first->second == &hook)
			{
				pit.first = map.erase(pit.first);
				++ret;
			}
			else ++pit.first;
	}};

	// Unconditional attempt to remove from always.
	std::remove(begin(always), end(always), &hook);

	if(json::get<"origin"_>(matching))
		unmap(origin, at<"origin"_>(matching));

	if(json::get<"room_id"_>(matching))
		unmap(room_id, at<"room_id"_>(matching));

	if(json::get<"sender"_>(matching))
		unmap(sender, at<"sender"_>(matching));

	if(json::get<"state_key"_>(matching))
		unmap(state_key, at<"state_key"_>(matching));

	if(json::get<"type"_>(matching))
		unmap(type, at<"type"_>(matching));

	return ret;
}

size_t
ircd::m::hook::maps::match(const event &event,
                           const std::function<bool (base &)> &callback)
const
{
	std::set<base *> matching
	{
		begin(always), end(always)
	};

	const auto site_match{[&matching]
	(auto &map, const string_view &key)
	{
		auto pit{map.equal_range(key)};
		for(; pit.first != pit.second; ++pit.first)
			matching.emplace(pit.first->second);
	}};

	if(json::get<"origin"_>(event))
		site_match(origin, at<"origin"_>(event));

	if(json::get<"room_id"_>(event))
		site_match(room_id, at<"room_id"_>(event));

	if(json::get<"sender"_>(event))
		site_match(sender, at<"sender"_>(event));

	if(json::get<"type"_>(event))
		site_match(type, at<"type"_>(event));

	if(json::get<"state_key"_>(event))
		site_match(state_key, at<"state_key"_>(event));

	auto it(begin(matching));
	while(it != end(matching))
	{
		const base &hook(**it);
		if(!_hook_match(hook.matching, event))
			it = matching.erase(it);
		else
			++it;
	}

	size_t ret{0};
	for(auto it(begin(matching)); it != end(matching); ++it, ++ret)
		if(!callback(**it))
			return ret;

	return ret;
}

//
// hook::base
//

/// Primary hook ctor
ircd::m::hook::base::base(const json::members &members)
:_feature
{
	_hook_make_feature(members)
}
,feature
{
	_feature
}
,matching
{
	feature
}
{
	site *site; try
	{
		if((site = find_site()))
			site->add(*this);
	}
	catch(...)
	{
		if(registered)
		{
			auto *const site(find_site());
			assert(site != nullptr);
			site->del(*this);
		}
	}
}

ircd::m::hook::base::~base()
noexcept
{
	if(!registered)
		return;

	auto *const site(find_site());
	assert(site != nullptr);
	assert(site->calling == 0);
	assert(calling == 0);
	site->del(*this);
}

ircd::m::hook::base::site *
ircd::m::hook::base::find_site()
const
{
	const auto &site_name
	{
		this->site_name()
	};

	if(!site_name)
		return nullptr;

	for(auto *const &site : m::hook::base::site::list)
		if(site->name() == site_name)
			return site;

	return nullptr;
}

ircd::string_view
ircd::m::hook::base::site_name()
const try
{
	return unquote(feature.at("_site"));
}
catch(const std::out_of_range &e)
{
	throw panic
	{
		"Hook %p must name a '_site' to register with.", this
	};
}

//
// hook::site
//

//
// hook::site::site
//

ircd::m::hook::base::site::site(const json::members &members)
:_feature
{
	members
}
,feature
{
	_feature
}
,maps
{
	std::make_unique<struct maps>()
}
,exceptions
{
	feature.get<bool>("exceptions", true)
}
{
	for(const auto &site : list)
		if(site->name() == name() && site != this)
			throw error
			{
				"Hook site '%s' already registered at %p",
				name(),
				site
			};

	// Find and register all of the orphan hooks which were constructed before
	// this site was constructed.
	for(auto *const &hook : m::hook::base::list)
		if(hook->site_name() == name())
			add(*hook);
}

ircd::m::hook::base::site::~site()
noexcept
{
	const std::vector<base *> hooks
	{
		begin(this->hooks), end(this->hooks)
	};

	for(auto *const hook : hooks)
		del(*hook);
}

void
ircd::m::hook::base::site::match(const event &event,
                                 const std::function<bool (base &)> &callback)
{
	maps->match(event, callback);
}

bool
ircd::m::hook::base::site::add(base &hook)
{
	assert(!hook.registered);
	assert(hook.site_name() == name());
	assert(hook.matchers == 0);

	if(!hooks.emplace(&hook).second)
	{
		log::warning
		{
			log, "Hook %p already registered to site %s",
			&hook,
			name()
		};

		return false;
	}

	assert(maps);
	const size_t matched
	{
		maps->add(hook, hook.matching)
	};

	hook.matchers = matched;
	hook.registered = true;
	matchers += matched;
	++count;

	log::debug
	{
		log, "Registered hook %p to site %s",
		&hook,
		name()
	};

	return true;
}

bool
ircd::m::hook::base::site::del(base &hook)
{
	log::debug
	{
		log, "Removing hook %p from site %s",
		&hook,
		name()
	};

	assert(hook.registered);
	assert(hook.site_name() == name());

	const size_t matched
	{
		maps->del(hook, hook.matching)
	};

	const auto erased
	{
		hooks.erase(&hook)
	};

	hook.matchers -= matched;
	hook.registered = false;
	matchers -= matched;
	--count;
	assert(hook.matchers == 0);
	assert(erased);
	return true;
}

ircd::string_view
ircd::m::hook::base::site::name()
const try
{
	return unquote(feature.at("name"));
}
catch(const std::out_of_range &e)
{
	throw panic
	{
		"Hook site %p requires a name", this
	};
}

//
// hook<void>
//

ircd::m::hook::hook<void>::hook(const json::members &feature,
                                decltype(function) function)
:base{feature}
,function{std::move(function)}
{
}

ircd::m::hook::hook<void>::hook(decltype(function) function,
                                const json::members &feature)
:base{feature}
,function{std::move(function)}
{
}

ircd::m::hook::site<void>::site(const json::members &feature)
:base::site{feature}
{
}

void
ircd::m::hook::site<void>::operator()(const event &event)
{
	match(event, [this, &event]
	(base &base)
	{
		call(dynamic_cast<hook<void> &>(base), event);
		return true;
	});
}

void
ircd::m::hook::site<void>::call(hook<void> &hfn,
                                const event &event)
try
{
	// stats for site
	++calls;
	const scope_count site_calling
	{
		calling
	};

	// stats for hook
	++hfn.calls;
	const scope_count hook_calling
	{
		hfn.calling
	};

	// call hook
	hfn.function(event);
}
catch(const std::exception &e)
{
	if(exceptions)
		throw;

	log::critical
	{
		log, "Unhandled hookfn(%p) %s error :%s",
		&hfn,
		string_view{hfn.feature},
		e.what()
	};
}

//
// hook internal
//

/// Internal interface which manipulates the initializer supplied by the
/// developer to the hook to create the proper JSON output. i.e They supply
/// a "room_id" of "!config" which has no hostname, that is added here
/// depending on my_host() in the deployment runtime...
///
ircd::json::strung
ircd::m::_hook_make_feature(const json::members &members)
{
	const ctx::critical_assertion ca;
	std::vector<json::member> copy
	{
		begin(members), end(members)
	};

	for(auto &member : copy) switch(hash(member.first))
	{
		case hash("room_id"):
			_hook_fix_room_id(members, member);
			continue;

		case hash("sender"):
			_hook_fix_sender(members, member);
			continue;

		case hash("state_key"):
			_hook_fix_state_key(members, member);
			continue;
	}

	return { copy.data(), copy.data() + copy.size() };
}

void
ircd::m::_hook_fix_sender(const json::members &members,
                          json::member &member)
{
	// Rewrite the sender if the supplied input has no hostname
	if(valid_local_only(id::USER, member.second))
	{
		assert(my_host());
		thread_local char buf[256];
		member.second = id::user { buf, member.second, my_host() };
	}

	validate(id::USER, member.second);
}

void
ircd::m::_hook_fix_room_id(const json::members &members,
                           json::member &member)
{
	// Rewrite the room_id if the supplied input has no hostname
	if(valid_local_only(id::ROOM, member.second))
	{
		assert(my_host());
		thread_local char buf[256];
		member.second = id::room { buf, member.second, my_host() };
	}

	validate(id::ROOM, member.second);
}

void
ircd::m::_hook_fix_state_key(const json::members &members,
                             json::member &member)
{
	const bool is_member_event
	{
		end(members) != std::find_if(begin(members), end(members), []
		(const auto &member)
		{
			return member.first == "type" && member.second == "m.room.member";
		})
	};

	if(!is_member_event)
		return;

	// Rewrite the sender if the supplied input has no hostname
	if(valid_local_only(id::USER, member.second))
	{
		assert(my_host());
		thread_local char buf[256];
		member.second = id::user { buf, member.second, my_host() };
	}

	validate(id::USER, member.second);
}

bool
ircd::m::_hook_match(const m::event &matching,
                     const m::event &event)
{
	if(json::get<"origin"_>(matching))
		if(at<"origin"_>(matching) != json::get<"origin"_>(event))
			return false;

	if(json::get<"room_id"_>(matching))
		if(at<"room_id"_>(matching) != json::get<"room_id"_>(event))
			return false;

	if(json::get<"sender"_>(matching))
		if(at<"sender"_>(matching) != json::get<"sender"_>(event))
			return false;

	if(json::get<"type"_>(matching))
		if(at<"type"_>(matching) != json::get<"type"_>(event))
			return false;

	if(json::get<"state_key"_>(matching))
		if(at<"state_key"_>(matching) != json::get<"state_key"_>(event))
			return false;

	if(membership(matching))
		if(membership(matching) != membership(event))
			return false;

	if(json::get<"content"_>(matching))
		if(json::get<"type"_>(event) == "m.room.message")
			if(at<"content"_>(matching).has("msgtype"))
				if(at<"content"_>(matching).get("msgtype") != json::get<"content"_>(event).get("msgtype"))
					return false;

	return true;
}

///////////////////////////////////////////////////////////////////////////////
//
// m/error.h
//

namespace ircd::m
{
	const std::array<http::header, 1> _error_headers
	{{
		{ "Content-Type", "application/json; charset=utf-8" },
	}};
}

thread_local
decltype(ircd::m::error::fmtbuf)
ircd::m::error::fmtbuf
{};

//
// error::error
//

ircd::m::error::error()
:error
{
	internal, http::INTERNAL_SERVER_ERROR, std::string{},
}
{}

ircd::m::error::error(std::string c)
:error
{
	internal, http::INTERNAL_SERVER_ERROR, std::move(c),
}
{}

ircd::m::error::error(const http::code &c)
:error
{
	internal, c, std::string{},
}
{}

ircd::m::error::error(const http::code &c,
                      const json::members &members)
:error
{
	internal, c, json::strung{members},
}
{}

ircd::m::error::error(const http::code &c,
                      const json::iov &iov)
:error
{
	internal, c, json::strung{iov},
}
{}

ircd::m::error::error(const http::code &c,
                      const json::object &object)
:error
{
	internal, c, json::strung{object},
}
{}

ircd::m::error::error(internal_t,
                      const http::code &c,
                      std::string object)
:http::error
{
	c, std::move(object), vector_view<const http::header>{_error_headers}
}
{
	if(!content.empty())
	{
		strlcat(ircd::exception::buf, " ");
		strlcat(ircd::exception::buf, errcode());
		strlcat(ircd::exception::buf, " :");
		strlcat(ircd::exception::buf, errstr());
	}
}

ircd::string_view
ircd::m::error::errstr()
const noexcept
{
	const json::object &content
	{
		this->http::error::content
	};

	return unquote(content.get("error"));
}

ircd::string_view
ircd::m::error::errcode()
const noexcept
{
	const json::object &content
	{
		this->http::error::content
	};

	return unquote(content.get("errcode", "M_UNKNOWN"_sv));
}
