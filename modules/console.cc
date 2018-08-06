// Matrix Construct
//
// Copyright (C) Matrix Construct Developers, Authors & Contributors
// Copyright (C) 2016-2018 Jason Volk <jason@zemos.net>
//
// Permission to use, copy, modify, and/or distribute this software for any
// purpose with or without fee is hereby granted, provided that the above
// copyright notice and this permission notice is present in all copies. The
// full license for this software is available in the LICENSE file.

#include <ircd/util/params.h>

using namespace ircd;

IRCD_EXCEPTION_HIDENAME(ircd::error, bad_command)

static void init_cmds();

mapi::header
IRCD_MODULE
{
	"IRCd terminal console: runtime-reloadable self-reflecting command library.", []
	{
		init_cmds();
	}
};

conf::item<seconds>
default_synapse
{
	{ "name",     "ircd.console.timeout" },
	{ "default",  45L                    },
};

/// The first parameter for all commands. This aggregates general options
/// passed to commands as well as providing the output facility with an
/// ostream interface. Commands should only send output to this object. The
/// command's input line is not included here; it's the second param to a cmd.
struct opt
{
	std::ostream &out;
	bool html {false};
	seconds timeout {default_synapse};
	string_view special;

	operator std::ostream &()
	{
		return out;
	}

	template<class T> auto &operator<<(T&& t)
	{
		out << std::forward<T>(t);
		return out;
	}

	auto &operator<<(std::ostream &(*manip)(std::ostream &))
	{
		return manip(out);
	}
};

/// Instances of this object are generated when this module reads its
/// symbols to find commands. These instances are then stored in the
/// cmds set for lookup and iteration.
struct cmd
{
	using is_transparent = void;

	static constexpr const size_t &PATH_MAX
	{
		8
	};

	std::string name;
	std::string symbol;
	mods::sym_ptr ptr;

	bool operator()(const cmd &a, const cmd &b) const
	{
		return a.name < b.name;
	}

	bool operator()(const string_view &a, const cmd &b) const
	{
		return a < b.name;
	}

	bool operator()(const cmd &a, const string_view &b) const
	{
		return a.name < b;
	}

	cmd(std::string name, std::string symbol)
	:name{std::move(name)}
	,symbol{std::move(symbol)}
	,ptr{IRCD_MODULE, this->symbol}
	{}

	cmd() = default;
	cmd(cmd &&) = delete;
	cmd(const cmd &) = delete;
};

std::set<cmd, cmd>
cmds;

void
init_cmds()
{
	auto symbols
	{
		mods::symbols(mods::path(IRCD_MODULE))
	};

	for(std::string &symbol : symbols)
	{
		// elide lots of grief by informally finding this first
		if(!has(symbol, "console_cmd"))
			continue;

		thread_local char buf[1024];
		const string_view demangled
		{
			demangle(buf, symbol)
		};

		std::string command
		{
			replace(between(demangled, "__", "("), "__", " ")
		};

		const auto iit
		{
			cmds.emplace(std::move(command), std::move(symbol))
		};

		if(!iit.second)
			throw error
			{
				"Command '%s' already exists", command
			};
	}
}

const cmd *
find_cmd(const string_view &line)
{
	const size_t elems
	{
		std::min(token_count(line, ' '), cmd::PATH_MAX)
	};

	for(size_t e(elems+1); e > 0; --e)
	{
		const auto name
		{
			tokens_before(line, ' ', e)
		};

		const auto it{cmds.lower_bound(name)};
		if(it == end(cmds) || it->name != name)
			continue;

		return &(*it);
	}

	return nullptr;
}

//
// Main command dispatch
//

/// This function may be linked and called by those wishing to execute a
/// command. Output from the command will be appended to the provided ostream.
/// The input to the command is passed in `line`. Since `struct opt` is not
/// accessible outside of this module, all public options are passed via a
/// plaintext string which is parsed here.
extern "C" int
console_command(std::ostream &out,
                const string_view &line,
                const string_view &opts)
try
{
	opt opt
	{
		out,
		has(opts, "html")
	};

	const cmd *const cmd
	{
		find_cmd(line)
	};

	if(!cmd)
		return true;

	const auto args
	{
		lstrip(split(line, cmd->name).second, ' ')
	};

	const auto &ptr{cmd->ptr};
	using prototype = bool (struct opt &, const string_view &);
	return ptr.operator()<prototype>(opt, args);
}
catch(const params::error &e)
{
	out << e.what() << std::endl;
	return true;
}
catch(const bad_command &e)
{
	return -2;
}

//
// Help
//

bool
console_cmd__help(opt &out, const string_view &line)
{
	const auto cmd
	{
		find_cmd(line)
	};

	if(cmd)
	{
		out << "No help available for '" << cmd->name << "'."
		    << std::endl;

		//TODO: help string symbol map
	}

	out << "Commands available: \n"
	    << std::endl;

	const size_t elems
	{
		std::min(token_count(line, ' '), cmd::PATH_MAX)
	};

	for(size_t e(elems+1); e > 0; --e)
	{
		const auto name
		{
			tokens_before(line, ' ', e)
		};

		string_view last;
		auto it{cmds.lower_bound(name)};
		if(it == end(cmds))
			continue;

		for(; it != end(cmds); ++it)
		{
			if(!startswith(it->name, name))
				break;

			const auto prefix
			{
				tokens_before(it->name, ' ', e)
			};

			if(last == prefix)
				continue;

			last = prefix;
			const auto suffix
			{
				e > 1? tokens_after(prefix, ' ', e - 2) : prefix
			};

			if(empty(suffix))
				continue;

			out << suffix << std::endl;
		}

		break;
	}

	return true;
}

//
// Test trigger stub
//

bool
console_cmd__test(opt &out, const string_view &line)
{
	return true;
}

//
// misc
//

bool
console_cmd__debug(opt &out, const string_view &line)
{
	if(!RB_DEBUG_LEVEL)
	{
		out << "Debugging is not compiled in." << std::endl;
		return true;
	}

	if(log::console_enabled(log::DEBUG))
	{
		out << "Turning off debuglog..." << std::endl;
		log::console_disable(log::DEBUG);
		return true;
	} else {
		out << "Turning on debuglog..." << std::endl;
		log::console_enable(log::DEBUG);
		return true;
	}
}

//
// log
//

bool
console_cmd__log(opt &out, const string_view &line)
{
	for(const auto *const &log : log::log::list)
		out << (log->snote? log->snote : '-')
		    << " " << std::setw(8) << std::left << log->name
		    << " "
		    << (log->fmasked? " FILE" : "")
		    << (log->cmasked? " CONSOLE" : "")
		    << std::endl;

	return true;
}

bool
console_cmd__log__level(opt &out, const string_view &line)
{
	const params param{line, " ",
	{
		"level",
	}};

	if(!param.count())
	{
		for(int i(0); i < num_of<log::facility>(); ++i)
			if(i > RB_LOG_LEVEL)
				out << "[\033[1;40m-\033[0m]: " << reflect(log::facility(i)) << std::endl;
			else if(console_enabled(log::facility(i)))
				out << "[\033[1;42m+\033[0m]: " << reflect(log::facility(i)) << std::endl;
			else
				out << "[\033[1;41m-\033[0m]: " << reflect(log::facility(i)) << std::endl;

		return true;
	}

	const int level
	{
		param.at<int>(0)
	};

	for(int i(0); i < num_of<log::facility>(); ++i)
		if(i > RB_LOG_LEVEL)
		{
			out << "[\033[1;40m-\033[0m]: " << reflect(log::facility(i)) << std::endl;
		}
		else if(i <= level)
		{
			console_enable(log::facility(i));
			out << "[\033[1;42m+\033[0m]: " << reflect(log::facility(i)) << std::endl;
		} else {
			console_disable(log::facility(i));
			out << "[\033[1;41m-\033[0m]: " << reflect(log::facility(i)) << std::endl;
		}

	return true;
}

bool
console_cmd__log__mask(opt &out, const string_view &line)
{
	thread_local string_view list[64];
	const auto &count
	{
		tokens(line, ' ', list)
	};

	log::console_mask({list, count});
	return true;
}

bool
console_cmd__log__unmask(opt &out, const string_view &line)
{
	thread_local string_view list[64];
	const auto &count
	{
		tokens(line, ' ', list)
	};

	log::console_unmask({list, count});
	return true;
}

bool
console_cmd__log__mark(opt &out, const string_view &line)
{
	const string_view &msg
	{
		empty(line)?
			"marked by console":
			line
	};

	log::mark
	{
		msg
	};

	out << "The log files were marked with '" << msg
	    << "'"
	    << std::endl;

	return true;
}

bool
console_cmd__mark(opt &out, const string_view &line)
{
	return console_cmd__log__mark(out, line);
}

//
// info
//

bool
console_cmd__info(opt &out, const string_view &line)
{
	info::dump();

	out << "Daemon information was written to the log."
	    << std::endl;

	return true;
}

//
// mem
//

bool
console_cmd__mem(opt &out, const string_view &line)
{
	auto &this_thread
	{
		ircd::allocator::profile::this_thread
	};

	out << "IRCd thread allocations:" << std::endl
	    << "alloc count:  " << this_thread.alloc_count << std::endl
	    << "freed count:  " << this_thread.free_count << std::endl
	    << "alloc bytes:  " << this_thread.alloc_bytes << std::endl
	    << "freed bytes:  " << this_thread.free_bytes << std::endl
	    << std::endl;

	thread_local char buf[1024];
	out << "malloc() information:" << std::endl
	    << allocator::info(buf) << std::endl
	    ;

	return true;
}

//
// conf
//

bool
console_cmd__conf__list(opt &out, const string_view &line)
{
	thread_local char val[4_KiB];
	for(const auto &item : conf::items)
		out
		<< std::setw(48) << std::left << std::setfill('_') << item.first
		<< " " << item.second->get(val)
		<< std::endl;

	return true;
}

bool
console_cmd__conf(opt &out, const string_view &line)
{
	return console_cmd__conf__list(out, line);
}

bool
console_cmd__conf__set(opt &out, const string_view &line)
{
	const params param{line, " ",
	{
		"key", "value"
	}};

	const auto &key
	{
		param.at(0)
	};

	const auto &val
	{
		param.at(1)
	};
/*
	using prototype = m::event::id::buf (const m::user::id &,
	                                     const string_view &key,
	                                     const string_view &val);

	static m::import<prototype> set_conf_item
	{
		"s_conf", "set_conf_item"
	};

	const auto event_id
	{
		set_conf_item(m::me, key, val)
	};

	out << event_id << " <- " << key << " = " << val << std::endl;
*/
	return true;
}

bool
console_cmd__conf__get(opt &out, const string_view &line)
{
	const params param{line, " ",
	{
		"key"
	}};

	const auto &key
	{
		param.at(0)
	};

	thread_local char val[4_KiB];
	for(const auto &item : conf::items)
	{
		if(item.first != key)
			continue;

		out << std::setw(48) << std::right << item.first
		    << " = " << item.second->get(val)
		    << std::endl;

		return true;
	}

	throw error
	{
		"Conf item '%s' not found", key
	};
}

//
// mod
//

bool
console_cmd__mod(opt &out, const string_view &line)
{
	auto avflist(mods::available());
	const auto b(std::make_move_iterator(begin(avflist)));
	const auto e(std::make_move_iterator(end(avflist)));
	std::vector<std::string> available(b, e);
	std::sort(begin(available), end(available));

	for(const auto &mod : available)
	{
		const auto loadstr
		{
			mods::loaded(mod)? "\033[1;32;42m+\033[0m" : " "
		};

		out << "[" << loadstr << "] " << mod << std::endl;
	}

	return true;
}

bool
console_cmd__mod__path(opt &out, const string_view &line)
{
	for(const auto &path : ircd::mods::paths)
		out << path << std::endl;

	return true;
}

bool
console_cmd__mod__syms(opt &out, const string_view &line)
{
	const std::string path
	{
		token(line, ' ', 0)
	};

	const std::vector<std::string> symbols
	{
		mods::symbols(path)
	};

	for(const auto &sym : symbols)
		out << sym << std::endl;

	out << " -- " << symbols.size() << " symbols in " << path << std::endl;
	return true;
}

bool
console_cmd__mod__reload(opt &out, const string_view &line)
{
	const auto names
	{
		tokens<std::vector>(line, ' ')
	};

	for(auto it(names.begin()); it != names.end(); ++it)
	{
		const auto &name{*it};
/*
		if(m::modules.erase(std::string{name}))
			out << name << " unloaded." << std::endl;
		else
			out << name << " is not loaded." << std::endl;
*/
	}

	for(auto it(names.rbegin()); it != names.rend(); ++it)
	{
		const auto &name{*it};
/*
		if(m::modules.emplace(std::string{name}, name).second)
			out << name << " loaded." << std::endl;
		else
			out << name << " is already loaded." << std::endl;
*/
	}

	return true;
}

bool
console_cmd__mod__load(opt &out, const string_view &line)
{
	tokens(line, ' ', [&out]
	(const string_view &name)
	{
		ircd::module mod
		{
			name
		};
/*
		if(m::modules.find(name) != end(m::modules))
		{
			out << name << " is already loaded." << std::endl;
			return;
		}

		m::modules.emplace(std::string{name}, name);
*/
		out << name << " loaded." << std::endl;
	});

	return true;
}

bool
console_cmd__mod__unload(opt &out, const string_view &line)
{
	tokens(line, ' ', [&out]
	(const string_view &name)
	{
/*
		if(!m::modules.erase(std::string{name}))
		{
			out << name << " is not loaded." << std::endl;
			return;
		}
*/
		out << "unloaded " << name << std::endl;
	});

	return true;
}

//
// ctx
//

bool
console_cmd__ctx__interrupt(opt &out, const string_view &line)
{
	const params param{line, " ",
	{
		"id", "[id]..."
	}};

	for(size_t i(0); i < param.count(); ++i)
		for(auto *const &ctx : ctx::ctxs)
			if(id(*ctx) == param.at<uint64_t>(i))
			{
				interrupt(*ctx);
				break;
			}

	return true;
}

bool
console_cmd__ctx__term(opt &out, const string_view &line)
{
	const params param{line, " ",
	{
		"id", "[id]..."
	}};

	for(size_t i(0); i < param.count(); ++i)
		for(auto *const &ctx : ctx::ctxs)
			if(id(*ctx) == param.at<uint64_t>(i))
			{
				terminate(*ctx);
				break;
			}

	return true;
}

bool
console_cmd__ctx__list(opt &out, const string_view &line)
{
	out << "   "
	    << "ID"
	    << "   "
	    << "STATE"
	    << "   "
	    << "YIELDS"
	    << "      "
	    << "CYCLE COUNT"
	    << "     "
	    << "PCT"
	    << "     "
	    << "STACK "
	    << "   "
	    << "LIMIT"
	    << "     "
	    << "PCT"
	    << "   "
	    << ":NAME"
	    << std::endl;

	for(const auto *const &ctxp : ctx::ctxs)
	{
		const auto &ctx{*ctxp};
		out << std::setw(5) << std::right << id(ctx);
		out << "  "
		    << (started(ctx)? 'S' : '-')
		    << (running(ctx)? 'R' : '-')
		    << (waiting(ctx)? 'W' : '-')
		    << (finished(ctx)? 'F' : '-')
		    << (interruptible(ctx)? '-' : 'N')
		    << (interruption(ctx)? 'I' : '-')
		    << (termination(ctx)? 'T' : '-')
		    ;

		out << " "
		    << std::setw(8) << std::right << yields(ctx)
		    << " ";

		out << " "
		    << std::setw(15) << std::right << cycles(ctx)
		    << " ";

		const long double total_cyc(ctx::prof::total_slice_cycles());
		const auto tsc_pct
		{
			total_cyc > 0.0? (cycles(ctx) / total_cyc) : 0.0L
		};

		out << " "
		    << std::setw(5) << std::right << std::fixed << std::setprecision(2) << (tsc_pct * 100)
		    << "% ";

		out << "  "
		    << std::setw(7) << std::right << stack_at(ctx)
		    << " ";

		out << " "
		    << std::setw(7) << std::right << stack_max(ctx)
		    << " ";

		const auto stack_pct
		{
			stack_at(ctx) / (long double)stack_max(ctx)
		};

		out << " "
		    << std::setw(5) << std::right << std::fixed << std::setprecision(2) << (stack_pct * 100)
		    << "% ";

		out << "  :"
		    << name(ctx);

		out << std::endl;
	}

	return true;
}

bool
console_cmd__ctx(opt &out, const string_view &line)
{
	if(empty(line))
		return console_cmd__ctx__list(out, line);

	return true;
}

//
// db
//

bool
console_cmd__db__sync(opt &out, const string_view &line)
try
{
	const params param{line, " ",
	{
		"dbname",
	}};

	const auto dbname
	{
		param.at(0)
	};

	auto &database
	{
		db::database::get(dbname)
	};

	sync(database);
	out << "done" << std::endl;
	return true;
}
catch(const std::out_of_range &e)
{
	out << "No open database by that name" << std::endl;
	return true;
}

bool
console_cmd__db__flush(opt &out, const string_view &line)
try
{
	const params param{line, " ",
	{
		"dbname", "[sync]"
	}};

	const auto dbname
	{
		param.at(0)
	};

	const auto sync
	{
		param.at(1, false)
	};

	auto &database
	{
		db::database::get(dbname)
	};

	flush(database, sync);
	out << "done" << std::endl;
	return true;
}
catch(const std::out_of_range &e)
{
	out << "No open database by that name" << std::endl;
	return true;
}

bool
console_cmd__db__sort(opt &out, const string_view &line)
try
{
	const params param{line, " ",
	{
		"dbname", "[blocking]"
	}};

	const auto dbname
	{
		param.at(0)
	};

	const auto blocking
	{
		param.at(1, false)
	};

	auto &database
	{
		db::database::get(dbname)
	};

	sort(database, blocking);
	out << "done" << std::endl;
	return true;
}
catch(const std::out_of_range &e)
{
	out << "No open database by that name" << std::endl;
	return true;
}

bool
console_cmd__db__compact(opt &out, const string_view &line)
try
{
	const params param{line, " ",
	{
		"dbname", "[colname]", "[begin]", "[end]", "[level]"
	}};

	const auto dbname
	{
		param.at(0)
	};

	const auto colname
	{
		param[1]
	};

	const auto begin
	{
		param[2]
	};

	const auto end
	{
		param[3]
	};

	const auto level
	{
		param.at(4, -1)
	};

	auto &database
	{
		db::database::get(dbname)
	};

	if(!colname)
	{
		compact(database);
		out << "done" << std::endl;
		return true;
	}

	db::column column
	{
		database, colname
	};

	const bool integer
	{
		begin? try_lex_cast<uint64_t>(begin) : false
	};

	const std::pair<string_view, string_view> range
	{
		integer? byte_view<string_view>(lex_cast<uint64_t>(begin)) : begin,
		integer && end? byte_view<string_view>(lex_cast<uint64_t>(end)) : end,
	};

	compact(column, range, level);
	compact(column, level);
	out << "done" << std::endl;
	return true;
}
catch(const std::out_of_range &e)
{
	out << "No open database by that name" << std::endl;
	return true;
}

bool
console_cmd__db__ticker(opt &out, const string_view &line)
try
{
	const params param{line, " ",
	{
		"dbname", "[ticker]"
	}};

	const auto dbname
	{
		param.at(0)
	};

	const auto ticker
	{
		param[1]
	};

	auto &database
	{
		db::database::get(dbname)
	};

	// Special branch for integer properties that RocksDB aggregates.
	if(ticker && ticker != "-a")
	{
		out << ticker << ": " << db::ticker(database, ticker) << std::endl;
		return true;
	}

	for(uint32_t i(0); i < db::ticker_max; ++i)
	{
		const string_view &name
		{
			db::ticker_id(i)
		};

		if(!name)
			continue;

		const auto &val
		{
			db::ticker(database, i)
		};

		if(val == 0 && ticker != "-a")
			continue;

		out << std::left << std::setw(48) << std::setfill('_') << name
		    << "  " << val
		    << std::endl;
	}

	return true;
}
catch(const std::out_of_range &e)
{
	out << "No open database by that name" << std::endl;
	return true;
}

bool
console_cmd__db__io(opt &out, const string_view &line)
{
	const auto &ic
	{
		db::iostats_current()
	};

	out << db::string(ic) << std::endl;
	return true;
}

bool
console_cmd__db__perf(opt &out, const string_view &line)
{
	const auto &pc
	{
		db::perf_current()
	};

	out << db::string(pc) << std::endl;
	return true;
}

bool
console_cmd__db__perf__level(opt &out, const string_view &line)
{
	const params param{line, " ",
	{
		"[level]"
	}};

	if(!param.count())
	{
		const auto &level
		{
			db::perf_level()
		};

		out << "Current level is: " << level << std::endl;
		return true;
	}

	const auto &level
	{
		param.at<uint>(0)
	};

	db::perf_level(level);
	out << "Set level to: " << level << std::endl;
	return true;
}

bool
console_cmd__db__prop(opt &out, const string_view &line)
try
{
	const params param{line, " ",
	{
		"dbname", "column", "property"
	}};

	const auto dbname
	{
		param.at(0)
	};

	const auto colname
	{
		param.at(1, "*"_sv)
	};

	const auto property
	{
		param.at(2)
	};

	auto &database
	{
		db::database::get(dbname)
	};

	// Special branch for integer properties that RocksDB aggregates.
	if(colname == "*")
	{
		const uint64_t value
		{
			db::property(database, property)
		};

		out << value << std::endl;
		return true;
	}

	const auto query{[&out, &database, &property]
	(const string_view &colname)
	{
		const db::column column
		{
			database, colname
		};

		const auto value
		{
			db::property<db::prop_map>(column, property)
		};

		for(const auto &p : value)
			out << p.first << " : " << p.second << std::endl;
	}};

	// Branch for querying the property for a single column
	if(colname != "**")
	{
		query(colname);
		return true;
	}

	// Querying the property for all columns in a loop
	for(const auto &column_name : database.column_names)
	{
		out << std::setw(16) << std::right << column_name << " : ";
		query(column_name);
	}

	return true;
}
catch(const std::out_of_range &e)
{
	out << "No open database by that name" << std::endl;
	return true;
}

bool
console_cmd__db__cache(opt &out, const string_view &line)
try
{
	const params param{line, " ",
	{
		"dbname", "column",
	}};

	const auto dbname
	{
		param.at(0)
	};

	const auto colname
	{
		param[1]
	};

	auto &database
	{
		db::database::get(dbname)
	};

	if(!colname)
	{
		const auto usage(db::usage(cache(database)));
		const auto capacity(db::capacity(cache(database)));
		const auto usage_pct
		{
			capacity > 0.0? (double(usage) / double(capacity)) : 0.0L
		};

		out << "(row cache) "
		    << std::setw(9) << usage
		    << " "
		    << std::setw(9) << capacity
		    << " "
		    << std::setw(5) << std::right << std::fixed << std::setprecision(2) << (usage_pct * 100)
		    << "%"
		    << std::endl;

		return true;
	}

	out << std::left
	    << std::setw(16) << "COLUMN"
	    << std::right
	    << " "
	    << std::setw(9) << "CACHED"
	    << " "
	    << std::setw(9) << "CAPACITY"
	    << " "
	    << std::setw(5) << "   PCT"
	    << " "
	    << std::setw(9) << " COMPRESS"
	    << " "
	    << std::setw(9) << "CAPACITY"
	    << " "
	    << std::setw(5) << "   PCT"
	    << " "
	    << std::endl;

	const auto output{[&out]
	(const string_view &column_name,
	 const size_t &usage, const size_t &capacity,
	 const size_t usage_comp, const size_t &capacity_comp,
	 const double &usage_pct, const double &usage_comp_pct)
	{
		out << std::setw(16) << std::left << column_name
		    << " "
		    << std::right
		    << std::setw(9) << usage
		    << " "
		    << std::setw(9) << capacity
		    << " "
		    << std::setw(5) << std::right << std::fixed << std::setprecision(2) << (usage_pct * 100)
		    << "% "
		    << std::setw(9) << usage_comp
		    << " "
		    << std::setw(9) << capacity_comp
		    << " "
		    << std::setw(5) << std::right << std::fixed << std::setprecision(2) << (usage_comp_pct * 100)
		    << "%"
		    << std::endl;
	}};

	const auto query{[&output, &database]
	(const string_view &colname)
	{
		const db::column column
		{
			database, colname
		};

		const auto usage(db::usage(cache(column)));
		const auto capacity(db::capacity(cache(column)));
		const auto usage_pct
		{
			capacity > 0.0? (double(usage) / double(capacity)) : 0.0L
		};

		const auto usage_comp(db::usage(cache_compressed(column)));
		const auto capacity_comp(db::capacity(cache_compressed(column)));
		const auto usage_comp_pct
		{
			capacity_comp > 0.0? (double(usage_comp) / double(capacity_comp)) : 0.0L
		};

		output(colname, usage, capacity, usage_comp, capacity_comp, usage_pct, usage_comp_pct);
	}};

	// Querying the totals for all caches for all columns in a loop
	if(colname == "*")
	{
		size_t usage(0), usage_comp(0);
		size_t capacity(0), capacity_comp(0);
		for(const auto &column : database.columns)
		{
			const db::column c(*column);
			usage += db::usage(cache(c));
			capacity += db::capacity(cache(c));
			usage_comp += db::usage(cache_compressed(c));
			capacity_comp += db::capacity(cache_compressed(c));
		}

		const auto usage_pct
		{
			capacity > 0.0? (double(usage) / double(capacity)) : 0.0L
		};

		const auto usage_comp_pct
		{
			capacity_comp > 0.0? (double(usage_comp) / double(capacity_comp)) : 0.0L
		};

		output("*", usage, capacity, usage_comp, capacity_comp, usage_pct, usage_comp_pct);
		return true;
	}

	// Query the cache for a single column
	if(colname != "**")
	{
		query(colname);
		return true;
	}

	// Querying the cache for all columns in a loop
	for(const auto &column_name : database.column_names)
		query(column_name);

	return true;
}
catch(const std::out_of_range &e)
{
	out << "No open database by that name" << std::endl;
	return true;
}

bool
console_cmd__db__cache__clear(opt &out, const string_view &line)
try
{
	const params param{line, " ",
	{
		"dbname", "column"
	}};

	const auto dbname
	{
		param.at(0)
	};

	const auto colname
	{
		param[1]
	};

	auto &database
	{
		db::database::get(dbname)
	};

	const auto clear{[&out, &database]
	(const string_view &colname)
	{
		db::column column
		{
			database, colname
		};

		db::clear(cache(column));
		db::clear(cache_compressed(column));
		out << "Cleared caches for '" << name(database) << "' '" << colname << "'"
		    << std::endl;
	}};

	if(!colname || colname == "**")
	{
		for(const auto &colname : database.column_names)
			clear(colname);

		return true;
	}

	clear(colname);
	return true;
}
catch(const std::out_of_range &e)
{
	out << "No open database by that name" << std::endl;
	return true;
}

bool
console_cmd__db__stats(opt &out, const string_view &line)
{
	const params param{line, " ",
	{
		"dbname", "column"
	}};

	return console_cmd__db__prop(out, fmt::snstringf
	{
		1024, "%s %s rocksdb.stats",
		param.at(0),
		param.at(1)
	});
}

bool
console_cmd__db__set(opt &out, const string_view &line)
try
{
	const params param{line, " ",
	{
		"dbname", "column", "option", "value"
	}};

	const auto dbname
	{
		param.at(0)
	};

	const auto colname
	{
		param.at(1, "*"_sv)
	};

	const auto option
	{
		param.at(2)
	};

	const auto value
	{
		param.at(3)
	};

	auto &database
	{
		db::database::get(dbname)
	};

	// Special branch for DBOptions
	if(colname == "*")
	{
		db::setopt(database, option, value);
		out << "done" << std::endl;
		return true;
	}

	const auto setopt{[&out, &database, &option, &value]
	(const string_view &colname)
	{
		db::column column
		{
			database, colname
		};

		db::setopt(column, option, value);
		out << colname << " :done" << std::endl;
	}};

	// Branch for querying the property for a single column
	if(colname != "**")
	{
		setopt(colname);
		return true;
	}

	// Querying the property for all columns in a loop
	for(const auto &colname : database.column_names)
		setopt(colname);

	return true;
}
catch(const std::out_of_range &e)
{
	out << "No open database by that name" << std::endl;
	return true;
}

bool
console_cmd__db__files(opt &out, const string_view &line)
try
{
	const auto dbname
	{
		token(line, ' ', 0)
	};

	auto &database
	{
		db::database::get(dbname)
	};

	uint64_t msz;
	const auto files
	{
		db::files(database, msz)
	};

	for(const auto &file : files)
		out << file << std::endl;

	out << "-- " << files.size() << " files; "
	    << "manifest is " << msz << " bytes.";

	return true;
}
catch(const std::out_of_range &e)
{
	out << "No open database by that name" << std::endl;
	return true;
}

bool
console_cmd__db__bytes(opt &out, const string_view &line)
try
{
	const params param{line, " ",
	{
		"dbname", "column"
	}};

	auto &database
	{
		db::database::get(param.at(0))
	};

	if(!param[1] || param[1] == "*")
	{
		const auto bytes
		{
			db::bytes(database)
		};

		out << bytes << std::endl;
		return true;
	}

	const auto query{[&out, &database]
	(const string_view &colname)
	{
		const db::column column
		{
			database, colname
		};

		const auto value
		{
			db::bytes(column)
		};

		out << std::setw(16) << std::right << colname
		    << " : " << value
		    << std::endl;
	}};

	if(param[1] != "**")
	{
		query(param.at(1));
		return true;
	}

	for(const auto &colname : database.column_names)
		query(colname);

	return true;
}
catch(const std::out_of_range &e)
{
	out << "No open database by that name" << std::endl;
	return true;
}

bool
console_cmd__db__txns(opt &out, const string_view &line)
try
{
	const auto dbname
	{
		token(line, ' ', 0)
	};

	if(dbname != "events")
		throw error
		{
			"Sorry, this command is specific to the events db for now."
		};

	const auto seqnum
	{
		lex_cast<uint64_t>(token(line, ' ', 1, "0"))
	};

	auto limit
	{
		lex_cast<size_t>(token(line, ' ', 2, "32"))
	};

	auto &database
	{
		db::database::get(dbname)
	};

	for_each(database, seqnum, db::seq_closure_bool{[&out, &limit]
	(db::txn &txn, const uint64_t &seqnum) -> bool
	{
/*
		m::event::id::buf event_id;
		txn.get(db::op::SET, "event_id", [&event_id]
		(const db::delta &delta)
		{
			event_id = std::get<delta.KEY>(delta);
		});

		if(event_id)
			return true;

		out << std::setw(12) << std::right << seqnum << " : "
		    << string_view{event_id}
		    << std::endl;
*/
		return --limit;
	}});

	return true;
}
catch(const std::out_of_range &e)
{
	out << "No open database by that name" << std::endl;
	return true;
}

bool
console_cmd__db__txn(opt &out, const string_view &line)
try
{
	const auto dbname
	{
		token(line, ' ', 0)
	};

	if(dbname != "events")
		throw error
		{
			"Sorry, this command is specific to the events db for now."
		};

	const auto seqnum
	{
		lex_cast<uint64_t>(token(line, ' ', 1, "0"))
	};

	auto &database
	{
		db::database::get(dbname)
	};

	get(database, seqnum, db::seq_closure{[&out]
	(db::txn &txn, const uint64_t &seqnum)
	{
		for_each(txn, [&out, &seqnum]
		(const db::delta &delta)
		{
			out << std::setw(12)  << std::right  << seqnum << " : "
			    << std::setw(8)   << std::left   << reflect(std::get<delta.OP>(delta)) << " "
			    << std::setw(18)  << std::right  << std::get<delta.COL>(delta) << " "
			    << std::get<delta.KEY>(delta)
			    << std::endl;
		});
	}});

	return true;
}
catch(const std::out_of_range &e)
{
	out << "No open database by that name" << std::endl;
	return true;
}

bool
console_cmd__db__checkpoint(opt &out, const string_view &line)
try
{
	const auto dbname
	{
		token(line, ' ', 0)
	};

	auto &database
	{
		db::database::get(dbname)
	};

	const auto seqnum
	{
		checkpoint(database)
	};

	out << "Checkpoint " << name(database)
	    << " at sequence " << seqnum << " complete."
	    << std::endl;

	return true;
}
catch(const std::out_of_range &e)
{
	out << "No open database by that name" << std::endl;
	return true;
}

bool
console_cmd__db__check(opt &out, const string_view &line)
try
{
	const auto dbname
	{
		token(line, ' ', 0)
	};

	auto &database
	{
		db::database::get(dbname)
	};

	check(database);
	out << "Check of " << dbname << " completed without error."
	    << std::endl;

	return true;
}
catch(const std::out_of_range &e)
{
	out << "No open database by that name" << std::endl;
	return true;
}

bool
console_cmd__db__list(opt &out, const string_view &line)
{
	const auto available
	{
		db::available()
	};

	for(const auto &path : available)
	{
		const auto name
		{
			replace(lstrip(lstrip(path, fs::DBPATH), '/'), "/", ":")
		};

		const auto &d
		{
			db::database::get(std::nothrow, name)
		};

		const auto &light
		{
			d? "\033[1;42m \033[0m" : " "
		};

		out << "[" << light << "]"
		    << " " << name
		    << " `" << path << "'"
		    << std::endl;
	}

	return true;
}

bool
console_cmd__db(opt &out, const string_view &line)
try
{
	if(empty(line))
		return console_cmd__db__list(out, line);

	const params param{line, " ",
	{
		"dbname"
	}};

	auto &database
	{
		db::database::get(param.at(0))
	};

	out << std::left << std::setw(28) << std::setfill('_') << "UUID "
	    << " " << uuid(database)
	    << std::endl;

	out << std::left << std::setw(28) << std::setfill('_') << "errors "
	    << " " << db::property(database, "rocksdb.background-errors")
	    << std::endl;

	out << std::left << std::setw(28) << std::setfill('_') << "columns "
	    << " " << database.columns.size()
	    << std::endl;

	out << std::left << std::setw(28) << std::setfill('_') << "files "
	    << " " << file_count(database)
	    << std::endl;

	out << std::left << std::setw(28) << std::setfill('_') << "sequence "
	    << " " << sequence(database)
	    << std::endl;

	out << std::left << std::setw(28) << std::setfill('_') << "keys "
	    << " " << db::property(database, "rocksdb.estimate-num-keys")
	    << std::endl;

	out << std::left << std::setw(28) << std::setfill('_') << "size "
	    << " " << bytes(database)
	    << std::endl;

	out << std::left << std::setw(28) << std::setfill('_') << "row cache size "
	    << " " << db::usage(cache(database))
	    << std::endl;

	out << std::left << std::setw(28) << std::setfill('_') << "live data size "
	    << " " << db::property(database, "rocksdb.estimate-live-data-size")
	    << std::endl;

	out << std::left << std::setw(28) << std::setfill('_') << "all tables size "
	    << " " << db::property(database, "rocksdb.size-all-mem-tables")
	    << std::endl;

	out << std::left << std::setw(28) << std::setfill('_') << "active table size "
	    << " " << db::property(database, "rocksdb.cur-size-active-mem-table")
	    << std::endl;

	out << std::left << std::setw(28) << std::setfill('_') << "active table entries "
	    << " " << db::property(database, "rocksdb.num-entries-active-mem-table")
	    << std::endl;

	out << std::left << std::setw(28) << std::setfill('_') << "active table deletes "
	    << " " << db::property(database, "rocksdb.num-deletes-active-mem-table")
	    << std::endl;

	out << std::left << std::setw(28) << std::setfill('_') << "lsm sequence "
	    << " " << db::property(database, "rocksdb.current-super-version-number")
	    << std::endl;

	return true;
}
catch(const std::out_of_range &e)
{
	out << "No open database by that name" << std::endl;
	return true;
}

//
// peer
//

static bool
html__peer(opt &out, const string_view &line)
{
	out << "<table>";

	out << "<tr>";
	out << "<td> HOST </td>";
	out << "<td> ADDR </td>";
	out << "<td> LINKS </td>";
	out << "<td> REQS </td>";
	out << "<td> ▲ BYTES Q</td>";
	out << "<td> ▼ BYTES Q</td>";
	out << "<td> ▲ BYTES</td>";
	out << "<td> ▼ BYTES</td>";
	out << "<td> ERROR </td>";
	out << "</tr>";

	for(const auto &p : server::peers)
	{
		using std::setw;
		using std::left;
		using std::right;

		const auto &host{p.first};
		const auto &peer{*p.second};
		const net::ipport &ipp{peer.remote};

		out << "<tr>";

		out << "<td>" << host << "</td>";
		out << "<td>" << ipp << "</td>";

		out << "<td>" << peer.link_count() << "</td>";
		out << "<td>" << peer.tag_count() << "</td>";
		out << "<td>" << peer.write_size() << "</td>";
		out << "<td>" << peer.read_size() << "</td>";
		out << "<td>" << peer.write_total() << "</td>";
		out << "<td>" << peer.read_total() << "</td>";

		out << "<td>";
		if(peer.err_has() && peer.err_msg())
			out << peer.err_msg();
		else if(peer.err_has())
			out << "<unknown error>"_sv;
		out << "</td>";

		out << "</tr>";
	}

	out << "</table>";
	return true;
}

bool
console_cmd__peer(opt &out, const string_view &line)
{
	if(out.html)
		return html__peer(out, line);

	const auto print{[&out]
	(const auto &host, const auto &peer)
	{
		using std::setw;
		using std::left;
		using std::right;

		const net::ipport &ipp{peer.remote};
		out << setw(40) << left << host;

		if(ipp)
		    out << ' ' << setw(22) << left << ipp;
		else
		    out << ' ' << setw(22) << left << " ";

		out << " " << setw(2) << right << peer.link_count()     << " L"
		    << " " << setw(2) << right << peer.tag_count()      << " T"
		    << " " << setw(2) << right << peer.tag_committed()  << " TC"
		    << " " << setw(9) << right << peer.write_size()     << " UP Q"
		    << " " << setw(9) << right << peer.read_size()      << " DN Q"
		    << " " << setw(9) << right << peer.write_total()    << " UP"
		    << " " << setw(9) << right << peer.read_total()     << " DN"
		    ;

		if(peer.err_has() && peer.err_msg())
			out << "  :" << peer.err_msg();
		else if(peer.err_has())
			out << "  <unknown error>"_sv;

		out << std::endl;
	}};

	const params param{line, " ",
	{
		"[hostport]", "[all]"
	}};

	const auto &hostport
	{
		param[0]
	};

	const bool all
	{
		has(line, "all")
	};

	if(hostport && hostport != "all")
	{
		auto &peer
		{
			server::find(hostport)
		};

		print(peer.hostname, peer);
		return true;
	}

	for(const auto &p : server::peers)
	{
		const auto &host{p.first};
		const auto &peer{*p.second};
		if(peer.err_has() && !all)
			continue;

		print(host, peer);
	}

	return true;
}

bool
console_cmd__peer__error(opt &out, const string_view &line)
{
	for(const auto &pair : ircd::server::peers)
	{
		using std::setw;
		using std::left;
		using std::right;

		const auto &host{pair.first};
		assert(bool(pair.second));
		const auto &peer{*pair.second};
		if(!peer.err_has())
			continue;

		const net::ipport &ipp{peer.remote};
		out << setw(40) << right << host;

		if(ipp)
		    out << ' ' << setw(22) << left << ipp;
		else
		    out << ' ' << setw(22) << left << " ";

		out << peer.e->etime;

		if(peer.err_msg())
			out << "  :" << peer.err_msg();
		else
			out << "  <unknown error>"_sv;

		out << std::endl;
	}

	return true;
}

bool
console_cmd__peer__error__clear__all(opt &out, const string_view &line)
{
	size_t cleared(0);
	for(auto &pair : ircd::server::peers)
	{
		const auto &name{pair.first};
		assert(bool(pair.second));
		auto &peer{*pair.second};
		cleared += peer.err_clear();
	}

	out << "cleared " << cleared
	    << " of " << ircd::server::peers.size()
	    << std::endl;

	return true;
}

bool
console_cmd__peer__error__clear(opt &out, const string_view &line)
{
	if(empty(line))
		return console_cmd__peer__error__clear__all(out, line);

	const net::hostport hp
	{
		token(line, ' ', 0)
	};

	const auto cleared
	{
		server::errclear(hp)
	};

	out << std::boolalpha << cleared << std::endl;
	return true;
}

bool
console_cmd__peer__version(opt &out, const string_view &line)
{
	for(const auto &p : server::peers)
	{
		using std::setw;
		using std::left;
		using std::right;

		const auto &host{p.first};
		const auto &peer{*p.second};
		const net::ipport &ipp{peer.remote};

		out << setw(40) << right << host;

		if(ipp)
		    out << ' ' << setw(22) << left << ipp;
		else
		    out << ' ' << setw(22) << left << " ";

		if(!empty(peer.server_name))
			out << " :" << peer.server_name;

		out << std::endl;
	}

	return true;
}

bool
console_cmd__peer__find(opt &out, const string_view &line)
{
	const params param{line, " ",
	{
		"ip:port"
	}};

	const auto &ip{rsplit(param.at(0), ':').first};
	const auto &port{rsplit(param.at(0), ':').second};
	const net::ipport ipp{ip, port? port : "0"};

	for(const auto &p : server::peers)
	{
		const auto &hostname{p.first};
		const auto &peer{*p.second};
		const net::ipport &ipp_
		{
			peer.remote
		};

		if(is_v6(ipp) && (!is_v6(ipp_) || host6(ipp) != host6(ipp_)))
			continue;

		if(is_v4(ipp) && (!is_v4(ipp_) || host4(ipp) != host4(ipp_)))
			continue;

		if(net::port(ipp) && net::port(ipp) != net::port(ipp_))
			continue;

		out << hostname << std::endl;
		break;
	}

	return true;
}

bool
console_cmd__peer__cancel(opt &out, const string_view &line)
try
{
	const params param{line, " ",
	{
		"hostport"
	}};

	const auto &hostport
	{
		param.at(0)
	};

	auto &peer
	{
		server::find(hostport)
	};

	peer.cancel();
	return true;
}
catch(const std::out_of_range &e)
{
	throw error
	{
		"Peer not found"
	};
}

bool
console_cmd__peer__close(opt &out, const string_view &line)
try
{
	const params param{line, " ",
	{
		"hostport", "[dc]"
	}};

	const auto &hostport
	{
		param.at(0)
	};

	const auto &dc
	{
		param.at(1, "SSL_NOTIFY"_sv)
	};

	auto &peer
	{
		server::find(hostport)
	};

	const net::close_opts opts
	{
		dc == "RST"?
			net::dc::RST:
		dc == "SSL_NOTIFY"?
			net::dc::SSL_NOTIFY:
			net::dc::SSL_NOTIFY
	};

	peer.close(opts);
	peer.err_clear();
	return true;
}
catch(const std::out_of_range &e)
{
	throw error
	{
		"Peer not found"
	};
}

//
// net
//

bool
console_cmd__net__host(opt &out, const string_view &line)
{
	const params token
	{
		line, " ", {"host", "service"}
	};

	const auto &host{token.at(0)};
	const auto &service
	{
		token.count() > 1? token.at(1) : string_view{}
	};

	const net::hostport hostport
	{
		host, service
	};

	ctx::dock dock;
	bool done{false};
	net::ipport ipport;
	std::exception_ptr eptr;
	net::dns(hostport, [&done, &dock, &eptr, &ipport]
	(std::exception_ptr eptr_, const net::hostport &, const net::ipport &ipport_)
	{
		eptr = std::move(eptr_);
		ipport = ipport_;
		done = true;
		dock.notify_one();
	});

	while(!done)
		dock.wait();

	if(eptr)
		std::rethrow_exception(eptr);
	else
		out << ipport << std::endl;

	return true;
}

bool
console_cmd__host(opt &out, const string_view &line)
{
	return console_cmd__net__host(out, line);
}

bool
console_cmd__net__host__cache__A(opt &out, const string_view &line)
{
	for(const auto &pair : net::dns::cache.A)
	{
		const auto &host{pair.first};
		const auto &record{pair.second};
		const net::ipport ipp{record.ip4, 0};
		out << std::setw(48) << std::right << host
		    << "  =>  " << std::setw(21) << std::left << ipp
		    << "  expires " << timestr(record.ttl, ircd::localtime)
		    << " (" << record.ttl << ")"
		    << std::endl;
	}

	return true;
}

bool
console_cmd__net__host__cache__SRV(opt &out, const string_view &line)
{
	for(const auto &pair : net::dns::cache.SRV)
	{
		const auto &key{pair.first};
		const auto &record{pair.second};
		const net::hostport hostport
		{
			record.tgt, record.port
		};

		out << std::setw(48) << std::right << key
		    << "  =>  " << std::setw(48) << std::left << hostport
		    <<  " expires " << timestr(record.ttl, ircd::localtime)
		    << " (" << record.ttl << ")"
		    << std::endl;
	}

	return true;
}

//
// client
//

bool
console_cmd__client(opt &out, const string_view &line)
{
	using std::setw;
	using std::left;
	using std::right;

	const params param{line, " ",
	{
		"[reqs|id]",
	}};

	const bool &reqs
	{
		param[0] == "reqs"
	};

	const auto &idnum
	{
		!reqs?
			param.at<ulong>(0, 0):
			0
	};

	for(const auto *const &client : ircd::client::list)
	{
		if(idnum && client->id < idnum)
			continue;
		else if(idnum && client->id > idnum)
			break;
		else if(reqs && !client->reqctx)
			continue;

		out << setw(8) << left << client->id
		    << "  " << right << setw(22) << local(*client)
		    << "  " << left << setw(22) << remote(*client)
		    ;

		if(bool(client->sock))
		{
			const auto stat
			{
				net::bytes(*client->sock)
			};

			out << " | UP " << setw(8) << right << stat.second
			    << " | DN " << setw(8) << right << stat.first
			    << " |";
		}

		if(client->reqctx)
			out << " CTX " << setw(4) << id(*client->reqctx);
		else
			out << " ASYNC";
/*
		if(client->request.user_id)
			out << " USER " << client->request.user_id;

		if(client->request.origin)
			out << " PEER " << client->request.origin;
*/
		if(client->request.head.method)
			out << " " << client->request.head.method << ""
			    << " " << client->request.head.path;

		out << std::endl;
	}

	return true;
}
