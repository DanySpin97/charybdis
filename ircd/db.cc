// Matrix Construct
//
// Copyright (C) Matrix Construct Developers, Authors & Contributors
// Copyright (C) 2016-2018 Jason Volk <jason@zemos.net>
//
// Permission to use, copy, modify, and/or distribute this software for any
// purpose with or without fee is hereby granted, provided that the above
// copyright notice and this permission notice is present in all copies. The
// full license for this software is available in the LICENSE file.

#include "db.h"

/// Dedicated logging facility for the database subsystem
decltype(ircd::db::log)
ircd::db::log
{
	"db", 'D'
};

/// Dedicated logging facility for rocksdb's log callbacks
decltype(ircd::db::rog)
ircd::db::rog
{
	"db.rocksdb"
};

decltype(ircd::db::version_api)
ircd::db::version_api
{
	"RocksDB", info::versions::API, 0,
	{
		ROCKSDB_MAJOR, ROCKSDB_MINOR, ROCKSDB_PATCH,
	}
};

extern "C" const char *
rocksdb_build_git_sha;

extern "C" const char *
rocksdb_build_compile_date;

decltype(ircd::db::version_abi)
ircd::db::version_abi
{
	"RocksDB", info::versions::ABI, 0, {0}, []
	(auto &, const mutable_buffer &buf)
	{
		fmt::sprintf
		{
			buf, "%s (%s)",
			lstrip(rocksdb_build_git_sha, "rocksdb_build_git_sha:"),
			rocksdb_build_compile_date,
		};
	}
};

ircd::conf::item<size_t>
ircd::db::request_pool_stack_size
{
	{ "name",     "ircd.db.request_pool.stack_size" },
	{ "default",  long(128_KiB)                     },
};

ircd::conf::item<size_t>
ircd::db::request_pool_size
{
	{
		{ "name",     "ircd.db.request_pool.size" },
		{ "default",  0L                          },
	}, []
	{
		request.set(size_t(request_pool_size));
	}
};

decltype(ircd::db::request_pool_opts)
ircd::db::request_pool_opts
{
	size_t(request_pool_stack_size),
	size_t(request_pool_size),
	-1,   // No hard limit
	0,    // Soft limit at any queued
	true, // Yield before hitting soft limit
};

/// Concurrent request pool. Requests to seek may be executed on this
/// pool in cases where a single context would find it advantageous.
/// Some examples are a db::row seek, or asynchronous prefetching.
///
/// The number of workers in this pool should upper bound at the
/// number of concurrent AIO requests which are effective on this
/// system. This is a static pool shared by all databases.
decltype(ircd::db::request)
ircd::db::request
{
	"db req", request_pool_opts
};

/// This mutex is necessary to serialize entry into rocksdb's write impl
/// otherwise there's a risk of a deadlock if their internal pthread
/// mutexes are contended. This is because a few parts of rocksdb are
/// incorrectly using std::mutex directly when they ought to be using their
/// rocksdb::port wrapper.
decltype(ircd::db::write_mutex)
ircd::db::write_mutex;

///////////////////////////////////////////////////////////////////////////////
//
// init
//

namespace ircd::db
{
	extern const std::string direct_io_test_file_path;
}

decltype(ircd::db::direct_io_test_file_path)
ircd::db::direct_io_test_file_path
{
	fs::path_string(fs::path_views
	{
		fs::base::db, "SUPPORTS_DIRECT_IO"_sv
	})
};

ircd::db::init::init()
try
{
	#ifdef IRCD_DB_HAS_ALLOCATOR
	database::allocator::init();
	#endif
	compressions();
	directory();
	request_pool();
	test_direct_io();
	test_hw_crc32();
}
catch(const std::exception &e)
{
	log::critical
	{
		log, "Cannot start database system :%s",
		e.what()
	};

	throw;
}

ircd::db::init::~init()
noexcept
{
	delete prefetcher;
	prefetcher = nullptr;

	if(request.active())
		log::warning
		{
			log, "Terminating %zu active of %zu client request contexts; %zu pending; %zu queued",
			request.active(),
			request.size(),
			request.pending(),
			request.queued()
		};

	request.terminate();
	log::debug
	{
		log, "Waiting for %zu active of %zu client request contexts; %zu pending; %zu queued",
		request.active(),
		request.size(),
		request.pending(),
		request.queued()
	};

	request.join();
	log::debug
	{
		log, "All contexts joined; all requests are clear."
	};

	#ifdef IRCD_DB_HAS_ALLOCATOR
	database::allocator::fini();
	#endif
}

void
ircd::db::init::directory()
try
{
	const string_view &dbdir
	{
		fs::base::db
	};

	if(!fs::is_dir(dbdir) && (ircd::read_only || ircd::write_avoid))
		log::warning
		{
			log, "Not creating database directory `%s' in read-only/write-avoid mode.", dbdir
		};
	else if(fs::mkdir(dbdir))
		log::notice
		{
			log, "Created new database directory at `%s'", dbdir
		};
	else
		log::info
		{
			log, "Using database directory at `%s'", dbdir
		};
}
catch(const fs::error &e)
{
	log::error
	{
		log, "Database directory error: %s", e.what()
	};

	throw;
}

void
ircd::db::init::test_direct_io()
try
{
	const auto &test_file_path
	{
		direct_io_test_file_path
	};

	if(fs::support::direct_io(test_file_path))
		log::debug
		{
			log, "Detected Direct-IO works by opening test file at `%s'",
			test_file_path
		};
	else
		log::warning
		{
			log, "Direct-IO is not supported in the database directory `%s'"
			"; Concurrent database queries will not be possible.",
			string_view{fs::base::db}
		};
}
catch(const std::exception &e)
{
	log::error
	{
		log, "Failed to test if Direct-IO possible with test file `%s'"
		"; Concurrent database queries will not be possible :%s",
		direct_io_test_file_path,
		e.what()
	};
}

namespace rocksdb::crc32c
{
	extern std::string IsFastCrc32Supported();
}

void
ircd::db::init::test_hw_crc32()
try
{
	const auto supported_str
	{
		rocksdb::crc32c::IsFastCrc32Supported()
	};

	const bool supported
	{
		startswith(supported_str, "Supported")
	};

	assert(supported || startswith(supported_str, "Not supported"));

	if(!supported)
		log::warning
		{
			log, "crc32c hardware acceleration is not available on this platform."
		};
}
catch(const std::exception &e)
{
	log::error
	{
		log, "Failed to test crc32c hardware acceleration support :%s",
		e.what()
	};
}

decltype(ircd::db::compressions)
ircd::db::compressions;

void
ircd::db::init::compressions()
try
{
	auto supported
	{
		rocksdb::GetSupportedCompressions()
	};

	size_t i(0);
	for(const rocksdb::CompressionType &type_ : supported) try
	{
		auto &[string, type]
		{
			db::compressions.at(i++)
		};

		type = type_;
		throw_on_error
		{
			rocksdb::GetStringFromCompressionType(&string, type_)
		};

		log::debug
		{
			log, "Detected supported compression #%zu type:%lu :%s",
			i,
			type,
			string,
		};
	}
	catch(const std::exception &e)
	{
		log::error
		{
			log, "Failed to identify compression type:%u :%s",
			uint(type_),
			e.what()
		};
	}

	if(supported.empty())
		log::warning
		{
			"No compression libraries have been linked with the DB."
			" This is probably not what you want."
		};
}
catch(const std::exception &e)
{
	log::error
	{
		log, "Failed to initialize database compressions :%s",
		e.what()
	};

	throw;
}

void
ircd::db::init::request_pool()
{
	char buf[32];
	const string_view value
	{
		conf::get(buf, "ircd.fs.aio.max_events")
	};

	const size_t aio_max_events
	{
		lex_castable<size_t>(value)?
			lex_cast<size_t>(value):
			0UL
	};

	const size_t new_size
	{
		size_t(request_pool_size)?
			request_pool_size:
		aio_max_events?
			aio_max_events:
			1UL
	};

	request_pool_size.set(lex_cast(new_size));
}

///////////////////////////////////////////////////////////////////////////////
//
// database
//

/// Conf item toggles if full database checksum verification should occur
/// when any database is opened.
decltype(ircd::db::open_check)
ircd::db::open_check
{
	{ "name",     "ircd.db.open.check"  },
	{ "default",  false                 },
	{ "persist",  false                 },
};

/// Conf item determines the recovery mode to use when opening any database.
///
/// "absolute" - The default and is the same for an empty value. This means
/// any database corruptions are treated as an error on open and an exception
/// is thrown with nothing else done.
///
/// "point" - The database is rolled back to before any corruption. This will
/// lose some of the latest data last committed, but will open the database
/// and continue normally thereafter.
///
/// "skip" - The corrupted areas are skipped over and the database continues
/// normally with just those assets missing. This option is dangerous because
/// the database continues in a logically incoherent state which is only ok
/// for very specific applications.
///
/// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
///
/// IRCd's applications are NOT tolerant of skip recovery. You will create an
/// incoherent database. NEVER USE "skip" RECOVERY MODE.
///
/// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
///
decltype(ircd::db::open_recover)
ircd::db::open_recover
{
	{ "name",     "ircd.db.open.recover"  },
	{ "default",  "absolute"              },
	{ "persist",  false                   },
};

/// Conf item determines if database repair should occur (before open). This
/// mechanism can be used when SST file corruption occurs which is too deep
/// for log-based recovery. The affected blocks may be discarded; this risks
/// destabilizing an application expecting the data in those blocks to exist.
///
/// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
///
/// Use with caution.
///
/// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
///
decltype(ircd::db::open_repair)
ircd::db::open_repair
{
	{ "name",     "ircd.db.open.repair"  },
	{ "default",  false                  },
	{ "persist",  false                  },
};

/// Conf item toggles whether automatic compaction is enabled or disabled for
/// all databases upon opening. This is useful for developers, debugging and
/// valgrind etc to prevent these background jobs from spawning when unwanted.
decltype(ircd::db::auto_compact)
ircd::db::auto_compact
{
	{ "name",     "ircd.db.compact.auto" },
	{ "default",  true                   },
	{ "persist",  false                  },
};

/// Conf item dictates whether databases will be opened in slave mode; this
/// is a recent feature of RocksDB which may not be available. It allows two
/// instances of a database, so long as only one is not opened as a slave.
decltype(ircd::db::open_slave)
ircd::db::open_slave
{
	{ "name",     "ircd.db.open.slave" },
	{ "default",  false                },
	{ "persist",  false                },
};

void
ircd::db::sync(database &d)
{
	log::debug
	{
		log, "[%s] @%lu SYNC WAL",
		name(d),
		sequence(d)
	};

	throw_on_error
	{
		d.d->SyncWAL()
	};
}

/// Flushes all columns. Note that if blocking=true, blocking may occur for
/// each column individually.
void
ircd::db::flush(database &d,
                const bool &sync)
{
	log::debug
	{
		log, "[%s] @%lu FLUSH WAL",
		name(d),
		sequence(d)
	};

	throw_on_error
	{
		d.d->FlushWAL(sync)
	};
}

/// Moves memory structures to SST files for all columns. This doesn't
/// necessarily sort anything that wasn't previously sorted, but it may create
/// new SST files and shouldn't be confused with a typical fflush().
/// Note that if blocking=true, blocking may occur for each column individually.
void
ircd::db::sort(database &d,
               const bool &blocking,
               const bool &now)
{
	for(const auto &c : d.columns)
	{
		db::column column{*c};
		db::sort(column, blocking, now);
	}
}

void
ircd::db::compact(database &d,
                  const compactor &cb)
{
	static const std::pair<string_view, string_view> range
	{
		{}, {}
	};

	for(const auto &c : d.columns) try
	{
		db::column column{*c};
		compact(column, range, -1, cb);
	}
	catch(const ctx::interrupted &)
	{
		throw;
	}
	catch(const std::exception &e)
	{
		assert(c);
		log::error
		{
			log, "[%s] compact '%s' :%s",
			name(d),
			name(*c),
			e.what(),
		};
	}
}

void
ircd::db::compact(database &d,
                  const std::pair<int, int> &level,
                  const compactor &cb)
{
	for(const auto &c : d.columns) try
	{
		db::column column{*c};
		compact(column, level, cb);
	}
	catch(const ctx::interrupted &)
	{
		throw;
	}
	catch(const std::exception &e)
	{
		assert(c);
		log::error
		{
			log, "[%s] compact '%s' :%s",
			name(d),
			name(*c),
			e.what(),
		};
	}
}

void
ircd::db::check(database &d)
{
	assert(d.d);
	throw_on_error
	{
		d.d->VerifyChecksum()
	};
}

void
ircd::db::check(database &d,
                const string_view &file)
{
	assert(file);
	assert(d.d);

	const auto &opts
	{
		d.d->GetOptions()
	};

	const rocksdb::EnvOptions env_opts
	{
		opts
	};

	const bool absolute
	{
		fs::is_absolute(file)
	};

	const string_view parts[]
	{
		d.path, file
	};

	const std::string path
	{
		!absolute?
			fs::path_string(parts):
			std::string{file}
	};

	throw_on_error
	{
		rocksdb::VerifySstFileChecksum(opts, env_opts, path)
	};
}

void
ircd::db::resume(database &d)
{
	assert(d.d);
	const ctx::uninterruptible::nothrow ui;
	const std::lock_guard lock{write_mutex};
	const auto errors
	{
		db::errors(d)
	};

	log::debug
	{
		log, "[%s] Attempting to resume from %zu errors @%lu",
		name(d),
		errors.size(),
		sequence(d)
	};

	throw_on_error
	{
		d.d->Resume()
	};

	d.errors.clear();

	log::info
	{
		log, "[%s] Resumed normal operation at sequence number %lu; cleared %zu errors",
		name(d),
		sequence(d),
		errors.size()
	};
}

void
ircd::db::refresh(database &d)
{
	assert(d.d);

	throw_on_error
	{
		#ifdef IRCD_DB_HAS_SECONDARY
		d.d->TryCatchUpWithPrimary()
		#else
		rocksdb::Status::NotSupported(slice("Slave mode not supported by this RocksDB"_sv))
		#endif
	};

	log::debug
	{
		log, "[%s] Caught up with primary database.",
		name(d)
	};
}

void
ircd::db::bgpause(database &d)
{
	assert(d.d);

	throw_on_error
	{
		d.d->PauseBackgroundWork()
	};

	log::debug
	{
		log, "[%s] Paused all background work",
		name(d)
	};
}

void
ircd::db::bgcontinue(database &d)
{
	assert(d.d);

	log::debug
	{
		log, "[%s] Continuing background work",
		name(d)
	};

	throw_on_error
	{
		d.d->ContinueBackgroundWork()
	};
}

void
ircd::db::bgcancel(database &d,
                   const bool &blocking)
{
	assert(d.d);
	log::debug
	{
		log, "[%s] Canceling all background work...",
		name(d)
	};

	rocksdb::CancelAllBackgroundWork(d.d.get(), blocking);
	if(!blocking)
		return;

	assert(d.env);
	assert(d.env->st);
	const ctx::uninterruptible::nothrow ui;
	for(auto &pool : d.env->st->pool) if(pool)
	{
		log::debug
		{
			log, "[%s] Waiting for tasks:%zu queued:%zu active:%zu in pool '%s'",
			name(d),
			pool->tasks.size(),
			pool->p.pending(),
			pool->p.active(),
			ctx::name(pool->p),
		};

		pool->wait();
	}

	const auto errors
	{
		property<uint64_t>(d, rocksdb::DB::Properties::kBackgroundErrors)
	};

	const auto level
	{
		errors? log::level::ERROR : log::level::DEBUG
	};

	log::logf
	{
		log, level,
		"[%s] Canceled all background work; errors:%lu",
		name(d),
		errors
	};
}

/// Writes a snapshot of this database to the directory specified. The
/// snapshot consists of hardlinks to the bulk data files of this db, but
/// copies the other stuff that usually gets corrupted. The directory can
/// then be opened as its own database either read-only or read-write.
/// Incremental backups and rollbacks can begin from this interface. Note
/// this may be an expensive blocking operation.
uint64_t
ircd::db::checkpoint(database &d)
{
	if(!d.checkpointer)
		throw error
		{
			"Checkpointing is not available for db(%p) '%s",
			&d,
			name(d)
		};

	const std::lock_guard lock{write_mutex};
	const ctx::uninterruptible::nothrow ui;
	const auto seqnum
	{
		sequence(d)
	};

	const std::string dir
	{
		db::path(name(d), seqnum)
	};

	throw_on_error
	{
		d.checkpointer->CreateCheckpoint(dir, 0)
	};

	log::debug
	{
		log, "[%s] Checkpoint at sequence %lu in `%s' complete",
		name(d),
		seqnum,
		dir
	};

	return seqnum;
}

/// This wraps RocksDB's "File Deletions" which means after RocksDB
/// compresses some file it then destroys the uncompressed version;
/// setting this to false will disable that and retain both versions.
/// This is useful when a direct reference is being manually held by
/// us into the uncompressed version which must remain valid.
void
ircd::db::fdeletions(database &d,
                     const bool &enable,
                     const bool &force)
{
	if(enable) throw_on_error
	{
		d.d->EnableFileDeletions(force)
	};
	else throw_on_error
	{
		d.d->DisableFileDeletions()
	};
}

void
ircd::db::setopt(database &d,
                 const string_view &key,
                 const string_view &val)
{
	const std::unordered_map<std::string, std::string> options
	{
		{ std::string{key}, std::string{val} }
	};

	throw_on_error
	{
		d.d->SetDBOptions(options)
	};
}

/// Set the rdb logging level by translating our ircd::log::level to the
/// RocksDB enum. This translation is a reasonable convenience, as both
/// enums are similar enough.
void
ircd::db::loglevel(database &d,
                   const ircd::log::level &fac)
{
	using ircd::log::level;

	rocksdb::InfoLogLevel lev
	{
		rocksdb::WARN_LEVEL
	};

	switch(fac)
	{
		case level::CRITICAL:  lev = rocksdb::FATAL_LEVEL;   break;
		case level::ERROR:     lev = rocksdb::ERROR_LEVEL;   break;
		case level::WARNING:
		case level::NOTICE:    lev = rocksdb::WARN_LEVEL;    break;
		case level::INFO:      lev = rocksdb::INFO_LEVEL;    break;
		case level::DERROR:
		case level::DWARNING:
		case level::DEBUG:     lev = rocksdb::DEBUG_LEVEL;   break;
		case level::_NUM_:     assert(0);                    break;
	}

	d.logger->SetInfoLogLevel(lev);
}

/// Set the rdb logging level by translating our ircd::log::level to the
/// RocksDB enum. This translation is a reasonable convenience, as both
/// enums are similar enough.
ircd::log::level
ircd::db::loglevel(const database &d)
{
	const auto &level
	{
		d.logger->GetInfoLogLevel()
	};

	switch(level)
	{
		default:
		case rocksdb::NUM_INFO_LOG_LEVELS:
			assert(0);

		case rocksdb::HEADER_LEVEL:
		case rocksdb::FATAL_LEVEL:     return log::level::CRITICAL;
		case rocksdb::ERROR_LEVEL:     return log::level::ERROR;
		case rocksdb::WARN_LEVEL:      return log::level::WARNING;
		case rocksdb::INFO_LEVEL:      return log::level::INFO;
		case rocksdb::DEBUG_LEVEL:     return log::level::DEBUG;
	}
}

ircd::db::options
ircd::db::getopt(const database &d)
{
	return options
	{
		d.d->GetDBOptions()
	};
}

size_t
ircd::db::bytes(const database &d)
{
	return std::accumulate(begin(d.columns), end(d.columns), size_t(0), []
	(auto ret, const auto &colptr)
	{
		db::column c{*colptr};
		return ret += db::bytes(c);
	});
}

size_t
ircd::db::file_count(const database &d)
{
	return std::accumulate(begin(d.columns), end(d.columns), size_t(0), []
	(auto ret, const auto &colptr)
	{
		db::column c{*colptr};
		return ret += db::file_count(c);
	});
}

/// Get the list of WAL (Write Ahead Log) files.
std::vector<std::string>
ircd::db::wals(const database &cd)
{
	auto &d
	{
		const_cast<database &>(cd)
	};

	std::vector<std::unique_ptr<rocksdb::LogFile>> vec;
	throw_on_error
	{
		d.d->GetSortedWalFiles(vec)
	};

	std::vector<std::string> ret(vec.size());
	std::transform(begin(vec), end(vec), begin(ret), []
	(const auto &file)
	{
		return file->PathName();
	});

	return ret;
}

/// Get the live file list for db; see overlord documentation.
std::vector<std::string>
ircd::db::files(const database &d)
{
	uint64_t ignored;
	return files(d, ignored);
}

/// Get the live file list for database relative to the database's directory.
/// One of the files is a manifest file which is over-allocated and its used
/// size is returned in the integer passed to the `msz` argument.
///
/// This list may not be completely up to date. The reliable way to get the
/// most current list is to flush all columns first and ensure no database
/// activity took place between the flushing and this query.
std::vector<std::string>
ircd::db::files(const database &cd,
                uint64_t &msz)
{
	std::vector<std::string> ret;
	auto &d(const_cast<database &>(cd));
	throw_on_error
	{
		d.d->GetLiveFiles(ret, &msz, false)
	};

	return ret;
}

const std::vector<std::string> &
ircd::db::errors(const database &d)
{
	return d.errors;
}

uint64_t
ircd::db::sequence(const database &cd)
{
	database &d(const_cast<database &>(cd));
	return d.d->GetLatestSequenceNumber();
}

rocksdb::Cache *
ircd::db::cache(database &d)
{
	return d.row_cache.get();
}

const rocksdb::Cache *
ircd::db::cache(const database &d)
{
	return d.row_cache.get();
}

template<>
ircd::db::prop_int
ircd::db::property(const database &cd,
                   const string_view &name)
{
	uint64_t ret(0);
	database &d(const_cast<database &>(cd));
	if(!d.d->GetAggregatedIntProperty(slice(name), &ret))
		throw not_found
		{
			"property '%s' for all columns in '%s' not found or not an integer.",
			name,
			db::name(d)
		};

	return ret;
}

std::shared_ptr<ircd::db::database::column>
ircd::db::shared_from(database::column &column)
{
	return column.shared_from_this();
}

std::shared_ptr<const ircd::db::database::column>
ircd::db::shared_from(const database::column &column)
{
	return column.shared_from_this();
}

const std::string &
ircd::db::uuid(const database &d)
{
	return d.uuid;
}

const std::string &
ircd::db::name(const database &d)
{
	return d.name;
}

//
// database
//

namespace ircd::db
{
	extern const description default_description;
}

// Instance list linkage
template<>
decltype(ircd::util::instance_list<ircd::db::database>::allocator)
ircd::util::instance_list<ircd::db::database>::allocator
{};

template<>
decltype(ircd::util::instance_list<ircd::db::database>::list)
ircd::util::instance_list<ircd::db::database>::list
{
	allocator
};

decltype(ircd::db::default_description)
ircd::db::default_description
{
	/// Requirement of RocksDB going back to LevelDB. This column must
	/// always exist in all descriptions and probably should be at idx[0].
	{ "default" }
};

ircd::db::database &
ircd::db::database::get(column &column)
{
	assert(column.d);
	return *column.d;
}

const ircd::db::database &
ircd::db::database::get(const column &column)
{
	assert(column.d);
	return *column.d;
}

ircd::db::database &
ircd::db::database::get(const string_view &name)
{
	const auto pair
	{
		namepoint(name)
	};

	return get(pair.first, pair.second);
}

ircd::db::database &
ircd::db::database::get(const string_view &name,
                        const uint64_t &checkpoint)
{
	auto *const &d
	{
		get(std::nothrow, name, checkpoint)
	};

	if(likely(d))
		return *d;

	throw checkpoint == uint64_t(-1)?
		std::out_of_range{"No database with that name exists"}:
		std::out_of_range{"No database with that name at that checkpoint exists"};
}

ircd::db::database *
ircd::db::database::get(std::nothrow_t,
                        const string_view &name)
{
	const auto pair
	{
		namepoint(name)
	};

	return get(std::nothrow, pair.first, pair.second);
}

ircd::db::database *
ircd::db::database::get(std::nothrow_t,
                        const string_view &name,
                        const uint64_t &checkpoint)
{
	for(auto *const &d : list)
		if(name == d->name)
			if(checkpoint == uint64_t(-1) || checkpoint == d->checkpoint)
				return d;

	return nullptr;
}

//
// database::database
//

ircd::db::database::database(const string_view &name,
                             std::string optstr)
:database
{
	name, std::move(optstr), default_description
}
{
}

ircd::db::database::database(const string_view &name,
                             std::string optstr,
                             description description)
:database
{
	namepoint(name).first, namepoint(name).second, std::move(optstr), std::move(description)
}
{
}

ircd::db::database::database(const string_view &name,
                             const uint64_t &checkpoint,
                             std::string optstr,
                             description description)
try
:name
{
	namepoint(name).first
}
,checkpoint
{
	// a -1 may have been generated by the db::namepoint() util when the user
	// supplied just a name without a checkpoint. In the context of database
	// opening/creation -1 just defaults to 0.
	checkpoint == uint64_t(-1)? 0 : checkpoint
}
,path
{
	db::path(this->name, this->checkpoint)
}
,optstr
{
	std::move(optstr)
}
,fsck
{
	db::open_repair
}
,slave
{
	db::open_slave
}
,read_only
{
	slave || ircd::read_only
}
,env
{
	std::make_shared<struct env>(this)
}
,stats
{
	std::make_shared<struct stats>(this)
}
,logger
{
	std::make_shared<struct logger>(this)
}
,events
{
	std::make_shared<struct events>(this)
}
,mergeop
{
	std::make_shared<struct mergeop>(this)
}
,wal_filter
{
	std::make_unique<struct wal_filter>(this)
}
,allocator
{
	#ifdef IRCD_DB_HAS_ALLOCATOR
	std::make_shared<struct allocator>(this, nullptr, database::allocator::cache_arena)
	#endif
}
,ssts{rocksdb::NewSstFileManager
(
	env.get(),   // env
	logger,      // logger
	{},          // trash_dir
	0,           // rate_bytes_per_sec
	true,        // delete_existing_trash
	nullptr,     // Status*
	0.05,        // max_trash_db_ratio 0.25
	64_MiB       // bytes_max_delete_chunk
)}
,row_cache
{
	std::make_shared<database::cache>
	(
		this, this->stats, this->allocator, this->name, 16_MiB
	)
}
,descriptors
{
	std::move(description)
}
,opts{[this]
{
	auto opts
	{
		std::make_unique<rocksdb::DBOptions>(make_dbopts(this->optstr, &this->optstr, &read_only, &fsck))
	};

	// Setup sundry
	opts->create_if_missing = true;
	opts->create_missing_column_families = true;

	// Uses thread_local counters in rocksdb and probably useless for ircd::ctx.
	opts->enable_thread_tracking = false;

	// MUST be 0 or std::threads are spawned in rocksdb.
	opts->max_file_opening_threads = 0;

	// limit maxfdto prevent too many small files degrading read perf; too low is
	// bad for write perf.
	opts->max_open_files = !slave?
		fs::support::rlimit_nofile():
		-1;

	// TODO: Check if these values can be increased; RocksDB may keep
	// thread_local state preventing values > 1.
	opts->max_background_jobs = 16;
	opts->max_background_flushes = 1;
	opts->max_background_compactions = 1;

	opts->max_total_wal_size = 96_MiB; //TODO: conf
	opts->db_write_buffer_size = 96_MiB; //TODO: conf
	//opts->max_log_file_size = 32_MiB; //TODO: conf

	//TODO: range_sync
	opts->bytes_per_sync = 0;
	opts->wal_bytes_per_sync = 0;

	// For the write-side of a compaction process: writes will be of approx
	// this size. The compaction process is composing a buffer of this size
	// between those writes. Too large a buffer will hog the CPU and starve
	// other ircd::ctx's. Too small a buffer will be inefficient.
	opts->writable_file_max_buffer_size = 4_MiB; //TODO: conf

	// MUST be 1 (no subcompactions) or rocksdb spawns internal std::thread.
	opts->max_subcompactions = 1;

	// Disable noise
	opts->stats_dump_period_sec = 0;

	// Disables the timer to delete unused files; this operation occurs
	// instead with our compaction operations so we don't need to complicate.
	opts->delete_obsolete_files_period_micros = 0;
	opts->keep_log_file_num = 16;

	// These values prevent codepaths from being taken in rocksdb which may
	// introduce issues for ircd::ctx. We should still fully investigate
	// if any of these features can safely be used.
	opts->allow_concurrent_memtable_write = true;
	opts->enable_write_thread_adaptive_yield = false;
	opts->enable_pipelined_write = false;
	opts->write_thread_max_yield_usec = 0;
	opts->write_thread_slow_yield_usec = 0;

	// Detect if O_DIRECT is possible if db::init left a file in the
	// database directory claiming such. User can force no direct io
	// with program option at startup (i.e -nodirect).
	opts->use_direct_reads = bool(fs::fd::opts::direct_io_enable)?
		fs::exists(direct_io_test_file_path):
		false;

	// For the read-side of the compaction process.
	opts->compaction_readahead_size = !opts->use_direct_reads?
		512_KiB: //TODO: conf
		0;

	// Use the determined direct io value for writes as well.
	//opts->use_direct_io_for_flush_and_compaction = opts->use_direct_reads;

	// Doesn't appear to be in effect when direct io is used. Not supported by
	// all filesystems so disabled for now.
	// TODO: use fs::support::test_fallocate() test similar to direct_io_test_file.
	opts->allow_fallocate = false;

	#ifdef RB_DEBUG
	opts->dump_malloc_stats = true;
	#endif

	// Default corruption tolerance is zero-tolerance; db fails to open with
	// error by default to inform the user. The rest of the options are
	// various relaxations for how to proceed.
	opts->wal_recovery_mode = rocksdb::WALRecoveryMode::kAbsoluteConsistency;

	// When corrupted after crash, the DB is rolled back before the first
	// corruption and erases everything after it, giving a consistent
	// state up at that point, though losing some recent data.
	if(string_view(open_recover) == "point" || string_view(open_recover) == "recover")
		opts->wal_recovery_mode = rocksdb::WALRecoveryMode::kPointInTimeRecovery;

	// When corrupted after crash and PointInTimeRecovery does not work,
	// this will drop more data, but consistently. RocksDB sez the WAL is not
	// used at all in this mode.
	#if ROCKSDB_MAJOR > 6 \
	|| (ROCKSDB_MAJOR == 6 && ROCKSDB_MINOR >= 10)
	if(string_view(open_recover) == "recover")
		opts->best_efforts_recovery = true;
	#endif

	// Skipping corrupted records will create gaps in the DB timeline where the
	// application (like a matrix timeline) cannot tolerate the unexpected gap.
	if(string_view(open_recover) == "skip")
		opts->wal_recovery_mode = rocksdb::WALRecoveryMode::kSkipAnyCorruptedRecords;

	// Tolerating corrupted records is very last-ditch for getting the database to
	// open in a catastrophe. We have no use for this option but should use it for
	//TODO: emergency salvage-mode.
	if(string_view(open_recover) == "tolerate")
		opts->wal_recovery_mode = rocksdb::WALRecoveryMode::kTolerateCorruptedTailRecords;

	// This prevents the creation of additional SST files and lots of I/O on
	// either DB open and close.
	opts->avoid_flush_during_recovery = true;
	opts->avoid_flush_during_shutdown = true;

	// Setup env
	opts->env = env.get();

	// Setup WAL filter
	opts->wal_filter = this->wal_filter.get();

	// Setup SST file mgmt
	opts->sst_file_manager = this->ssts;

	// Setup logging
	logger->SetInfoLogLevel(ircd::debugmode? rocksdb::DEBUG_LEVEL : rocksdb::WARN_LEVEL);
	opts->info_log_level = logger->GetInfoLogLevel();
	opts->info_log = logger;

	// Setup event and statistics callbacks
	opts->listeners.emplace_back(this->events);

	// Setup histogram collecting
	#if ROCKSDB_MAJOR > 6 \
	|| (ROCKSDB_MAJOR == 6 && ROCKSDB_MINOR >= 1)
		//this->stats->set_stats_level(rocksdb::kExceptTimeForMutex);
		this->stats->set_stats_level(rocksdb::kAll);
	#else
		//this->stats->stats_level_ = rocksdb::kExceptTimeForMutex;
		this->stats->stats_level_ = rocksdb::kAll;
	#endif

	opts->statistics = this->stats;

	// Setup performance metric options
	//rocksdb::SetPerfLevel(rocksdb::PerfLevel::kDisable);

	// Setup row cache.
	opts->row_cache = this->row_cache;

	return opts;
}()}
,column_names{[this]
{
	// Existing columns at path. If any are left the descriptor set did not
	// describe all of the columns found in the database at path.
	const auto required
	{
		db::column_names(path, *opts)
	};

	// As we find descriptors for all of the columns on the disk we'll
	// remove their names from this set. Anything remaining is undescribed
	// and that's a fatal error.
	std::set<string_view> existing
	{
		begin(required), end(required)
	};

	// The names of the columns extracted from the descriptor set
	decltype(this->column_names) ret;
	for(auto &descriptor : descriptors)
	{
		// Deprecated columns which have already been dropped won't appear
		// in the existing (required) list. We don't need to construct those.
		if(!existing.count(descriptor.name) && descriptor.drop)
			continue;

		// Construct the column instance and indicate that we have a description
		// for it by removing it from existing.
		ret.emplace(descriptor.name, std::make_shared<column>(*this, descriptor));
		existing.erase(descriptor.name);
	}

	if(!existing.empty())
		throw error
		{
			"Failed to describe existing column '%s' (and %zd others...)",
			*begin(existing),
			existing.size() - 1
		};

	return ret;
}()}
,d{[this]
{
	std::vector<rocksdb::ColumnFamilyHandle *> handles; // filled by DB::Open()
	std::vector<rocksdb::ColumnFamilyDescriptor> columns(this->column_names.size());
	std::transform(begin(this->column_names), end(this->column_names), begin(columns), []
	(const auto &pair)
	{
		const auto &column(*pair.second);
		return static_cast<const rocksdb::ColumnFamilyDescriptor &>(column);
	});

	// NOTE: rocksdb sez RepairDB is broken; can't use now
	if(fsck && fs::is_dir(path))
	{
		log::notice
		{
			log, "Checking database @ `%s' columns[%zu]", path, columns.size()
		};

		throw_on_error
		{
			rocksdb::RepairDB(path, *opts, columns)
		};

		log::info
		{
			log, "Database @ `%s' check complete", path
		};
	}

	// If the directory does not exist, though rocksdb will create it, we can
	// avoid scaring the user with an error log message if we just do that..
	if(opts->create_if_missing && !fs::is_dir(path) && !ircd::write_avoid)
		fs::mkdir(path);

	// Announce attempt before usual point where exceptions are thrown
	log::info
	{
		log, "Opening database \"%s\" @ `%s' with %zu columns...",
		this->name,
		path,
		columns.size()
	};

	if(read_only)
		log::warning
		{
			log, "Database \"%s\" @ `%s' will be opened in read-only mode.",
			this->name,
			path,
		};

	// Open DB into ptr
	rocksdb::DB *ptr;
	if(slave)
		throw_on_error
		{
			#ifdef IRCD_DB_HAS_SECONDARY
			rocksdb::DB::OpenAsSecondary(*opts, path, "/tmp/slave", columns, &handles, &ptr)
			#else
			rocksdb::Status::NotSupported(slice("Slave mode not supported by this RocksDB"_sv))
			#endif
		};
	else if(read_only)
		throw_on_error
		{
			rocksdb::DB::OpenForReadOnly(*opts, path, columns, &handles, &ptr)
		};
	else
		throw_on_error
		{
			rocksdb::DB::Open(*opts, path, columns, &handles, &ptr)
		};

	std::unique_ptr<rocksdb::DB> ret
	{
		ptr
	};

	// Set the handles. We can't throw here so we just log an error.
	for(const auto &handle : handles) try
	{
		this->column_names.at(handle->GetName())->handle.reset(handle);
	}
	catch(const std::exception &e)
	{
		log::critical
		{
			log, "[%s] Error finding described handle '%s' which RocksDB opened :%s",
			this->name,
			handle->GetName(),
			e.what()
		};
	}

	return ret;
}()}
,column_index{[this]
{
	size_t size{0};
	for(const auto &p : column_names)
	{
		const auto &column(*p.second);
		if(db::id(column) + 1 > size)
			size = db::id(column) + 1;
	}

	// This may have some gaps containing nullptrs where a CFID is unused.
	decltype(this->column_index) ret(size);
	for(const auto &p : column_names)
	{
		const auto &colptr(p.second);
		ret.at(db::id(*colptr)) = colptr;
	}

	return ret;
}()}
,columns{[this]
{
	// Skip the gaps in the column_index vector to make the columns list
	// only contain active column instances.
	decltype(this->columns) ret;
	for(const auto &ptr : this->column_index)
		if(ptr)
			ret.emplace_back(ptr);

	return ret;
}()}
,uuid{[this]
{
	std::string ret;
	throw_on_error
	{
		d->GetDbIdentity(ret)
	};

	return ret;
}()}
,checkpointer{[this]
{
	rocksdb::Checkpoint *checkpointer{nullptr};
	throw_on_error
	{
		rocksdb::Checkpoint::Create(this->d.get(), &checkpointer)
	};

	return checkpointer;
}()}
{
	// Conduct drops from schema changes. The database must be fully opened
	// as if they were not dropped first, then we conduct the drop operation
	// here. The drop operation has no effects until the database is next
	// closed; the dropped columns will still work during this instance.
	for(const auto &colptr : columns)
		if(describe(*colptr).drop)
			db::drop(*colptr);

	// Database integrity check branch.
	if(bool(open_check))
	{
		log::notice
		{
			log, "[%s] Verifying database integrity. This may take several minutes...",
			this->name
		};

		check(*this);
	}

	log::info
	{
		log, "[%s] Opened database @ `%s' with %zu columns at sequence number %lu.",
		this->name,
		path,
		columns.size(),
		d->GetLatestSequenceNumber()
	};
}
catch(const error &e)
{
	log::error
	{
		"Error opening db [%s] %s",
		name,
		e.what()
	};

	throw;
}
catch(const std::exception &e)
{
	log::error
	{
		"Error opening db [%s] %s",
		name,
		e.what()
	};

	throw error
	{
		"Failed to open db [%s] %s",
		name,
		e.what()
	};
}

ircd::db::database::~database()
noexcept try
{
	const ctx::uninterruptible::nothrow ui;
	const std::unique_lock lock{write_mutex};
	log::info
	{
		log, "[%s] closing database @ `%s'...",
		name,
		path
	};

	if(likely(prefetcher))
	{
		const size_t canceled
		{
			prefetcher->cancel(*this)
		};

		log::debug
		{
			log, "[%s] canceled %zu queued prefetches; waiting for any pending ...",
			name,
			canceled,
		};

		// prefetcher::cancel() only removes requests from its queue, but if
		// a prefetch request from this database is in flight that is bad; so
		// we wait until the unit has completed its pending requests.
		prefetcher->wait_pending();
	}

	bgcancel(*this, true);

	log::debug
	{
		log, "[%s] closing columns...",
		name
	};

	this->checkpointer.reset(nullptr);
	this->column_names.clear();
	this->column_index.clear();
	this->columns.clear();
	log::debug
	{
		log, "[%s] closed columns; flushing...",
		name
	};

	if(!read_only)
		flush(*this);

	log::debug
	{
		log, "[%s] flushed; synchronizing...",
		name
	};

	if(!read_only)
		sync(*this);

	log::debug
	{
		log, "[%s] synchronized with hardware.",
		name
	};

	const auto sequence
	{
		d->GetLatestSequenceNumber()
	};

	throw_on_error
	{
		d->Close()
	};

	env->st.reset(nullptr);

	log::info
	{
		log, "[%s] closed database @ `%s' at sequence number %lu.",
		name,
		path,
		sequence
	};
}
catch(const std::exception &e)
{
	log::error
	{
		log, "Error closing database(%p) :%s",
		this,
		e.what()
	};

	return;
}
catch(...)
{
	log::critical
	{
		log, "Unknown error closing database(%p)",
		this
	};

	return;
}

void
ircd::db::database::operator()(const delta &delta)
{
	operator()(sopts{}, delta);
}

void
ircd::db::database::operator()(const std::initializer_list<delta> &deltas)
{
	operator()(sopts{}, deltas);
}

void
ircd::db::database::operator()(const delta *const &begin,
                               const delta *const &end)
{
	operator()(sopts{}, begin, end);
}

void
ircd::db::database::operator()(const sopts &sopts,
                               const delta &delta)
{
	operator()(sopts, &delta, &delta + 1);
}

void
ircd::db::database::operator()(const sopts &sopts,
                               const std::initializer_list<delta> &deltas)
{
	operator()(sopts, std::begin(deltas), std::end(deltas));
}

void
ircd::db::database::operator()(const sopts &sopts,
                               const delta *const &begin,
                               const delta *const &end)
{
	rocksdb::WriteBatch batch;
	std::for_each(begin, end, [this, &batch]
	(const delta &delta)
	{
		const auto &op(std::get<op>(delta));
		const auto &col(std::get<1>(delta));
		const auto &key(std::get<2>(delta));
		const auto &val(std::get<3>(delta));
		db::column column(operator[](col));
		append(batch, column, db::column::delta
		{
			op,
			key,
			val
		});
	});

	commit(*this, batch, sopts);
}

ircd::db::database::column &
ircd::db::database::operator[](const string_view &name)
{
	return operator[](cfid(name));
}

ircd::db::database::column &
ircd::db::database::operator[](const uint32_t &id)
try
{
	auto &ret(*column_index.at(id));
	assert(db::id(ret) == id);
	return ret;
}
catch(const std::out_of_range &e)
{
	throw not_found
	{
		"[%s] column id[%u] is not available or specified in schema",
		this->name,
		id
	};
}

const ircd::db::database::column &
ircd::db::database::operator[](const string_view &name)
const
{
	return operator[](cfid(name));
}

const ircd::db::database::column &
ircd::db::database::operator[](const uint32_t &id)
const try
{
	auto &ret(*column_index.at(id));
	assert(db::id(ret) == id);
	return ret;
}
catch(const std::out_of_range &e)
{
	throw not_found
	{
		"[%s] column id[%u] is not available or specified in schema",
		this->name,
		id
	};
}

uint32_t
ircd::db::database::cfid(const string_view &name)
const
{
	const int32_t id
	{
		cfid(std::nothrow, name)
	};

	if(id < 0)
		throw not_found
		{
			"[%s] column '%s' is not available or specified in schema",
			this->name,
			name
		};

	return id;
}

int32_t
ircd::db::database::cfid(const std::nothrow_t,
                         const string_view &name)
const
{
	const auto it{column_names.find(name)};
	return it != std::end(column_names)?
		db::id(*it->second):
		-1;
}

///////////////////////////////////////////////////////////////////////////////
//
// database::column
//

void
ircd::db::drop(database::column &c)
{
	if(!c.handle)
		return;

	database &d(c);
	log::debug
	{
		log, "[%s]'%s' @%lu DROPPING COLUMN",
		name(d),
		name(c),
		sequence(d)
	};

	throw_on_error
	{
		c.d->d->DropColumnFamily(c.handle.get())
	};

	log::notice
	{
		log, "[%s]'%s' @%lu DROPPED COLUMN",
		name(d),
		name(c),
		sequence(d)
	};
}

bool
ircd::db::dropped(const database::column &c)
{
	return c.descriptor?
		c.descriptor->drop:
		true;
}

uint32_t
ircd::db::id(const database::column &c)
{
	if(!c.handle)
		return -1;

	return c.handle->GetID();
}

const std::string &
ircd::db::name(const database::column &c)
{
	return c.name;
}

const ircd::db::descriptor &
ircd::db::describe(const database::column &c)
{
	assert(c.descriptor);
	return *c.descriptor;
}

//
// database::column
//

ircd::db::database::column::column(database &d,
                                   db::descriptor &descriptor)
:rocksdb::ColumnFamilyDescriptor
(
	descriptor.name, db::options{descriptor.options}
)
,d{&d}
,descriptor{&descriptor}
,key_type{this->descriptor->type.first}
,mapped_type{this->descriptor->type.second}
,cmp{this->d, this->descriptor->cmp}
,prefix{this->d, this->descriptor->prefix}
,cfilter{this, this->descriptor->compactor}
,stats
{
	descriptor.name != "default"s?
		std::make_shared<struct database::stats>(this->d, this):
		this->d->stats
}
,allocator
{
	#ifdef IRCD_DB_HAS_ALLOCATOR
	std::make_shared<struct database::allocator>(this->d, this, database::allocator::cache_arena, descriptor.block_size)
	#endif
}
,handle
{
	nullptr, [&d](rocksdb::ColumnFamilyHandle *const handle)
	{
		assert(d.d);
		if(handle && d.d)
			d.d->DestroyColumnFamilyHandle(handle);
	}
}
{
	// If possible, deduce comparator based on type given in descriptor
	if(!this->descriptor->cmp.less)
	{
		if(key_type == typeid(string_view))
			this->cmp.user = cmp_string_view{};
		else if(key_type == typeid(int64_t))
			this->cmp.user = cmp_int64_t{};
		else if(key_type == typeid(uint64_t))
			this->cmp.user = cmp_uint64_t{};
		else
			throw error
			{
				"column '%s' key type[%s] requires user supplied comparator",
				this->name,
				key_type.name()
			};
	}

	// Set the key comparator
	this->options.comparator = &this->cmp;

	// Set the prefix extractor
	if(this->prefix.user.get && this->prefix.user.has)
		this->options.prefix_extractor = std::shared_ptr<const rocksdb::SliceTransform>
		{
			&this->prefix, [](const rocksdb::SliceTransform *) {}
		};

	// Set the insert hint prefix extractor
	if(this->options.prefix_extractor)
		this->options.memtable_insert_with_hint_prefix_extractor = this->options.prefix_extractor;

	// Set the compaction filter
	this->options.compaction_filter = &this->cfilter;

	//this->options.paranoid_file_checks = true;

	// More stats reported by the rocksdb.stats property.
	this->options.report_bg_io_stats = true;

	// Set filter reductions for this column. This means we expect a key to exist.
	this->options.optimize_filters_for_hits = this->descriptor->expect_queries_hit;

	// Compression type
	this->options.compression = find_supported_compression(this->descriptor->compression);
	//this->options.compression = rocksdb::kNoCompression;

	// Compression options
	this->options.compression_opts.enabled = true;
	this->options.compression_opts.max_dict_bytes = 0;//8_MiB;

	// Mimic the above for bottommost compression.
	//this->options.bottommost_compression = this->options.compression;
	//this->options.bottommost_compression_opts = this->options.compression_opts;

	//TODO: descriptor / conf

	this->options.write_buffer_size = 4_MiB;
	this->options.max_write_buffer_number = 8;
	this->options.min_write_buffer_number_to_merge = 4;
	this->options.max_write_buffer_number_to_maintain = 0;

	// Conf item can be set to disable automatic compactions. For developers
	// and debugging; good for valgrind.
	this->options.disable_auto_compactions = !bool(db::auto_compact);

	// Set the compaction style; we don't override this in the descriptor yet.
	//this->options.compaction_style = rocksdb::kCompactionStyleNone;
	this->options.compaction_style = rocksdb::kCompactionStyleLevel;
	//this->options.compaction_style = rocksdb::kCompactionStyleUniversal;

	// Set the compaction priority from string in the descriptor
	this->options.compaction_pri =
		this->descriptor->compaction_pri == "kByCompensatedSize"?
			rocksdb::CompactionPri::kByCompensatedSize:
		this->descriptor->compaction_pri == "kMinOverlappingRatio"?
			rocksdb::CompactionPri::kMinOverlappingRatio:
		this->descriptor->compaction_pri == "kOldestSmallestSeqFirst"?
			rocksdb::CompactionPri::kOldestSmallestSeqFirst:
		this->descriptor->compaction_pri == "kOldestLargestSeqFirst"?
			rocksdb::CompactionPri::kOldestLargestSeqFirst:
			rocksdb::CompactionPri::kOldestLargestSeqFirst;

	this->options.num_levels = 7;
	this->options.level0_file_num_compaction_trigger = 2;
	this->options.level_compaction_dynamic_level_bytes = false;
	this->options.ttl = 0;
	#ifdef IRCD_DB_HAS_PERIODIC_COMPACTIONS
	this->options.periodic_compaction_seconds = this->descriptor->compaction_period.count();
	#endif

	this->options.target_file_size_base = this->descriptor->target_file_size.base;
	this->options.target_file_size_multiplier = this->descriptor->target_file_size.multiplier;

	this->options.max_bytes_for_level_base = this->descriptor->max_bytes_for_level[0].base;
	this->options.max_bytes_for_level_multiplier = this->descriptor->max_bytes_for_level[0].multiplier;
	this->options.max_bytes_for_level_multiplier_additional = std::vector<int>(this->options.num_levels, 1);
	{
		auto &dst(this->options.max_bytes_for_level_multiplier_additional);
		const auto &src(this->descriptor->max_bytes_for_level);
		const size_t src_size(std::distance(begin(src) + 1, std::end(src)));
		assert(src_size >= 1);
		const auto end
		{
			begin(src) + 1 + std::min(dst.size(), src_size)
		};

		std::transform(begin(src) + 1, end, begin(dst), []
		(const auto &mbfl)
		{
			return mbfl.multiplier;
		});
	}

	// Universal compaction options; these are unused b/c we don't use this
	// style; they are here for hacking an experimentation for now.
	this->options.compaction_options_universal.size_ratio = 1;
	this->options.compaction_options_universal.min_merge_width = 2;
	this->options.compaction_options_universal.max_merge_width = UINT_MAX;
	this->options.compaction_options_universal.max_size_amplification_percent = 200;
	this->options.compaction_options_universal.compression_size_percent = -1;
	this->options.compaction_options_universal.stop_style = rocksdb::kCompactionStopStyleTotalSize;
	this->options.compaction_options_universal.allow_trivial_move = false;;

	//
	// Table options
	//

	// Block based table index type.
	if constexpr(ROCKSDB_MAJOR > 6 || (ROCKSDB_MAJOR == 6 && ROCKSDB_MINOR >= 6))
		table_opts.format_version = 5; // RocksDB >= 6.6.x compat only; otherwise use 4
	else
		table_opts.format_version = 4; // RocksDB >= 5.16.x compat only; otherwise use 3.

	table_opts.index_type = rocksdb::BlockBasedTableOptions::kTwoLevelIndexSearch;
	table_opts.index_block_restart_interval = 64;
	table_opts.read_amp_bytes_per_bit = 8;

	// Specify that index blocks should use the cache. If not, they will be
	// pre-read into RAM by rocksdb internally. Because of the above
	// TwoLevelIndex + partition_filters configuration on RocksDB v5.15 it's
	// better to use pre-read except in the case of a massive database.
	table_opts.cache_index_and_filter_blocks = true;
	table_opts.cache_index_and_filter_blocks_with_high_priority = true;
	table_opts.pin_top_level_index_and_filter = false;
	table_opts.pin_l0_filter_and_index_blocks_in_cache = false;
	table_opts.partition_filters = true;
	table_opts.use_delta_encoding = false;

	// Determine whether the index for this column should be compressed.
	const bool is_string_index(this->descriptor->type.first == typeid(string_view));
	const bool is_compression(this->options.compression != rocksdb::kNoCompression);
	table_opts.enable_index_compression = is_compression; //&& is_string_index;

	// Setup the block size
	table_opts.block_size = this->descriptor->block_size;
	table_opts.metadata_block_size = this->descriptor->meta_block_size;
	table_opts.block_size_deviation = 50;
	table_opts.block_restart_interval = 64;

	//table_opts.data_block_index_type = rocksdb::BlockBasedTableOptions::kDataBlockBinaryAndHash;
	//table_opts.data_block_hash_table_util_ratio = 0.75;

	// Block alignment doesn't work if compression is enabled for this
	// column. If not, we want block alignment for direct IO.
	table_opts.block_align = this->options.compression == rocksdb::kNoCompression;

	// Setup the cache for assets.
	const auto &cache_size(this->descriptor->cache_size);
	if(cache_size != 0)
		table_opts.block_cache = std::make_shared<database::cache>(this->d, this->stats, this->allocator, this->name, cache_size);

	// RocksDB will create an 8_MiB block_cache if we don't create our own.
	// To honor the user's desire for a zero-size cache, this must be set.
	if(!table_opts.block_cache)
	{
		table_opts.no_block_cache = true;
		table_opts.cache_index_and_filter_blocks = false; // MBZ or error w/o block_cache
	}

	// Setup the cache for compressed assets.
	const auto &cache_size_comp(this->descriptor->cache_size_comp);
	if(cache_size_comp != 0)
		table_opts.block_cache_compressed = std::make_shared<database::cache>(this->d, this->stats, this->allocator, this->name, cache_size_comp);

	// Setup the bloom filter.
	const auto &bloom_bits(this->descriptor->bloom_bits);
	if(bloom_bits)
		table_opts.filter_policy.reset(rocksdb::NewBloomFilterPolicy(bloom_bits, false));

	// Tickers::READ_AMP_TOTAL_READ_BYTES / Tickers::READ_AMP_ESTIMATE_USEFUL_BYTES
	//table_opts.read_amp_bytes_per_bit = 8;

	// Finally set the table options in the column options.
	this->options.table_factory.reset(rocksdb::NewBlockBasedTableFactory(table_opts));

	log::debug
	{
		log, "schema '%s' column [%s => %s] cmp[%s] pfx[%s] lru:%s:%s bloom:%zu compression:%d %s",
		db::name(d),
		demangle(key_type.name()),
		demangle(mapped_type.name()),
		this->cmp.Name(),
		this->options.prefix_extractor? this->prefix.Name() : "none",
		cache_size? "YES": "NO",
		cache_size_comp? "YES": "NO",
		bloom_bits,
		int(this->options.compression),
		this->descriptor->name
	};
}

ircd::db::database::column::~column()
noexcept
{
}

ircd::db::database::column::operator
database &()
{
	return *d;
}

ircd::db::database::column::operator
rocksdb::ColumnFamilyHandle *()
{
	return handle.get();
}

ircd::db::database::column::operator
const database &()
const
{
	return *d;
}

ircd::db::database::column::operator
const rocksdb::ColumnFamilyHandle *()
const
{
	return handle.get();
}

ircd::db::database::column::operator
const rocksdb::ColumnFamilyOptions &()
const
{
	return options;
}

///////////////////////////////////////////////////////////////////////////////
//
// database::comparator
//

ircd::db::database::comparator::comparator(database *const &d,
                                           db::comparator user)
:d{d}
,user
{
	std::move(user)
}
{
}

const char *
ircd::db::database::comparator::Name()
const noexcept
{
	assert(!user.name.empty());
	return user.name.data();
}

bool
ircd::db::database::comparator::Equal(const Slice &a,
                                      const Slice &b)
const noexcept
{
	return user.equal?
		user.equal(slice(a), slice(b)):
		Compare(a, b) == 0;
}

int
ircd::db::database::comparator::Compare(const Slice &a,
                                        const Slice &b)
const noexcept
{
	assert(bool(user.less));
	const auto sa{slice(a)};
	const auto sb{slice(b)};
	return user.less(sa, sb)?                -1:  // less[Y], equal[?], greater[?]
	       user.equal && user.equal(sa, sb)?  0:  // less[N], equal[Y], greater[?]
	       user.equal?                        1:  // less[N], equal[N], greater[Y]
	       user.less(sb, sa)?                 1:  // less[N], equal[?], greater[Y]
	                                          0;  // less[N], equal[Y], greater[N]
}

void
ircd::db::database::comparator::FindShortestSeparator(std::string *const key,
                                                      const Slice &limit)
const noexcept
{
	assert(key != nullptr);
	if(user.separator)
		user.separator(*key, slice(limit));
}

void
ircd::db::database::comparator::FindShortSuccessor(std::string *const key)
const noexcept
{
	assert(key != nullptr);
	if(user.successor)
		user.successor(*key);
}

bool
ircd::db::database::comparator::IsSameLengthImmediateSuccessor(const Slice &s,
                                                               const Slice &t)
const noexcept
{
	return rocksdb::Comparator::IsSameLengthImmediateSuccessor(s, t);
}

bool
ircd::db::database::comparator::CanKeysWithDifferentByteContentsBeEqual()
const noexcept
{
	// When keys with different byte contents can be equal the keys are
	// not hashable.
	return !user.hashable;
}

///////////////////////////////////////////////////////////////////////////////
//
// database::prefix_transform
//

const char *
ircd::db::database::prefix_transform::Name()
const noexcept
{
	assert(!user.name.empty());
	return user.name.c_str();
}

rocksdb::Slice
ircd::db::database::prefix_transform::Transform(const Slice &key)
const noexcept
{
	assert(bool(user.get));
	return slice(user.get(slice(key)));
}

bool
ircd::db::database::prefix_transform::InRange(const Slice &key)
const noexcept
{
	return InDomain(key);
}

bool
ircd::db::database::prefix_transform::InDomain(const Slice &key)
const noexcept
{
	assert(bool(user.has));
	return user.has(slice(key));
}

///////////////////////////////////////////////////////////////////////////////
//
// database::snapshot
//

uint64_t
ircd::db::sequence(const database::snapshot &s)
{
	const rocksdb::Snapshot *const rs(s);
	return sequence(rs);
}

uint64_t
ircd::db::sequence(const rocksdb::Snapshot *const &rs)
{
	return likely(rs)? rs->GetSequenceNumber() : 0ULL;
}

//
// snapshot::shapshot
//

ircd::db::database::snapshot::snapshot(database &d)
:s
{
	!d.slave?
		d.d->GetSnapshot():
		nullptr,

	[dp(weak_from(d))](const rocksdb::Snapshot *const s)
	{
		if(!s)
			return;

		const auto d(dp.lock());
		d->d->ReleaseSnapshot(s);
	}
}
{
}

ircd::db::database::snapshot::~snapshot()
noexcept
{
}

///////////////////////////////////////////////////////////////////////////////
//
// database::logger
//

ircd::db::database::logger::logger(database *const &d)
:rocksdb::Logger{}
,d{d}
{
}

ircd::db::database::logger::~logger()
noexcept
{
}

rocksdb::Status
ircd::db::database::logger::Close()
noexcept
{
	return rocksdb::Status::NotSupported();
}

static
ircd::log::level
translate(const rocksdb::InfoLogLevel &level)
{
	switch(level)
	{
		// Treat all infomational messages from rocksdb as debug here for now.
		// We can clean them up and make better reports for our users eventually.
		default:
		case rocksdb::InfoLogLevel::DEBUG_LEVEL:     return ircd::log::level::DEBUG;
		case rocksdb::InfoLogLevel::INFO_LEVEL:      return ircd::log::level::DEBUG;

		case rocksdb::InfoLogLevel::WARN_LEVEL:      return ircd::log::level::WARNING;
		case rocksdb::InfoLogLevel::ERROR_LEVEL:     return ircd::log::level::ERROR;
		case rocksdb::InfoLogLevel::FATAL_LEVEL:     return ircd::log::level::CRITICAL;
		case rocksdb::InfoLogLevel::HEADER_LEVEL:    return ircd::log::level::NOTICE;
	}
}

void
ircd::db::database::logger::Logv(const char *const fmt,
                                 va_list ap)
noexcept
{
	Logv(rocksdb::InfoLogLevel::DEBUG_LEVEL, fmt, ap);
}

void
ircd::db::database::logger::LogHeader(const char *const fmt,
                                      va_list ap)
noexcept
{
	Logv(rocksdb::InfoLogLevel::DEBUG_LEVEL, fmt, ap);
}

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wsuggest-attribute=format"
#ifdef __clang__
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wformat-nonliteral"
#endif __clang__
void
ircd::db::database::logger::Logv(const rocksdb::InfoLogLevel level_,
                                 const char *const fmt,
                                 va_list ap)
noexcept
{
	if(level_ < GetInfoLogLevel())
		return;

	const log::level level
	{
		translate(level_)
	};

	if(level > RB_LOG_LEVEL)
		return;

	thread_local char buf[1024]; const auto len
	{
		vsnprintf(buf, sizeof(buf), fmt, ap)
	};

	const auto str
	{
		// RocksDB adds annoying leading whitespace to attempt to right-justify things and idc
		lstrip(string_view{buf, size_t(len)}, ' ')
	};

	// Skip the options for now
	if(startswith(str, "Options"))
		return;

	rog(level, "[%s] %s", d->name, str);
}
#ifdef __clang__
#pragma clang diagnostic pop
#endif __clang__
#pragma GCC diagnostic pop

///////////////////////////////////////////////////////////////////////////////
//
// database::mergeop
//

ircd::db::database::mergeop::mergeop(database *const &d,
                                     merge_closure merger)
:d{d}
,merger
{
	merger?
		std::move(merger):
		ircd::db::merge_operator
}
{
}

ircd::db::database::mergeop::~mergeop()
noexcept
{
}

const char *
ircd::db::database::mergeop::Name()
const noexcept
{
	return "<unnamed>";
}

bool
ircd::db::database::mergeop::Merge(const rocksdb::Slice &_key,
                                   const rocksdb::Slice *const _exist,
                                   const rocksdb::Slice &_update,
                                   std::string *const newval,
                                   rocksdb::Logger *const)
const noexcept try
{
	const string_view key
	{
		_key.data(), _key.size()
	};

	const string_view exist
	{
		_exist? string_view { _exist->data(), _exist->size() } : string_view{}
	};

	const string_view update
	{
		_update.data(), _update.size()
	};

	if(exist.empty())
	{
		*newval = std::string(update);
		return true;
	}

	//XXX caching opportunity?
	*newval = merger(key, {exist, update});   // call the user
	return true;
}
catch(const std::bad_function_call &e)
{
	log::critical
	{
		log, "merge: missing merge operator (%s)", e
	};

	return false;
}
catch(const std::exception &e)
{
	log::error
	{
		log, "merge: %s", e
	};

	return false;
}

///////////////////////////////////////////////////////////////////////////////
//
// database::stats (db/database/stats.h) internal
//

namespace ircd::db
{
	static thread_local char database_stats_name_buf[128];
}

//
// stats::stats
//

ircd::db::database::stats::stats(database *const &d,
                                 database::column *const &c)
:d{d}
,c{c}
,get_copied
{
	{ "name", make_name("get.copied")                                   },
	{ "desc", "Number of DB::Get() results violating zero-copy."        },
}
,get_referenced
{
	{ "name", make_name("get.referenced")                               },
	{ "desc", "Number of DB::Get() results adhering to zero-copy."      },
}
,multiget_copied
{
	{ "name", make_name("multiget.copied")                              },
	{ "desc", "Number of DB::MultiGet() results violating zero-copy."   },
}
,multiget_referenced
{
	{ "name", make_name("multiget.referenced")                          },
	{ "desc", "Number of DB::MultiGet() results adhering to zero-copy." },
}
{
	assert(item.size() == ticker.size());
	for(size_t i(0); i < item.size(); ++i)
	{
		const auto &[id, ticker_name]
		{
			rocksdb::TickersNameMap[i]
		};

		assert(id == i);
		new (item.data() + i) ircd::stats::item<uint64_t *>
		{
			std::addressof(ticker[i]), json::members
			{
				{ "name", make_name(ticker_name)                },
				{ "desc", "RocksDB library statistics counter." },
			}
		};
	}
}

ircd::db::database::stats::~stats()
noexcept
{
}

rocksdb::Status
ircd::db::database::stats::Reset()
noexcept
{
	ticker.fill(0);
	histogram.fill({0.0});
	return rocksdb::Status::OK();
}

bool
ircd::db::database::stats::HistEnabledForType(const uint32_t type)
const noexcept
{
	return type < histogram.size();
}

void
ircd::db::database::stats::measureTime(const uint32_t type,
                                       const uint64_t time)
noexcept
{
	auto &data(histogram.at(type));

	data.time += time;
	data.hits++;

	data.max = std::max(data.max, double(time));
	data.avg = data.time / static_cast<long double>(data.hits);
}

void
ircd::db::database::stats::histogramData(const uint32_t type,
                                         rocksdb::HistogramData *const data)
const noexcept
{
	assert(data);
	const auto &h
	{
		histogram.at(type)
	};

	data->median = h.median;
	data->percentile95 = h.pct95;
	data->percentile99 = h.pct99;
	data->average = h.avg;
	data->standard_deviation = h.stddev;
	data->max = h.max;
}

void
ircd::db::database::stats::recordTick(const uint32_t type,
                                      const uint64_t count)
noexcept
{
	ticker.at(type) += count;
}

void
ircd::db::database::stats::setTickerCount(const uint32_t type,
                                          const uint64_t count)
noexcept
{
	ticker.at(type) = count;
}

uint64_t
ircd::db::database::stats::getAndResetTickerCount(const uint32_t type)
noexcept
{
	const auto ret(getTickerCount(type));
	setTickerCount(type, 0);
	return ret;
}

uint64_t
ircd::db::database::stats::getTickerCount(const uint32_t type)
const noexcept
{
	return ticker.at(type);
}

ircd::string_view
ircd::db::database::stats::make_name(const string_view &ticker_name)
const
{
	assert(this->d);
	return fmt::sprintf
	{
		database_stats_name_buf, "ircd.db.%s.%s.%s",
		db::name(*d),
		c? db::name(*c): "db"s,
		ticker_name,
	};
}

//
// database::stats::passthru
//

ircd::db::database::stats::passthru::passthru(rocksdb::Statistics *const &a,
                                              rocksdb::Statistics *const &b)
:pass
{
	{ a, b }
}
{
}

ircd::db::database::stats::passthru::~passthru()
noexcept
{
}

rocksdb::Status
__attribute__((noreturn))
ircd::db::database::stats::passthru::Reset()
noexcept
{
	ircd::terminate
	{
		"Unavailable for passthru"
	};

	__builtin_unreachable();
}

void
ircd::db::database::stats::passthru::recordTick(const uint32_t tickerType,
                                                const uint64_t count)
noexcept
{
	for(auto *const &pass : this->pass)
		pass->recordTick(tickerType, count);
}

void
ircd::db::database::stats::passthru::measureTime(const uint32_t histogramType,
                                                 const uint64_t time)
noexcept
{
	for(auto *const &pass : this->pass)
		pass->measureTime(histogramType, time);
}

bool
ircd::db::database::stats::passthru::HistEnabledForType(const uint32_t type)
const noexcept
{
	return std::all_of(begin(pass), end(pass), [&type]
	(const auto *const &pass)
	{
		return pass->HistEnabledForType(type);
	});
}

uint64_t
__attribute__((noreturn))
ircd::db::database::stats::passthru::getTickerCount(const uint32_t tickerType)
const noexcept
{
	ircd::terminate
	{
		"Unavailable for passthru"
	};

	__builtin_unreachable();
}

void
__attribute__((noreturn))
ircd::db::database::stats::passthru::setTickerCount(const uint32_t tickerType,
                                                    const uint64_t count)
noexcept
{
	ircd::terminate
	{
		"Unavailable for passthru"
	};

	__builtin_unreachable();
}

void
__attribute__((noreturn))
ircd::db::database::stats::passthru::histogramData(const uint32_t type,
                                                   rocksdb::HistogramData *const data)
const noexcept
{
	ircd::terminate
	{
		"Unavailable for passthru"
	};

	__builtin_unreachable();
}

uint64_t
__attribute__((noreturn))
ircd::db::database::stats::passthru::getAndResetTickerCount(const uint32_t tickerType)
noexcept
{
	ircd::terminate
	{
		"Unavailable for passthru"
	};

	__builtin_unreachable();
}

///////////////////////////////////////////////////////////////////////////////
//
// database::events
//

void
ircd::db::database::events::OnFlushCompleted(rocksdb::DB *const db,
                                             const rocksdb::FlushJobInfo &info)
noexcept
{
	log::info
	{
		log, "[%s] job:%d ctx:%lu flush ended writes[slow:%d stop:%d] seq[%zu -> %zu] %s '%s' `%s'",
		d->name,
		info.job_id,
		info.thread_id,
		info.triggered_writes_slowdown,
		info.triggered_writes_stop,
		info.smallest_seqno,
		info.largest_seqno,
		reflect(info.flush_reason),
		info.cf_name,
		info.file_path,
	};

	assert(info.thread_id == ctx::id(*ctx::current));
}

void
ircd::db::database::events::OnFlushBegin(rocksdb::DB *const db,
                                         const rocksdb::FlushJobInfo &info)
noexcept
{
	log::info
	{
		log, "[%s] job:%d ctx:%lu flush start writes[slow:%d stop:%d] seq[%zu -> %zu] %s '%s'",
		d->name,
		info.job_id,
		info.thread_id,
		info.triggered_writes_slowdown,
		info.triggered_writes_stop,
		info.smallest_seqno,
		info.largest_seqno,
		reflect(info.flush_reason),
		info.cf_name,
	};

	assert(info.thread_id == ctx::id(*ctx::current));
}

void
ircd::db::database::events::OnCompactionCompleted(rocksdb::DB *const db,
                                                  const rocksdb::CompactionJobInfo &info)
noexcept
{
	const log::level level
	{
		info.status == rocksdb::Status::OK()?
			log::level::INFO:
			log::level::ERROR
	};

	log::logf
	{
		log, level,
		"[%s] job:%d ctx:%lu compacted level[%d -> %d] files[%zu -> %zu] %s '%s' (%d): %s",
		d->name,
		info.job_id,
		info.thread_id,
		info.base_input_level,
		info.output_level,
		info.input_files.size(),
		info.output_files.size(),
		reflect(info.compaction_reason),
		info.cf_name,
		int(info.status.code()),
		info.status.getState()?: "OK",
	};

	const bool bytes_same
	{
		info.stats.total_input_bytes == info.stats.total_output_bytes
	};

	log::debug
	{
		log, "[%s] job:%d keys[in:%zu out:%zu upd:%zu] bytes[%s -> %s] falloc:%s write:%s rsync:%s fsync:%s total:%s",
		d->name,
		info.job_id,
		info.stats.num_input_records,
		info.stats.num_output_records,
		info.stats.num_records_replaced,
		pretty(iec(info.stats.total_input_bytes)),
		bytes_same? "same": pretty(iec(info.stats.total_output_bytes)),
		pretty(nanoseconds(info.stats.file_prepare_write_nanos), true),
		pretty(nanoseconds(info.stats.file_write_nanos), true),
		pretty(nanoseconds(info.stats.file_range_sync_nanos), true),
		pretty(nanoseconds(info.stats.file_fsync_nanos), true),
		pretty(microseconds(info.stats.elapsed_micros), true),
	};

	if(info.stats.num_corrupt_keys > 0)
		log::error
		{
			log, "[%s] job:%d reported %lu corrupt keys.",
			d->name,
			info.job_id,
			info.stats.num_corrupt_keys
		};

	assert(info.thread_id == ctx::id(*ctx::current));
}

void
ircd::db::database::events::OnTableFileDeleted(const rocksdb::TableFileDeletionInfo &info)
noexcept
{
	const log::level level
	{
		info.status == rocksdb::Status::OK()?
			log::level::DEBUG:
			log::level::ERROR
	};

	log::logf
	{
		log, level,
		"[%s] job:%d table file delete [%s][%s] (%d): %s",
		d->name,
		info.job_id,
		info.db_name,
		lstrip(info.file_path, info.db_name),
		int(info.status.code()),
		info.status.getState()?: "OK",
	};
}

void
ircd::db::database::events::OnTableFileCreated(const rocksdb::TableFileCreationInfo &info)
noexcept
{
	const log::level level
	{
		info.status == rocksdb::Status::OK()?
			log::level::DEBUG:
			log::level::ERROR
	};

	log::logf
	{
		log, level,
		"[%s] job:%d table file closed [%s][%s] size:%s '%s' (%d): %s",
		d->name,
		info.job_id,
		info.db_name,
		lstrip(info.file_path, info.db_name),
		pretty(iec(info.file_size)),
		info.cf_name,
		int(info.status.code()),
		info.status.getState()?: "OK",
	};

	log::debug
	{
		log, "[%s] job:%d head[%s] index[%s] filter[%s] data[%lu %s] keys[%lu %s] vals[%s] %s",
		d->name,
		info.job_id,
		pretty(iec(info.table_properties.top_level_index_size)),
		pretty(iec(info.table_properties.index_size)),
		pretty(iec(info.table_properties.filter_size)),
		info.table_properties.num_data_blocks,
		pretty(iec(info.table_properties.data_size)),
		info.table_properties.num_entries,
		pretty(iec(info.table_properties.raw_key_size)),
		pretty(iec(info.table_properties.raw_value_size)),
		info.table_properties.compression_name
	};
}

void
ircd::db::database::events::OnTableFileCreationStarted(const rocksdb::TableFileCreationBriefInfo &info)
noexcept
{
	log::debug
	{
		log, "[%s] job:%d table file opened [%s][%s] '%s'",
		d->name,
		info.job_id,
		info.db_name,
		lstrip(info.file_path, info.db_name),
		info.cf_name,
	};
}

void
ircd::db::database::events::OnMemTableSealed(const rocksdb::MemTableInfo &info)
noexcept
{
	log::debug
	{
		log, "[%s] memory table sealed '%s' entries:%lu deletes:%lu",
		d->name,
		info.cf_name,
		info.num_entries,
		info.num_deletes
	};
}

void
ircd::db::database::events::OnColumnFamilyHandleDeletionStarted(rocksdb::ColumnFamilyHandle *const h)
noexcept
{
	log::debug
	{
		log, "[%s] column[%s] handle closing @ %p",
		d->name,
		h->GetName(),
		h
	};
}

void
ircd::db::database::events::OnExternalFileIngested(rocksdb::DB *const d,
                                                   const rocksdb::ExternalFileIngestionInfo &info)
noexcept
{
	log::notice
	{
		log, "[%s] external file ingested column[%s] external[%s] internal[%s] sequence:%lu",
		this->d->name,
		info.cf_name,
		info.external_file_path,
		info.internal_file_path,
		info.global_seqno
	};
}

void
ircd::db::database::events::OnBackgroundError(rocksdb::BackgroundErrorReason reason,
                                              rocksdb::Status *const status)
noexcept
{
	assert(d);
	assert(status);

	thread_local char buf[1024];
	const string_view str{fmt::sprintf
	{
		buf, "%s error in %s :%s",
		reflect(status->severity()),
		reflect(reason),
		status->ToString()
	}};

	// This is a legitimate when we want to use it. If the error is not
	// suppressed the DB will enter read-only mode and will require a
	// call to db::resume() to clear the error (i.e by admin at console).
	const bool ignore
	{
		false
	};

	const log::level fac
	{
		ignore?
			log::level::DERROR:
		status->severity() == rocksdb::Status::kFatalError?
			log::level::CRITICAL:
			log::level::ERROR
	};

	log::logf
	{
		log, fac, "[%s] %s", d->name, str
	};

	if(ignore)
	{
		*status = rocksdb::Status::OK();
		return;
	}

	// Downgrade select fatal errors to hard errors. If this downgrade
	// does not occur then it can never be cleared by a db::resume() and
	// the daemon must be restarted.

	if(reason == rocksdb::BackgroundErrorReason::kCompaction)
		if(status->severity() == rocksdb::Status::kFatalError)
			*status = rocksdb::Status(*status, rocksdb::Status::kHardError);

	// Save the error string to the database instance for later examination.
	d->errors.emplace_back(str);
}

void
ircd::db::database::events::OnStallConditionsChanged(const rocksdb::WriteStallInfo &info)
noexcept
{
	const auto level
	{
		info.condition.cur == rocksdb::WriteStallCondition::kNormal?
			log::level::INFO:
			log::level::WARNING
	};

	log::logf
	{
		log, level,
		"'%s' stall condition column[%s] %s -> %s",
		d->name,
		info.cf_name,
		reflect(info.condition.prev),
		reflect(info.condition.cur)
	};
}

///////////////////////////////////////////////////////////////////////////////
//
// database::cache (internal)
//

decltype(ircd::db::database::cache::DEFAULT_SHARD_BITS)
ircd::db::database::cache::DEFAULT_SHARD_BITS
(
	std::log2(std::min(size_t(db::request_pool_size), 16UL))
);

decltype(ircd::db::database::cache::DEFAULT_STRICT)
ircd::db::database::cache::DEFAULT_STRICT
{
	false
};

decltype(ircd::db::database::cache::DEFAULT_HI_PRIO)
ircd::db::database::cache::DEFAULT_HI_PRIO
{
	0.25
};

//
// cache::cache
//

ircd::db::database::cache::cache(database *const &d,
                                 std::shared_ptr<struct database::stats> stats,
                                 std::shared_ptr<struct database::allocator> allocator,
                                 std::string name,
                                 const ssize_t &initial_capacity)
#ifdef IRCD_DB_HAS_ALLOCATOR
:rocksdb::Cache{allocator}
,d{d}
#else
:d{d}
#endif
,name{std::move(name)}
,stats{std::move(stats)}
,allocator{std::move(allocator)}
,c{rocksdb::NewLRUCache(rocksdb::LRUCacheOptions
{
	size_t(std::max(initial_capacity, ssize_t(0)))
	,DEFAULT_SHARD_BITS
	,DEFAULT_STRICT
	,DEFAULT_HI_PRIO
	#ifdef IRCD_DB_HAS_ALLOCATOR
	,this->allocator
	#endif
})}
{
	assert(bool(c));
	#ifdef IRCD_DB_HAS_ALLOCATOR
	assert(c->memory_allocator() == this->allocator.get());
	#endif
}

ircd::db::database::cache::~cache()
noexcept
{
}

const char *
ircd::db::database::cache::Name()
const noexcept
{
	return !empty(name)?
		name.c_str():
		c->Name();
}

rocksdb::Status
ircd::db::database::cache::Insert(const Slice &key,
                                  void *const value,
                                  size_t charge,
                                  deleter del,
                                  Handle **const handle,
                                  Priority priority)
noexcept
{
	assert(bool(c));
	assert(bool(stats));

	const rocksdb::Status &ret
	{
		c->Insert(key, value, charge, del, handle, priority)
	};

	stats->recordTick(rocksdb::Tickers::BLOCK_CACHE_ADD, ret.ok());
	stats->recordTick(rocksdb::Tickers::BLOCK_CACHE_ADD_FAILURES, !ret.ok());
	stats->recordTick(rocksdb::Tickers::BLOCK_CACHE_DATA_BYTES_INSERT, ret.ok()? charge : 0UL);
	return ret;
}

rocksdb::Cache::Handle *
ircd::db::database::cache::Lookup(const Slice &key,
                                  Statistics *const statistics)
noexcept
{
	assert(bool(c));
	assert(bool(this->stats));

	database::stats::passthru passthru
	{
		this->stats.get(), statistics
	};

	rocksdb::Statistics *const s
	{
		statistics?
			dynamic_cast<rocksdb::Statistics *>(&passthru):
			dynamic_cast<rocksdb::Statistics *>(this->stats.get())
	};

	auto *const &ret
	{
		c->Lookup(key, s)
	};

	// Rocksdb's LRUCache stats are broke. The statistics ptr is null and
	// passing it to Lookup() does nothing internally. We have to do this
	// here ourselves :/

	this->stats->recordTick(rocksdb::Tickers::BLOCK_CACHE_HIT, bool(ret));
	this->stats->recordTick(rocksdb::Tickers::BLOCK_CACHE_MISS, !bool(ret));
	return ret;
}

bool
ircd::db::database::cache::Ref(Handle *const handle)
noexcept
{
	assert(bool(c));
	return c->Ref(handle);
}

bool
ircd::db::database::cache::Release(Handle *const handle,
                                   bool force_erase)
noexcept
{
	assert(bool(c));
	return c->Release(handle, force_erase);
}

void *
ircd::db::database::cache::Value(Handle *const handle)
noexcept
{
	assert(bool(c));
	return c->Value(handle);
}

void
ircd::db::database::cache::Erase(const Slice &key)
noexcept
{
	assert(bool(c));
	return c->Erase(key);
}

uint64_t
ircd::db::database::cache::NewId()
noexcept
{
	assert(bool(c));
	return c->NewId();
}

void
ircd::db::database::cache::SetCapacity(size_t capacity)
noexcept
{
	assert(bool(c));
	return c->SetCapacity(capacity);
}

void
ircd::db::database::cache::SetStrictCapacityLimit(bool strict_capacity_limit)
noexcept
{
	assert(bool(c));
	return c->SetStrictCapacityLimit(strict_capacity_limit);
}

bool
ircd::db::database::cache::HasStrictCapacityLimit()
const noexcept
{
	assert(bool(c));
	return c->HasStrictCapacityLimit();
}

size_t
ircd::db::database::cache::GetCapacity()
const noexcept
{
	assert(bool(c));
	return c->GetCapacity();
}

size_t
ircd::db::database::cache::GetUsage()
const noexcept
{
	assert(bool(c));
	return c->GetUsage();
}

size_t
ircd::db::database::cache::GetUsage(Handle *const handle)
const noexcept
{
	assert(bool(c));
	return c->GetUsage(handle);
}

size_t
ircd::db::database::cache::GetPinnedUsage()
const noexcept
{
	assert(bool(c));
	return c->GetPinnedUsage();
}

void
ircd::db::database::cache::DisownData()
noexcept
{
	assert(bool(c));
	return c->DisownData();
}

void
ircd::db::database::cache::ApplyToAllCacheEntries(callback cb,
                                                  bool thread_safe)
noexcept
{
	assert(bool(c));
	return c->ApplyToAllCacheEntries(cb, thread_safe);
}

void
ircd::db::database::cache::EraseUnRefEntries()
noexcept
{
	assert(bool(c));
	return c->EraseUnRefEntries();
}

std::string
ircd::db::database::cache::GetPrintableOptions()
const noexcept
{
	assert(bool(c));
	return c->GetPrintableOptions();
}

#ifdef IRCD_DB_HAS_CACHE_GETCHARGE
size_t
ircd::db::database::cache::GetCharge(Handle *const handle)
const noexcept
{
	assert(bool(c));
	return c->GetCharge(handle);
}
#endif

///////////////////////////////////////////////////////////////////////////////
//
// database::compaction_filter
//

ircd::db::database::compaction_filter::compaction_filter(column *const &c,
                                                         db::compactor user)
:c{c}
,d{c->d}
,user{std::move(user)}
{
}

ircd::db::database::compaction_filter::~compaction_filter()
noexcept
{
}

rocksdb::CompactionFilter::Decision
ircd::db::database::compaction_filter::FilterV2(const int level,
                                                const Slice &key,
                                                const ValueType type,
                                                const Slice &oldval,
                                                std::string *const newval,
                                                std::string *const skip)
const noexcept
{
	const ctx::uninterruptible::nothrow ui;

	#ifdef RB_DEBUG_DB_ENV
	const auto typestr
	{
		type == kValue?
			"VALUE"_sv:
		type == kMergeOperand?
			"MERGE"_sv:
			"BLOB"_sv
	};
	#endif

	static const compactor::callback empty;
	const db::compactor::callback &callback
	{
		type == ValueType::kValue && user.value?
			user.value:

		type == ValueType::kMergeOperand && user.merge?
			user.merge:

		empty
	};

	if(!callback)
		return Decision::kKeep;

	#ifdef RB_DEBUG_DB_ENV
	log::debug
	{
		log, "[%s]'%s': compaction level:%d key:%zu@%p type:%s old:%zu@%p new:%p skip:%p",
		d->name,
		c->name,
		level,
		size(key),
		data(key),
		typestr,
		size(oldval),
		data(oldval),
		(const void *)newval,
		(const void *)skip
	};
	#endif

	const compactor::args args
	{
		level, slice(key), slice(oldval), newval, skip
	};

	switch(callback(args))
	{
		default:
		case db::op::GET:            return Decision::kKeep;
		case db::op::SET:            return Decision::kChangeValue;
		case db::op::DELETE:         return Decision::kRemove;
		case db::op::DELETE_RANGE:   return Decision::kRemoveAndSkipUntil;
	}
}

bool
ircd::db::database::compaction_filter::IgnoreSnapshots()
const noexcept
{
	// RocksDB >= 6.0.0 sez this must no longer be false.
	return true;
}

const char *
ircd::db::database::compaction_filter::Name()
const noexcept
{
	assert(c);
	return db::name(*c).c_str();
}

///////////////////////////////////////////////////////////////////////////////
//
// database::wal_filter
//

decltype(ircd::db::database::wal_filter::debug)
ircd::db::database::wal_filter::debug
{
	{ "name",      "ircd.db.wal.debug" },
	{ "default",   false               },
	{ "persist",   false               },
};

ircd::db::database::wal_filter::wal_filter(database *const &d)
:d{d}
{
}

ircd::db::database::wal_filter::~wal_filter()
noexcept
{
}

void
ircd::db::database::wal_filter::ColumnFamilyLogNumberMap(const log_number_map &log_number,
                                                         const name_id_map &name_id)
noexcept
{
	assert(d);

	this->log_number = log_number;
	this->name_id = name_id;

	log::debug
	{
		log, "[%s] WAL recovery mapping update: log_number:%zu name_id:%zu",
		db::name(*d),
		log_number.size(),
		name_id.size(),
	};
}

rocksdb::WalFilter::WalProcessingOption
ircd::db::database::wal_filter::LogRecordFound(unsigned long long log_nr,
                                               const std::string &name,
                                               const WriteBatch &wb,
                                               WriteBatch *const replace,
                                               bool *const replaced)
noexcept
{
	assert(d && replace && replaced);

	if(debug)
		log::debug
		{
			log, "[%s] WAL recovery record log:%lu '%s' wb[count:%zu size:%zu]",
			db::name(*d),
			log_nr,
			name,
			wb.Count(),
			wb.GetDataSize(),
		};

	*replaced = false;
	return WalProcessingOption::kContinueProcessing;
}

rocksdb::WalFilter::WalProcessingOption
ircd::db::database::wal_filter::LogRecord(const WriteBatch &wb,
                                          WriteBatch *const replace,
                                          bool *const replaced)
const noexcept
{
	return WalProcessingOption::kContinueProcessing;
}

const char *
ircd::db::database::wal_filter::Name()
const noexcept
{
	assert(d);
	return db::name(*d).c_str();
}

///////////////////////////////////////////////////////////////////////////////
//
// database::sst
//

void
ircd::db::database::sst::tool(const vector_view<const string_view> &args)
{
	const ctx::uninterruptible::nothrow ui;

	static const size_t arg_max {16};
	static const size_t arg_max_len {256};
	thread_local char arg[arg_max][arg_max_len]
	{
		"./sst_dump"
	};

	size_t i(0);
	char *argv[arg_max] { arg[i++] };
	for(; i < arg_max - 1 && i - 1 < args.size(); ++i)
	{
		strlcpy(arg[i], args.at(i - 1));
		argv[i] = arg[i];
	}
	argv[i++] = nullptr;
	assert(i <= arg_max);

	rocksdb::SSTDumpTool tool;
	const int ret
	{
		tool.Run(i, argv)
	};

	if(ret != 0)
		throw error
		{
			"Error from SST dump tool: return value: %d", ret
		};
}

//
// sst::dump::dump
//

ircd::db::database::sst::dump::dump(db::column column,
                                    const key_range &range,
                                    const string_view &path_)
{
	database::column &c(column);
	const database &d(column);
	std::string path
	{
		path_
	};

	if(path.empty())
	{
		const string_view path_parts[]
		{
			fs::base::db, db::name(d), db::name(c)
		};

		path = fs::path_string(path_parts);
	}

	rocksdb::Options opts(d.d->GetOptions(c));
	rocksdb::EnvOptions eopts(opts);
	rocksdb::SstFileWriter writer
	{
		eopts, opts, c
	};

	throw_on_error
	{
		writer.Open(path)
	};

	size_t i(0);
	for(auto it(column.begin()); it != column.end(); ++it, ++i)
		throw_on_error
		{
			writer.Put(slice(it->first), slice(it->second))
		};

	rocksdb::ExternalSstFileInfo info;
	if(i)
		throw_on_error
		{
			writer.Finish(&info)
		};

	this->info.column = db::name(column);
	this->info.path = std::move(info.file_path);
	this->info.min_key = std::move(info.smallest_key);
	this->info.max_key = std::move(info.largest_key);
	this->info.min_seq = info.sequence_number;
	this->info.max_seq = info.sequence_number;
	this->info.size = info.file_size;
	this->info.entries = info.num_entries;
	this->info.version = info.version;
}

//
// sst::info::vector
//

ircd::db::database::sst::info::vector::vector(const database &d)
{
	this->reserve(db::file_count(d));
	for(const auto &c : d.columns)
	{
		db::column column{*c};
		for(auto &&info : vector(column))
			this->emplace_back(std::move(info));
	}
}

ircd::db::database::sst::info::vector::vector(const db::column &column)
{
	database::column &c(const_cast<db::column &>(column));
	database &d(*c.d);

	rocksdb::ColumnFamilyMetaData cfmd;
	d.d->GetColumnFamilyMetaData(c, &cfmd);

	rocksdb::TablePropertiesCollection tpc;
	throw_on_error
	{
		d.d->GetPropertiesOfAllTables(c, &tpc)
	};

	size_t i(0);
	this->resize(std::max(cfmd.file_count, tpc.size()));
	for(rocksdb::LevelMetaData &level : cfmd.levels)
		for(rocksdb::SstFileMetaData md : level.files)
		{
			auto &info(this->at(i++));
			info.operator=(std::move(md));
			info.level = level.level;

			const auto path(info.path + info.name);
			auto tp(*tpc.at(path));
			info.operator=(std::move(tp));
			tpc.erase(path);
		}

	for(auto &&kv : tpc)
	{
		auto &info(this->at(i++));
		auto tp(*kv.second);
		info.operator=(std::move(tp));
		info.path = kv.first;
	}

	assert(i == this->size());
}

//
// sst::info::info
//

ircd::db::database::sst::info::info(const database &d_,
                                    const string_view &filename)
{
	auto &d(const_cast<database &>(d_));
	const ctx::uninterruptible::nothrow ui;

	std::vector<rocksdb::LiveFileMetaData> v;
	d.d->GetLiveFilesMetaData(&v);

	for(auto &md : v)
		if(md.name == filename)
		{
			rocksdb::TablePropertiesCollection tpc;
			throw_on_error
			{
				d.d->GetPropertiesOfAllTables(d[md.column_family_name], &tpc)
			};

			auto tp(*tpc.at(md.db_path + md.name));
			this->operator=(std::move(md));
			this->operator=(std::move(tp));
			return;
		}

	throw not_found
	{
		"No file named '%s' is live in database '%s'",
		filename,
		d.name
	};
}

ircd::db::database::sst::info &
ircd::db::database::sst::info::operator=(rocksdb::LiveFileMetaData &&md)
{
	name = std::move(md.name);
	path = std::move(md.db_path);
	column = std::move(md.column_family_name);
	size = std::move(md.size);
	min_seq = std::move(md.smallest_seqno);
	max_seq = std::move(md.largest_seqno);
	min_key = std::move(md.smallestkey);
	max_key = std::move(md.largestkey);
	num_reads = std::move(md.num_reads_sampled);
	level = std::move(md.level);
	compacting = std::move(md.being_compacted);
	return *this;
}

ircd::db::database::sst::info &
ircd::db::database::sst::info::operator=(rocksdb::SstFileMetaData &&md)
{
	name = std::move(md.name);
	path = std::move(md.db_path);
	size = std::move(md.size);
	min_seq = std::move(md.smallest_seqno);
	max_seq = std::move(md.largest_seqno);
	min_key = std::move(md.smallestkey);
	max_key = std::move(md.largestkey);
	num_reads = std::move(md.num_reads_sampled);
	compacting = std::move(md.being_compacted);
	return *this;
}

ircd::db::database::sst::info &
ircd::db::database::sst::info::operator=(rocksdb::TableProperties &&tp)
{
	column = std::move(tp.column_family_name);
	filter = std::move(tp.filter_policy_name);
	comparator = std::move(tp.comparator_name);
	merge_operator = std::move(tp.merge_operator_name);
	prefix_extractor = std::move(tp.prefix_extractor_name);
	compression = std::move(tp.compression_name);
	format = std::move(tp.format_version);
	cfid = std::move(tp.column_family_id);
	data_size = std::move(tp.data_size);
	index_size = std::move(tp.index_size);
	top_index_size = std::move(tp.top_level_index_size);
	filter_size = std::move(tp.filter_size);
	keys_size = std::move(tp.raw_key_size);
	values_size = std::move(tp.raw_value_size);
	index_parts = std::move(tp.index_partitions);
	data_blocks = std::move(tp.num_data_blocks);
	entries = std::move(tp.num_entries);
	range_deletes = std::move(tp.num_range_deletions);
	fixed_key_len = std::move(tp.fixed_key_len);
	created = std::move(tp.creation_time);
	oldest_key = std::move(tp.oldest_key_time);
	delta_encoding = std::move(tp.index_value_is_delta_encoded);
	return *this;
}

///////////////////////////////////////////////////////////////////////////////
//
// database::wal
//

//
// wal::info::vector
//

ircd::db::database::wal::info::vector::vector(const database &d_)
{
	auto &d{const_cast<database &>(d_)};
	std::vector<std::unique_ptr<rocksdb::LogFile>> vec;
	throw_on_error
	{
		d.d->GetSortedWalFiles(vec)
	};

	this->resize(vec.size());
	for(size_t i(0); i < vec.size(); ++i)
		this->at(i).operator=(*vec.at(i));
}

//
// wal::info::info
//

ircd::db::database::wal::info::info(const database &d_,
                                    const string_view &filename)
{
	auto &d{const_cast<database &>(d_)};
	std::vector<std::unique_ptr<rocksdb::LogFile>> vec;
	throw_on_error
	{
		d.d->GetSortedWalFiles(vec)
	};

	for(const auto &ptr : vec)
		if(ptr->PathName() == filename)
		{
			this->operator=(*ptr);
			return;
		}

	throw not_found
	{
		"No file named '%s' is live in database '%s'",
		filename,
		d.name
	};
}

ircd::db::database::wal::info &
ircd::db::database::wal::info::operator=(const rocksdb::LogFile &lf)
{
	name = lf.PathName();
	number = lf.LogNumber();
	seq = lf.StartSequence();
	size = lf.SizeFileBytes();
	alive = lf.Type() == rocksdb::WalFileType::kAliveLogFile;

	return *this;
}

///////////////////////////////////////////////////////////////////////////////
//
// db/stats.h
//

std::string
ircd::db::string(const rocksdb::IOStatsContext &ic,
                 const bool &all)
{
	const bool exclude_zeros(!all);
	return ic.ToString(exclude_zeros);
}

const rocksdb::IOStatsContext &
ircd::db::iostats_current()
{
	const auto *const &ret
	{
		rocksdb::get_iostats_context()
	};

	if(unlikely(!ret))
		throw error
		{
			"IO counters are not available on this thread."
		};

	return *ret;
}

std::string
ircd::db::string(const rocksdb::PerfContext &pc,
                 const bool &all)
{
	const bool exclude_zeros(!all);
	return pc.ToString(exclude_zeros);
}

const rocksdb::PerfContext &
ircd::db::perf_current()
{
	const auto *const &ret
	{
		rocksdb::get_perf_context()
	};

	if(unlikely(!ret))
		throw error
		{
			"Performance counters are not available on this thread."
		};

	return *ret;
}

void
ircd::db::perf_level(const uint &level)
{
	if(level >= rocksdb::PerfLevel::kOutOfBounds)
		throw error
		{
			"Perf level of '%u' is invalid; maximum is '%u'",
			level,
			uint(rocksdb::PerfLevel::kOutOfBounds)
		};

	rocksdb::SetPerfLevel(rocksdb::PerfLevel(level));
}

uint
ircd::db::perf_level()
{
	return rocksdb::GetPerfLevel();
}

//
// ticker
//

uint64_t
ircd::db::ticker(const database &d,
                 const string_view &key)
{
	return ticker(d, ticker_id(key));
}

uint64_t
ircd::db::ticker(const database &d,
                 const uint32_t &id)
{
	return d.stats->getTickerCount(id);
}

uint32_t
ircd::db::ticker_id(const string_view &key)
{
	for(const auto &pair : rocksdb::TickersNameMap)
		if(key == pair.second)
			return pair.first;

	throw std::out_of_range
	{
		"No ticker with that key"
	};
}

ircd::string_view
ircd::db::ticker_id(const uint32_t &id)
{
	for(const auto &pair : rocksdb::TickersNameMap)
		if(id == pair.first)
			return pair.second;

	return {};
}

decltype(ircd::db::ticker_max)
ircd::db::ticker_max
{
	rocksdb::TICKER_ENUM_MAX
};

//
// histogram
//

const struct ircd::db::histogram &
ircd::db::histogram(const database &d,
                    const string_view &key)
{
	return histogram(d, histogram_id(key));
}

const struct ircd::db::histogram &
ircd::db::histogram(const database &d,
                    const uint32_t &id)
{
	return d.stats->histogram.at(id);
}

uint32_t
ircd::db::histogram_id(const string_view &key)
{
	for(const auto &pair : rocksdb::HistogramsNameMap)
		if(key == pair.second)
			return pair.first;

	throw std::out_of_range
	{
		"No histogram with that key"
	};
}

ircd::string_view
ircd::db::histogram_id(const uint32_t &id)
{
	for(const auto &pair : rocksdb::HistogramsNameMap)
		if(id == pair.first)
			return pair.second;

	return {};
}

decltype(ircd::db::histogram_max)
ircd::db::histogram_max
{
	rocksdb::HISTOGRAM_ENUM_MAX
};

///////////////////////////////////////////////////////////////////////////////
//
// db/prefetcher.h
//

decltype(ircd::db::prefetcher)
ircd::db::prefetcher;

//
// db::prefetcher
//
ircd::db::prefetcher::prefetcher()
:ticker
{
	std::make_unique<struct ticker>()
}
,context
{
	"db.prefetcher",
	128_KiB,
	context::POST,
	std::bind(&prefetcher::worker, this)
}
{
}

ircd::db::prefetcher::~prefetcher()
noexcept
{
	while(!queue.empty())
	{
		log::warning
		{
			log, "Prefetcher waiting for %zu requests to clear...",
			queue.size(),
		};

		dock.wait_for(seconds(5), [this]
		{
			return queue.empty();
		});
	}

	assert(queue.empty());
}

bool
ircd::db::prefetcher::operator()(column &c,
                                 const string_view &key,
                                 const gopts &opts)
{
	auto &d
	{
		static_cast<database &>(c)
	};

	assert(ticker);
	ticker->queries++;
	if(db::cached(c, key, opts))
	{
		ticker->rejects++;
		return false;
	}

	queue.emplace_back(d, c, key);
	queue.back().snd = now<steady_point>();
	ticker->request++;

	// Branch here based on whether it's not possible to directly dispatch
	// a db::request worker. If all request workers are busy we notify our own
	// prefetcher worker, and then it blocks on submitting to the request
	// worker instead of us blocking here. This is done to avoid use and growth
	// of any request pool queue, and allow for more direct submission.
	if(db::request.wouldblock())
	{
		dock.notify_one();

		// If the user sets NO_BLOCKING we honor their request to not
		// context switch for a prefetch. However by default we want to
		// control queue growth, so we insert voluntary yield here to allow
		// prefetch operations to at least be processed before returning to
		// the user submitting more prefetches.
		if(likely(!test(opts, db::get::NO_BLOCKING)))
			ctx::yield();

		return true;
	}

	const ctx::critical_assertion ca;
	ticker->directs++;
	this->handle();
	return true;
}

size_t
ircd::db::prefetcher::cancel(column &c)
{
	return cancel([&c]
	(const auto &request)
	{
		return request.cid == id(c);
	});
}

size_t
ircd::db::prefetcher::cancel(database &d)
{
	return cancel([&d]
	(const auto &request)
	{
		return request.d == std::addressof(d);
	});
}

size_t
ircd::db::prefetcher::cancel(const closure &closure)
{
	size_t canceled(0);
	for(auto &request : queue)
	{
		// already finished
		if(request.fin != steady_point::min())
			continue;

		// in progress; can't cancel
		if(request.req != steady_point::min())
			continue;

		// allow user to accept or reject
		if(!closure(request))
			continue;

		// cancel by precociously setting the finish time.
		request.fin = now<steady_point>();
		++canceled;
	}

	if(canceled)
		dock.notify_all();

	assert(ticker);
	ticker->cancels += canceled;
	return canceled;
}

void
ircd::db::prefetcher::worker()
try
{
	while(1)
	{
		dock.wait([this]
		{
			if(queue.empty())
				return false;

			assert(ticker);
			if(ticker->request <= ticker->handles)
				return false;

			return true;
		});

		handle();
	}
}
catch(const std::exception &e)
{
	log::critical
	{
		log, "prefetcher worker: %s",
		e.what()
	};
}

void
ircd::db::prefetcher::handle()
{
	auto handler
	{
		std::bind(&prefetcher::request_worker, this)
	};

	ticker->handles++;
	db::request(std::move(handler));
	ticker->handled++;
}

void
ircd::db::prefetcher::request_worker()
{
	const ctx::scope_notify notify
	{
		this->dock
	};

	const scope_count request_workers
	{
		this->request_workers
	};

	// Garbage collection of the queue invoked unconditionally on unwind.
	const unwind cleanup_on_leave
	{
		std::bind(&prefetcher::request_cleanup, this)
	};

	// GC the queue here to get rid of any cancelled requests which have
	// arrived at the front so they don't become our request.
	const size_t cleanup_on_enter
	{
		request_cleanup()
	};

	// Find the first request in the queue which does not have its req
	// timestamp sent.
	auto request
	{
		std::find_if(begin(queue), end(queue), []
		(const auto &request)
		{
			return request.req == steady_point::min();
		})
	};

	if(request == end(queue))
		return;

	assert(ticker);
	assert(request->fin == steady_point::min());
	request->req = now<steady_point>();
	ticker->last_snd_req = duration_cast<microseconds>(request->req - request->snd);
	ticker->accum_snd_req += ticker->last_snd_req;

	ticker->fetches++;
	request_handle(*request);
	assert(request->fin != steady_point::min());
	ticker->fetched++;

	#ifdef IRCD_DB_DEBUG_PREFETCH
	log::debug
	{
		log, "prefetcher reject:%zu request:%zu handle:%zu fetch:%zu direct:%zu cancel:%zu queue:%zu rw:%zu",
		ticker->rejects,
		ticker->request,
		ticker->handles,
		ticker->fetches,
		ticker->directs,
		ticker->cancels,
		queue.size(),
		this->request_workers,
	};
	#endif
}

size_t
ircd::db::prefetcher::request_cleanup()
noexcept
{
	size_t removed(0);
	const ctx::critical_assertion ca;
	for(; !queue.empty() && queue.front().fin != steady_point::min(); ++removed)
		queue.pop_front();

	return removed;
}

void
ircd::db::prefetcher::request_handle(request &request)
try
{
	assert(request.d);
	db::column column
	{
		(*request.d)[request.cid]
	};

	const string_view key
	{
		request
	};

	const auto it
	{
		seek(column, key, gopts{})
	};

	const ctx::critical_assertion ca;
	request.fin = now<steady_point>();
	ticker->last_req_fin = duration_cast<microseconds>(request.fin - request.req);
	ticker->accum_req_fin += ticker->last_req_fin;
	const bool lte
	{
		valid_lte(*it, key)
	};

	if(likely(lte))
	{
		ticker->fetched_bytes_key += size(it->key());
		ticker->fetched_bytes_val += size(it->value());
	}

	#ifdef IRCD_DB_DEBUG_PREFETCH
	char pbuf[3][32];
	log::debug
	{
		log, "[%s][%s] completed prefetch len:%zu lte:%b k:%zu v:%zu snd-req:%s req-fin:%s snd-fin:%s queue:%zu",
		name(*request.d),
		name(column),
		size(key),
		lte,
		lte? size(it->key()) : 0UL,
		lte? size(it->value()) : 0UL,
		pretty(pbuf[0], request.req - request.snd, 1),
		pretty(pbuf[1], request.fin - request.req, 1),
		pretty(pbuf[2], request.fin - request.snd, 1),
		queue.size(),
	};
	#endif
}
catch(const std::exception &e)
{
	assert(request.d);
	request.fin = now<steady_point>();

	log::error
	{
		log, "[%s][%u] :%s",
		name(*request.d),
		request.cid,
		e.what(),
	};
}
catch(...)
{
	request.fin = now<steady_point>();
	throw;
}

size_t
ircd::db::prefetcher::wait_pending()
{
	const size_t fetched_counter
	{
		ticker->fetched
	};

	const size_t fetched_target
	{
		fetched_counter + request_workers
	};

	dock.wait([this, &fetched_target]
	{
		return this->ticker->fetched >= fetched_target;
	});

	assert(fetched_target >= fetched_counter);
	return fetched_target - fetched_counter;
}

//
// prefetcher::request
//

ircd::db::prefetcher::request::request(database &d,
                                       const column &c,
                                       const string_view &key)
noexcept
:d
{
	std::addressof(d)
}
,cid
{
	db::id(c)
}
,len
{
	 uint32_t(std::min(size(key), sizeof(this->key)))
}
,snd
{
	steady_point::min()
}
,req
{
	steady_point::min()
}
,fin
{
	steady_point::min()
}
{
	const size_t &len
	{
		buffer::copy(this->key, key)
	};

	assert(this->len == len);
}

ircd::db::prefetcher::request::operator
ircd::string_view()
const noexcept
{
	return
	{
		key, len
	};
}

///////////////////////////////////////////////////////////////////////////////
//
// db/txn.h
//

void
ircd::db::get(database &d,
              const uint64_t &seq,
              const seq_closure &closure)
{
	for_each(d, seq, seq_closure_bool{[&closure]
	(txn &txn, const uint64_t &seq)
	{
		closure(txn, seq);
		return false;
	}});
}

void
ircd::db::for_each(database &d,
                   const uint64_t &seq,
                   const seq_closure &closure)
{
	for_each(d, seq, seq_closure_bool{[&closure]
	(txn &txn, const uint64_t &seq)
	{
		closure(txn, seq);
		return true;
	}});
}

bool
ircd::db::for_each(database &d,
                   const uint64_t &seq,
                   const seq_closure_bool &closure)
{
	std::unique_ptr<rocksdb::TransactionLogIterator> tit;
	{
		const ctx::uninterruptible ui;
		throw_on_error
		{
			d.d->GetUpdatesSince(seq, &tit)
		};
	}

	assert(bool(tit));
	for(; tit->Valid(); tit->Next())
	{
		const ctx::uninterruptible ui;

		auto batchres
		{
			tit->GetBatch()
		};

		throw_on_error
		{
			tit->status()
		};

		db::txn txn
		{
			d, std::move(batchres.writeBatchPtr)
		};

		assert(bool(txn.wb));
		if(!closure(txn, batchres.sequence))
			return false;
	}

	return true;
}

std::string
ircd::db::debug(const txn &t)
{
	const rocksdb::WriteBatch &wb(t);
	return db::debug(wb);
}

void
ircd::db::for_each(const txn &t,
                   const delta_closure &closure)
{
	const auto re{[&closure]
	(const delta &delta)
	{
		closure(delta);
		return true;
	}};

	const database &d(t);
	const rocksdb::WriteBatch &wb(t);
	txn::handler h{d, re};
	wb.Iterate(&h);
}

bool
ircd::db::for_each(const txn &t,
                   const delta_closure_bool &closure)
{
	const database &d(t);
	const rocksdb::WriteBatch &wb(t);
	txn::handler h{d, closure};
	wb.Iterate(&h);
	return h._continue;
}

///
/// handler (db/database/txn.h)
///

rocksdb::Status
ircd::db::txn::handler::PutCF(const uint32_t cfid,
                              const Slice &key,
                              const Slice &val)
noexcept
{
	return callback(cfid, op::SET, key, val);
}

rocksdb::Status
ircd::db::txn::handler::DeleteCF(const uint32_t cfid,
                                 const Slice &key)
noexcept
{
	return callback(cfid, op::DELETE, key, {});
}

rocksdb::Status
ircd::db::txn::handler::DeleteRangeCF(const uint32_t cfid,
                                      const Slice &begin,
                                      const Slice &end)
noexcept
{
	return callback(cfid, op::DELETE_RANGE, begin, end);
}

rocksdb::Status
ircd::db::txn::handler::SingleDeleteCF(const uint32_t cfid,
                                       const Slice &key)
noexcept
{
	return callback(cfid, op::SINGLE_DELETE, key, {});
}

rocksdb::Status
ircd::db::txn::handler::MergeCF(const uint32_t cfid,
                                const Slice &key,
                                const Slice &value)
noexcept
{
	return callback(cfid, op::MERGE, key, value);
}

rocksdb::Status
ircd::db::txn::handler::MarkBeginPrepare(bool b)
noexcept
{
	ircd::not_implemented{};
	return Status::OK();
}

rocksdb::Status
ircd::db::txn::handler::MarkEndPrepare(const Slice &xid)
noexcept
{
	ircd::not_implemented{};
	return Status::OK();
}

rocksdb::Status
ircd::db::txn::handler::MarkCommit(const Slice &xid)
noexcept
{
	ircd::not_implemented{};
	return Status::OK();
}

rocksdb::Status
ircd::db::txn::handler::MarkRollback(const Slice &xid)
noexcept
{
	ircd::not_implemented{};
	return Status::OK();
}

rocksdb::Status
ircd::db::txn::handler::callback(const uint32_t &cfid,
                                 const op &op,
                                 const Slice &a,
                                 const Slice &b)
noexcept try
{
	auto &c{d[cfid]};
	const delta delta
	{
		op,
		db::name(c),
		slice(a),
		slice(b)
	};

	return callback(delta);
}
catch(const std::exception &e)
{
	_continue = false;
	log::critical
	{
		log, "txn::handler: cfid[%u]: %s",
		cfid,
		e.what()
	};

	ircd::terminate();
	__builtin_unreachable();
}

rocksdb::Status
ircd::db::txn::handler::callback(const delta &delta)
noexcept try
{
	_continue = cb(delta);
	return Status::OK();
}
catch(const std::exception &e)
{
	_continue = false;
	return Status::OK();
}

bool
ircd::db::txn::handler::Continue()
noexcept
{
	return _continue;
}

//
// txn
//

ircd::db::txn::txn(database &d)
:txn{d, opts{}}
{
}

ircd::db::txn::txn(database &d,
                   const opts &opts)
:d{&d}
,wb
{
	std::make_unique<rocksdb::WriteBatch>(opts.reserve_bytes, opts.max_bytes)
}
{
}

ircd::db::txn::txn(database &d,
                   std::unique_ptr<rocksdb::WriteBatch> &&wb)
:d{&d}
,wb{std::move(wb)}
{
}

ircd::db::txn::~txn()
noexcept
{
}

void
ircd::db::txn::operator()(const sopts &opts)
{
	assert(bool(d));
	operator()(*d, opts);
}

void
ircd::db::txn::operator()(database &d,
                          const sopts &opts)
{
	assert(bool(wb));
	assert(this->state == state::BUILD);
	this->state = state::COMMIT;
	commit(d, *wb, opts);
	this->state = state::COMMITTED;
}

void
ircd::db::txn::clear()
{
	assert(bool(wb));
	wb->Clear();
	this->state = state::BUILD;
}

size_t
ircd::db::txn::size()
const
{
	assert(bool(wb));
	return wb->Count();
}

size_t
ircd::db::txn::bytes()
const
{
	assert(bool(wb));
	return wb->GetDataSize();
}

bool
ircd::db::txn::has(const op &op)
const
{
	assert(bool(wb));
	switch(op)
	{
		case op::GET:              assert(0); return false;
		case op::SET:              return wb->HasPut();
		case op::MERGE:            return wb->HasMerge();
		case op::DELETE:           return wb->HasDelete();
		case op::DELETE_RANGE:     return wb->HasDeleteRange();
		case op::SINGLE_DELETE:    return wb->HasSingleDelete();
	}

	return false;
}

bool
ircd::db::txn::has(const op &op,
                   const string_view &col)
const
{
	return !for_each(*this, delta_closure_bool{[&op, &col]
	(const auto &delta)
	{
		return std::get<delta::OP>(delta) != op &&
		       std::get<delta::COL>(delta) != col;
	}});
}

void
ircd::db::txn::at(const op &op,
                  const string_view &col,
                  const delta_closure &closure)
const
{
	if(!get(op, col, closure))
		throw not_found
		{
			"db::txn::at(%s, %s): no matching delta in transaction",
			reflect(op),
			col
		};
}

bool
ircd::db::txn::get(const op &op,
                   const string_view &col,
                   const delta_closure &closure)
const
{
	return !for_each(*this, delta_closure_bool{[&op, &col, &closure]
	(const delta &delta)
	{
		if(std::get<delta::OP>(delta) == op &&
		   std::get<delta::COL>(delta) == col)
		{
			closure(delta);
			return false;
		}
		else return true;
	}});
}

bool
ircd::db::txn::has(const op &op,
                   const string_view &col,
                   const string_view &key)
const
{
	return !for_each(*this, delta_closure_bool{[&op, &col, &key]
	(const auto &delta)
	{
		return std::get<delta::OP>(delta) != op &&
		       std::get<delta::COL>(delta) != col &&
		       std::get<delta::KEY>(delta) != key;
	}});
}

void
ircd::db::txn::at(const op &op,
                  const string_view &col,
                  const string_view &key,
                  const value_closure &closure)
const
{
	if(!get(op, col, key, closure))
		throw not_found
		{
			"db::txn::at(%s, %s, %s): no matching delta in transaction",
			reflect(op),
			col,
			key
		};
}

bool
ircd::db::txn::get(const op &op,
                   const string_view &col,
                   const string_view &key,
                   const value_closure &closure)
const
{
	return !for_each(*this, delta_closure_bool{[&op, &col, &key, &closure]
	(const delta &delta)
	{
		if(std::get<delta::OP>(delta) == op &&
		   std::get<delta::COL>(delta) == col &&
		   std::get<delta::KEY>(delta) == key)
		{
			closure(std::get<delta::VAL>(delta));
			return false;
		}
		else return true;
	}});
}

ircd::db::txn::operator
ircd::db::database &()
{
	assert(bool(d));
	return *d;
}

ircd::db::txn::operator
rocksdb::WriteBatch &()
{
	assert(bool(wb));
	return *wb;
}

ircd::db::txn::operator
const ircd::db::database &()
const
{
	assert(bool(d));
	return *d;
}

ircd::db::txn::operator
const rocksdb::WriteBatch &()
const
{
	assert(bool(wb));
	return *wb;
}

//
// txn::checkpoint
//

ircd::db::txn::checkpoint::checkpoint(txn &t)
:t{t}
{
	assert(bool(t.wb));
	t.wb->SetSavePoint();
}

ircd::db::txn::checkpoint::~checkpoint()
noexcept
{
	const ctx::uninterruptible ui;
	if(likely(!std::uncaught_exceptions()))
		throw_on_error { t.wb->PopSavePoint() };
	else
		throw_on_error { t.wb->RollbackToSavePoint() };
}

//
// txn::append
//

ircd::db::txn::append::append(txn &t,
                              const string_view &key,
                              const json::iov &iov)
{
	std::for_each(std::begin(iov), std::end(iov), [&t, &key]
	(const auto &member)
	{
		append
		{
			t, delta
			{
				member.first,   // col
				key,            // key
				member.second   // val
			}
		};
	});
}

ircd::db::txn::append::append(txn &t,
                              const delta &delta)
{
	assert(bool(t.d));
	append(t, *t.d, delta);
}

__attribute__((noreturn))
ircd::db::txn::append::append(txn &t,
                              const row::delta &delta)
{
	throw ircd::not_implemented
	{
		"db::txn::append (row::delta)"
	};
}

ircd::db::txn::append::append(txn &t,
                              const cell::delta &delta)
{
	db::append(*t.wb, delta);
}

ircd::db::txn::append::append(txn &t,
                              column &c,
                              const column::delta &delta)
{
	db::append(*t.wb, c, delta);
}

ircd::db::txn::append::append(txn &t,
                              database &d,
                              const delta &delta)
{
	db::column c
	{
		d[std::get<1>(delta)]
	};

	db::append(*t.wb, c, db::column::delta
	{
		std::get<op>(delta),
		std::get<2>(delta),
		std::get<3>(delta)
	});
}

///////////////////////////////////////////////////////////////////////////////
//
// db/row.h
//

namespace ircd::db
{
	static std::vector<rocksdb::Iterator *>
	_make_iterators(database &d,
	                database::column *const *const &columns,
	                const size_t &columns_size,
	                const rocksdb::ReadOptions &opts);
}

void
ircd::db::del(row &row,
              const sopts &sopts)
{
	write(row::delta{op::DELETE, row}, sopts);
}

void
ircd::db::write(const row::delta &delta,
                const sopts &sopts)
{
	write(&delta, &delta + 1, sopts);
}

void
ircd::db::write(const sopts &sopts,
                const std::initializer_list<row::delta> &deltas)
{
	write(deltas, sopts);
}

void
ircd::db::write(const std::initializer_list<row::delta> &deltas,
                const sopts &sopts)
{
	write(std::begin(deltas), std::end(deltas), sopts);
}

void
ircd::db::write(const row::delta *const &begin,
                const row::delta *const &end,
                const sopts &sopts)
{
	// Count the total number of cells for this transaction.
	const auto cells
	{
		std::accumulate(begin, end, size_t(0), []
		(auto ret, const row::delta &delta)
		{
			const auto &row(std::get<row *>(delta));
			return ret += row->size();
		})
	};

	//TODO: allocator?
	std::vector<cell::delta> deltas;
	deltas.reserve(cells);

	// Compose all of the cells from all of the rows into a single txn
	std::for_each(begin, end, [&deltas]
	(const auto &delta)
	{
		const auto &op(std::get<op>(delta));
		const auto &row(std::get<row *>(delta));
		std::for_each(std::begin(*row), std::end(*row), [&deltas, &op]
		(auto &cell)
		{
			// For operations like DELETE which don't require a value in
			// the delta, we can skip a potentially expensive load of the cell.
			const auto value
			{
				value_required(op)? cell.val() : string_view{}
			};

			deltas.emplace_back(op, cell, value);
		});
	});

	// Commitment
	write(&deltas.front(), &deltas.front() + deltas.size(), sopts);
}

size_t
ircd::db::seek(row &r,
               const string_view &key,
               const gopts &opts)
{
	// The following closure performs the seek() for a single cell in the row.
	// It may be executed on another ircd::ctx if the data isn't cached and
	// blocking IO is required. This frame can't be interrupted because it may
	// have requests pending in the request pool which must synchronize back
	// here.
	size_t ret{0};
	std::exception_ptr eptr;
	ctx::latch latch{r.size()};
	const ctx::uninterruptible ui;
	const auto closure{[&opts, &latch, &ret, &key, &eptr]
	(auto &cell) noexcept
	{
		// If there's a pending error from another cell by the time this
		// closure is executed we don't perform the seek() unless the user
		// specifies db::get::NO_THROW to suppress it.
		if(!eptr || test(opts, get::NO_THROW)) try
		{
			if(!seek(cell, key))
			{
				// If the cell is not_found that's not a thrown exception here;
				// the cell will just be !valid(). The user can specify
				// get::THROW to propagate a not_found from the seek(row);
				if(test(opts, get::THROW))
					throw not_found
					{
						"column '%s' key '%s'", cell.col(), key
					};
			}
			else ++ret;
		}
		catch(const not_found &e)
		{
			eptr = std::current_exception();
		}
		catch(const std::exception &e)
		{
			log::error
			{
				log, "row seek: column '%s' key '%s' :%s",
				cell.col(),
				key,
				e.what()
			};

			eptr = std::make_exception_ptr(e);
		}

		// The latch must always be hit here. No exception should propagate
		// to prevent this from being reached or beyond.
		latch.count_down();
	}};

	#ifdef RB_DEBUG_DB_SEEK_ROW
	const ircd::timer timer;
	size_t submits{0};
	#endif

	// Submit all the requests
	for(auto &cell : r)
	{
		db::column &column(cell);
		const auto reclosure{[&closure, &cell]
		() noexcept
		{
			closure(cell);
		}};

		// Whether to submit the request to another ctx or execute it here.
		// Explicit option to prevent submitting must not be set. If there
		// is a chance the data is already in the cache, we can avoid the
		// context switching and occupation of the request pool.
		//TODO: should check a bloom filter on the cache for this branch
		//TODO: because right now double-querying the cache is gross.
		const bool submit
		{
			r.size() > 1 &&
			!test(opts, get::NO_PARALLEL) &&
			!db::cached(column, key, opts)
		};

		#ifdef RB_DEBUG_DB_SEEK_ROW
		submits += submit;
		#endif

		if(submit)
			request(reclosure);
		else
			reclosure();
	}

	// Wait for responses.
	latch.wait();
	assert(ret <= r.size());

	#ifdef RB_DEBUG_DB_SEEK_ROW
	if(likely(!r.empty()))
	{
		const column &c(r[0]);
		const database &d(c);
		thread_local char tmbuf[32];
		log::debug
		{
			log, "'%s' SEEK ROW seq:%lu:%-10lu cnt:%-2zu req:%-2zu ret:%-2zu in %s %s",
			name(d),
			sequence(d),
			sequence(opts.snapshot),
			r.size(),
			submits,
			ret,
			pretty(tmbuf, timer.at<microseconds>(), true),
			what(eptr)
		};
	}
	#endif

	if(eptr && !test(opts, get::NO_THROW))
		std::rethrow_exception(eptr);

	return ret;
}

//
// row
//
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wstack-usage="
__attribute__((stack_protect))
ircd::db::row::row(database &d,
                   const string_view &key,
                   const vector_view<const string_view> &colnames,
                   const vector_view<cell> &buf,
                   gopts opts)
:vector_view<cell>{[&d, &colnames, &buf, &opts]
{
	using std::end;
	using std::begin;

	if(!opts.snapshot)
		opts.snapshot = database::snapshot(d);

	const rocksdb::ReadOptions options
	{
		make_opts(opts)
	};

	assert(buf.size() >= colnames.size());
	const size_t request_count
	{
		std::min(colnames.size(), buf.size())
	};

	size_t count(0);
	database::column *colptr[request_count];
	for(size_t i(0); i < request_count; ++i)
	{
		const auto cfid
		{
			d.cfid(std::nothrow, colnames.at(i))
		};

		if(cfid >= 0)
			colptr[count++] = &d[cfid];
	}

	// All pointers returned by rocksdb in this vector must be free'd.
	const auto iterators
	{
		_make_iterators(d, colptr, count, options)
	};

	assert(iterators.size() == count);
	for(size_t i(0); i < iterators.size(); ++i)
	{
		std::unique_ptr<rocksdb::Iterator> it
		{
			iterators.at(i)
		};

		buf[i] = cell
		{
			*colptr[i], std::move(it), opts
		};
	}

	return vector_view<cell>
	{
		buf.data(), iterators.size()
	};
}()}
{
	if(key)
		seek(*this, key, opts);
}
#pragma GCC diagnostic pop

static std::vector<rocksdb::Iterator *>
ircd::db::_make_iterators(database &d,
                          database::column *const *const &column,
                          const size_t &column_count,
                          const rocksdb::ReadOptions &opts)
{
	using rocksdb::Iterator;
	using rocksdb::ColumnFamilyHandle;
	assert(column_count <= d.columns.size());

	//const ctx::critical_assertion ca;
	// NewIterators() has been seen to lead to IO and block the ircd::ctx;
	// specifically when background options are aggressive and shortly
	// after db opens. It would be nice if we could maintain the
	// critical_assertion for this function, as we could eliminate the
	// vector allocation for ColumnFamilyHandle pointers.

	std::vector<ColumnFamilyHandle *> handles(column_count);
	std::transform(column, column + column_count, begin(handles), []
	(database::column *const &ptr)
	{
		assert(ptr);
		return ptr->handle.get();
	});

	std::vector<Iterator *> ret;
	const ctx::stack_usage_assertion sua;
	throw_on_error
	{
		d.d->NewIterators(opts, handles, &ret)
	};

	return ret;
}

void
ircd::db::row::operator()(const op &op,
                          const string_view &col,
                          const string_view &val,
                          const sopts &sopts)
{
	write(cell::delta{op, (*this)[col], val}, sopts);
}

ircd::db::cell &
ircd::db::row::operator[](const string_view &column)
{
	const auto it(find(column));
	if(unlikely(it == end()))
		throw not_found
		{
			"column '%s' not specified in the descriptor schema", column
		};

	return *it;
}

const ircd::db::cell &
ircd::db::row::operator[](const string_view &column)
const
{
	const auto it(find(column));
	if(unlikely(it == end()))
		throw not_found
		{
			"column '%s' not specified in the descriptor schema", column
		};

	return *it;
}

ircd::db::row::iterator
ircd::db::row::find(const string_view &col)
{
	return std::find_if(std::begin(*this), std::end(*this), [&col]
	(const auto &cell)
	{
		return name(cell.c) == col;
	});
}

ircd::db::row::const_iterator
ircd::db::row::find(const string_view &col)
const
{
	return std::find_if(std::begin(*this), std::end(*this), [&col]
	(const auto &cell)
	{
		return name(cell.c) == col;
	});
}

bool
ircd::db::row::cached()
const
{
	return std::all_of(std::begin(*this), std::end(*this), []
	(const auto &cell)
	{
		db::column &column(const_cast<db::cell &>(cell));
		return cell.valid() && db::cached(column, cell.key());
	});
}

bool
ircd::db::row::cached(const string_view &key)
const
{
	return std::all_of(std::begin(*this), std::end(*this), [&key]
	(const auto &cell)
	{
		db::column &column(const_cast<db::cell &>(cell));
		return db::cached(column, key);
	});
}

bool
ircd::db::row::valid_all(const string_view &s)
const
{
	return !empty() && std::all_of(std::begin(*this), std::end(*this), [&s]
	(const auto &cell)
	{
		return cell.valid(s);
	});
}

bool
ircd::db::row::valid(const string_view &s)
const
{
	return std::any_of(std::begin(*this), std::end(*this), [&s]
	(const auto &cell)
	{
		return cell.valid(s);
	});
}

bool
ircd::db::row::valid_all()
const
{
	return !empty() && std::all_of(std::begin(*this), std::end(*this), []
	(const auto &cell)
	{
		return cell.valid();
	});
}

bool
ircd::db::row::valid()
const
{
	return std::any_of(std::begin(*this), std::end(*this), []
	(const auto &cell)
	{
		return cell.valid();
	});
}

///////////////////////////////////////////////////////////////////////////////
//
// db/cell.h
//

uint64_t
ircd::db::sequence(const cell &c)
{
	const database::snapshot &ss(c);
	return sequence(database::snapshot(c));
}

const std::string &
ircd::db::name(const cell &c)
{
	return name(c.c);
}

void
ircd::db::write(const cell::delta &delta,
                const sopts &sopts)
{
	write(&delta, &delta + 1, sopts);
}

void
ircd::db::write(const sopts &sopts,
                const std::initializer_list<cell::delta> &deltas)
{
	write(deltas, sopts);
}

void
ircd::db::write(const std::initializer_list<cell::delta> &deltas,
                const sopts &sopts)
{
	write(std::begin(deltas), std::end(deltas), sopts);
}

void
ircd::db::write(const cell::delta *const &begin,
                const cell::delta *const &end,
                const sopts &sopts)
{
	if(begin == end)
		return;

	// Find the database through one of the cell's columns. cell::deltas
	// may come from different columns so we do nothing else with this.
	auto &front(*begin);
	column &c(std::get<cell *>(front)->c);
	database &d(c);

	rocksdb::WriteBatch batch;
	std::for_each(begin, end, [&batch]
	(const cell::delta &delta)
	{
		append(batch, delta);
	});

	commit(d, batch, sopts);
}

template<class pos>
bool
ircd::db::seek(cell &c,
               const pos &p,
               gopts opts)
{
	column &cc(c);
	database::column &dc(cc);

	if(!opts.snapshot)
		opts.snapshot = c.ss;

	const auto ropts(make_opts(opts));
	return seek(dc, p, ropts, c.it);
}
template bool ircd::db::seek<ircd::db::pos>(cell &, const pos &, gopts);
template bool ircd::db::seek<ircd::string_view>(cell &, const string_view &, gopts);

// Linkage for incomplete rocksdb::Iterator
ircd::db::cell::cell()
{
}

ircd::db::cell::cell(database &d,
                     const string_view &colname,
                     const gopts &opts)
:cell
{
	column(d[colname]), std::unique_ptr<rocksdb::Iterator>{}, opts
}
{
}

ircd::db::cell::cell(database &d,
                     const string_view &colname,
                     const string_view &index,
                     const gopts &opts)
:cell
{
	column(d[colname]), index, opts
}
{
}

ircd::db::cell::cell(column column,
                     const string_view &index,
                     const gopts &opts)
:c{std::move(column)}
,ss{opts.snapshot}
,it
{
	!index.empty()?
		seek(this->c, index, opts):
		std::unique_ptr<rocksdb::Iterator>{}
}
{
	if(bool(this->it))
		if(!valid_eq(*this->it, index))
			this->it.reset();
}

ircd::db::cell::cell(column column,
                     const string_view &index,
                     std::unique_ptr<rocksdb::Iterator> it,
                     const gopts &opts)
:c{std::move(column)}
,ss{opts.snapshot}
,it{std::move(it)}
{
	if(index.empty())
		return;

	seek(*this, index, opts);
	if(!valid_eq(*this->it, index))
		this->it.reset();
}

ircd::db::cell::cell(column column,
                     std::unique_ptr<rocksdb::Iterator> it,
                     const gopts &opts)
:c{std::move(column)}
,ss{opts.snapshot}
,it{std::move(it)}
{
}

// Linkage for incomplete rocksdb::Iterator
ircd::db::cell::cell(cell &&o)
noexcept
:c{std::move(o.c)}
,ss{std::move(o.ss)}
,it{std::move(o.it)}
{
}

// Linkage for incomplete rocksdb::Iterator
ircd::db::cell &
ircd::db::cell::operator=(cell &&o)
noexcept
{
	c = std::move(o.c);
	ss = std::move(o.ss);
	it = std::move(o.it);

	return *this;
}

// Linkage for incomplete rocksdb::Iterator
ircd::db::cell::~cell()
noexcept
{
}

bool
ircd::db::cell::load(const string_view &index,
                     gopts opts)
{
	database &d(c);
	if(valid(index) && !opts.snapshot && sequence(ss) == sequence(d))
		return true;

	if(bool(opts.snapshot))
	{
		this->it.reset();
		this->ss = std::move(opts.snapshot);
	}

	database::column &c(this->c);
	const auto _opts
	{
		make_opts(opts)
	};

	if(!seek(c, index, _opts, this->it))
		return false;

	return valid(index);
}

ircd::db::cell &
ircd::db::cell::operator=(const string_view &s)
{
	write(c, key(), s);
	return *this;
}

void
ircd::db::cell::operator()(const op &op,
                           const string_view &val,
                           const sopts &sopts)
{
	write(cell::delta{op, *this, val}, sopts);
}

ircd::db::cell::operator
string_view()
{
	return val();
}

ircd::db::cell::operator
string_view()
const
{
	return val();
}

ircd::string_view
ircd::db::cell::val()
{
	if(!valid())
		load();

	return likely(valid())? db::val(*it) : string_view{};
}

ircd::string_view
ircd::db::cell::key()
{
	if(!valid())
		load();

	return likely(valid())? db::key(*it) : string_view{};
}

ircd::string_view
ircd::db::cell::val()
const
{
	return likely(valid())? db::val(*it) : string_view{};
}

ircd::string_view
ircd::db::cell::key()
const
{
	return likely(valid())? db::key(*it) : string_view{};
}

bool
ircd::db::cell::valid(const string_view &s)
const
{
	return valid() && db::valid_eq(*it, s);
}

bool
ircd::db::cell::valid_gt(const string_view &s)
const
{
	return valid() && db::valid_gt(*it, s);
}

bool
ircd::db::cell::valid_lte(const string_view &s)
const
{
	return valid() && db::valid_lte(*it, s);
}

bool
ircd::db::cell::valid()
const
{
	return bool(it) && db::valid(*it);
}

///////////////////////////////////////////////////////////////////////////////
//
// db/domain.h
//

const ircd::db::gopts
ircd::db::domain::applied_opts
{
	get::PREFIX
};

bool
ircd::db::seek(domain::const_iterator_base &it,
               const pos &p)
{
	switch(p)
	{
		case pos::BACK:
		{
			// This is inefficient as per RocksDB's prefix impl. unknown why
			// a seek to NEXT is still needed after walking back one.
			while(seek(it, pos::NEXT));
			if(seek(it, pos::PREV))
				seek(it, pos::NEXT);

			return bool(it);
		}

		default:
			break;
	}

	it.opts |= domain::applied_opts;
	return seek(static_cast<column::const_iterator_base &>(it), p);
}

bool
ircd::db::seek(domain::const_iterator_base &it,
               const string_view &p)
{
	it.opts |= domain::applied_opts;
	return seek(static_cast<column::const_iterator_base &>(it), p);
}

ircd::db::domain::const_iterator
ircd::db::domain::begin(const string_view &key,
                        gopts opts)
{
	const_iterator ret
	{
		c, {}, std::move(opts)
	};

	seek(ret, key);
	return ret;
}

ircd::db::domain::const_iterator
ircd::db::domain::end(const string_view &key,
                      gopts opts)
{
	const_iterator ret
	{
		c, {}, std::move(opts)
	};

	if(seek(ret, key))
		seek(ret, pos::END);

	return ret;
}

/// NOTE: RocksDB says they don't support reverse iteration over a prefix range
/// This means we have to forward scan to the end and then walk back! Reverse
/// iterations of a domain should only be used for debugging and statistics! The
/// domain should be ordered the way it will be primarily accessed using the
/// comparator. If it will be accessed in different directions, make another
/// domain column.
ircd::db::domain::const_reverse_iterator
ircd::db::domain::rbegin(const string_view &key,
                         gopts opts)
{
	const_reverse_iterator ret
	{
		c, {}, std::move(opts)
	};

	if(seek(ret, key))
		seek(ret, pos::BACK);

	return ret;
}

ircd::db::domain::const_reverse_iterator
ircd::db::domain::rend(const string_view &key,
                       gopts opts)
{
	const_reverse_iterator ret
	{
		c, {}, std::move(opts)
	};

	if(seek(ret, key))
		seek(ret, pos::END);

	return ret;
}

//
// const_iterator
//

ircd::db::domain::const_iterator &
ircd::db::domain::const_iterator::operator--()
{
	if(likely(bool(*this)))
		seek(*this, pos::PREV);
	else
		seek(*this, pos::BACK);

	return *this;
}

ircd::db::domain::const_iterator &
ircd::db::domain::const_iterator::operator++()
{
	if(likely(bool(*this)))
		seek(*this, pos::NEXT);
	else
		seek(*this, pos::FRONT);

	return *this;
}

ircd::db::domain::const_reverse_iterator &
ircd::db::domain::const_reverse_iterator::operator--()
{
	if(likely(bool(*this)))
		seek(*this, pos::NEXT);
	else
		seek(*this, pos::FRONT);

	return *this;
}

ircd::db::domain::const_reverse_iterator &
ircd::db::domain::const_reverse_iterator::operator++()
{
	if(likely(bool(*this)))
		seek(*this, pos::PREV);
	else
		seek(*this, pos::BACK);

	return *this;
}

const ircd::db::domain::const_iterator_base::value_type &
ircd::db::domain::const_iterator_base::operator*()
const
{
	const auto &prefix
	{
		describe(*c).prefix
	};

	// Fetch the full value like a standard column first
	column::const_iterator_base::operator*();
	string_view &key{val.first};

	// When there's no prefixing this domain column is just
	// like a normal column. Otherwise, we remove the prefix
	// from the key the user will end up seeing.
	if(prefix.has && prefix.has(key))
	{
		const auto &first(prefix.get(key));
		const auto &second(key.substr(first.size()));
		key = second;
	}

	return val;
}

const ircd::db::domain::const_iterator_base::value_type *
ircd::db::domain::const_iterator_base::operator->()
const
{
	return &this->operator*();
}

///////////////////////////////////////////////////////////////////////////////
//
// db/column.h
//

std::string
ircd::db::read(column &column,
               const string_view &key,
               const gopts &gopts)
{
	std::string ret;
	const auto closure([&ret]
	(const string_view &src)
	{
		ret.assign(begin(src), end(src));
	});

	column(key, closure, gopts);
	return ret;
}

ircd::string_view
ircd::db::read(column &column,
               const string_view &key,
               const mutable_buffer &buf,
               const gopts &gopts)
{
	string_view ret;
	const auto closure([&ret, &buf]
	(const string_view &src)
	{
		ret = { data(buf), copy(buf, src) };
	});

	column(key, closure, gopts);
	return ret;
}

std::string
ircd::db::read(column &column,
               const string_view &key,
               bool &found,
               const gopts &gopts)
{
	std::string ret;
	const auto closure([&ret]
	(const string_view &src)
	{
		ret.assign(begin(src), end(src));
	});

	found = column(key, std::nothrow, closure, gopts);
	return ret;
}

ircd::string_view
ircd::db::read(column &column,
               const string_view &key,
               bool &found,
               const mutable_buffer &buf,
               const gopts &gopts)
{
	string_view ret;
	const auto closure([&buf, &ret]
	(const string_view &src)
	{
		ret = { data(buf), copy(buf, src) };
	});

	found = column(key, std::nothrow, closure, gopts);
	return ret;
}

rocksdb::Cache *
ircd::db::cache(column &column)
{
	database::column &c(column);
	return c.table_opts.block_cache.get();
}

rocksdb::Cache *
ircd::db::cache_compressed(column &column)
{
	database::column &c(column);
	return c.table_opts.block_cache_compressed.get();
}

const rocksdb::Cache *
ircd::db::cache(const column &column)
{
	const database::column &c(column);
	return c.table_opts.block_cache.get();
}

const rocksdb::Cache *
ircd::db::cache_compressed(const column &column)
{
	const database::column &c(column);
	return c.table_opts.block_cache_compressed.get();
}

template<>
ircd::db::prop_str
ircd::db::property(const column &column,
                   const string_view &name)
{
	std::string ret;
	database::column &c(const_cast<db::column &>(column));
	database &d(const_cast<db::column &>(column));
	if(!d.d->GetProperty(c, slice(name), &ret))
		throw not_found
		{
			"'property '%s' for column '%s' in '%s' not found.",
			name,
			db::name(column),
			db::name(d)
		};

	return ret;
}

template<>
ircd::db::prop_int
ircd::db::property(const column &column,
                   const string_view &name)
{
	uint64_t ret(0);
	database::column &c(const_cast<db::column &>(column));
	database &d(const_cast<db::column &>(column));
	if(!d.d->GetIntProperty(c, slice(name), &ret))
		throw not_found
		{
			"property '%s' for column '%s' in '%s' not found or not an integer.",
			name,
			db::name(column),
			db::name(d)
		};

	return ret;
}

template<>
ircd::db::prop_map
ircd::db::property(const column &column,
                   const string_view &name)
{
	std::map<std::string, std::string> ret;
	database::column &c(const_cast<db::column &>(column));
	database &d(const_cast<db::column &>(column));
	if(!d.d->GetMapProperty(c, slice(name), &ret))
		ret.emplace(std::string{name}, property<std::string>(column, name));

	return ret;
}

ircd::db::options
ircd::db::getopt(const column &column)
{
	database &d(const_cast<db::column &>(column));
	database::column &c(const_cast<db::column &>(column));
	return options
	{
		static_cast<rocksdb::ColumnFamilyOptions>(d.d->GetOptions(c))
	};
}

size_t
ircd::db::bytes(const column &column)
{
	rocksdb::ColumnFamilyMetaData cfm;
	database &d(const_cast<db::column &>(column));
	database::column &c(const_cast<db::column &>(column));
	assert(bool(c.handle));
	d.d->GetColumnFamilyMetaData(c.handle.get(), &cfm);
	return cfm.size;
}

size_t
ircd::db::file_count(const column &column)
{
	rocksdb::ColumnFamilyMetaData cfm;
	database &d(const_cast<db::column &>(column));
	database::column &c(const_cast<db::column &>(column));
	assert(bool(c.handle));
	d.d->GetColumnFamilyMetaData(c.handle.get(), &cfm);
	return cfm.file_count;
}

uint32_t
ircd::db::id(const column &column)
{
	const database::column &c(column);
	return id(c);
}

const std::string &
ircd::db::name(const column &column)
{
	const database::column &c(column);
	return name(c);
}

const ircd::db::descriptor &
ircd::db::describe(const column &column)
{
	const database::column &c(column);
	return describe(c);
}

std::vector<std::string>
ircd::db::files(const column &column)
{
	database::column &c(const_cast<db::column &>(column));
	database &d(*c.d);

	rocksdb::ColumnFamilyMetaData cfmd;
	d.d->GetColumnFamilyMetaData(c, &cfmd);

	size_t count(0);
	for(const auto &level : cfmd.levels)
		count += level.files.size();

	std::vector<std::string> ret;
	ret.reserve(count);
	for(auto &level : cfmd.levels)
		for(auto &file : level.files)
			ret.emplace_back(std::move(file.name));

	return ret;
}

void
ircd::db::drop(column &column)
{
	database::column &c(column);
	drop(c);
}

void
ircd::db::check(column &column)
{
	database &d(column);
	const auto &files
	{
		db::files(column)
	};

	for(const auto &file : files)
	{
		const auto &path
		{
			 // remove false leading slash; the rest is relative to db.
			lstrip(file, '/')
		};

		db::check(d, path);
	}
}

void
ircd::db::sort(column &column,
               const bool &blocking,
               const bool &now)
{
	database::column &c(column);
	database &d(*c.d);

	rocksdb::FlushOptions opts;
	opts.wait = blocking;
	opts.allow_write_stall = now;

	const ctx::uninterruptible::nothrow ui;
	const std::lock_guard lock{write_mutex};
	log::debug
	{
		log, "[%s]'%s' @%lu FLUSH (sort) %s %s",
		name(d),
		name(c),
		sequence(d),
		blocking? "blocking"_sv: "non-blocking"_sv,
		now? "now"_sv: "later"_sv
	};

	throw_on_error
	{
		d.d->Flush(opts, c)
	};
}

void
ircd::db::compact(column &column,
                  const std::pair<int, int> &level,
                  const compactor &cb)
{
	database::column &c(column);
	database &d(*c.d);

	const auto &dst_level{level.second};
	const auto &src_level{level.first};

	rocksdb::ColumnFamilyMetaData cfmd;
	d.d->GetColumnFamilyMetaData(c, &cfmd);
	for(const auto &level : cfmd.levels)
	{
		if(src_level != -1 && src_level != level.level)
			continue;

		if(level.files.empty())
			continue;

		const ctx::uninterruptible ui;
		const std::lock_guard lock
		{
			write_mutex
		};

		const auto &to_level
		{
			dst_level > -1? dst_level : level.level
		};

		rocksdb::CompactionOptions opts;
		opts.output_file_size_limit = 1_GiB; //TODO: conf

		// RocksDB sez that setting this to Disable means that the column's
		// compression options are read instead. If we don't set this here,
		// rocksdb defaults to "snappy" (which is strange).
		opts.compression = rocksdb::kDisableCompressionOption;

		std::vector<std::string> files(level.files.size());
		std::transform(level.files.begin(), level.files.end(), files.begin(), []
		(auto &metadata)
		{
			return std::move(metadata.name);
		});

		// Save and restore the existing filter callback so we can allow our
		// caller to use theirs. Note that this manual compaction should be
		// exclusive for this column (no background compaction should be
		// occurring, at least one relying on this filter).
		auto their_filter(std::move(c.cfilter.user));
		const unwind unfilter{[&c, &their_filter]
		{
			c.cfilter.user = std::move(their_filter);
		}};

		c.cfilter.user = cb;

		log::debug
		{
			log, "[%s]'%s' COMPACT L%d -> L%d files:%zu size:%zu",
			name(d),
			name(c),
			level.level,
			to_level,
			level.files.size(),
			level.size
		};

		throw_on_error
		{
			d.d->CompactFiles(opts, c, files, to_level)
		};
	}
}

void
ircd::db::compact(column &column,
                  const std::pair<string_view, string_view> &range,
                  const int &to_level,
                  const compactor &cb)
{
	database &d(column);
	database::column &c(column);
	const ctx::uninterruptible ui;

	const auto begin(slice(range.first));
	const rocksdb::Slice *const b
	{
		empty(range.first)? nullptr : &begin
	};

	const auto end(slice(range.second));
	const rocksdb::Slice *const e
	{
		empty(range.second)? nullptr : &end
	};

	rocksdb::CompactRangeOptions opts;
	opts.exclusive_manual_compaction = true;
	opts.allow_write_stall = true;
	opts.change_level = true;
	opts.target_level = std::max(to_level, -1);
	opts.bottommost_level_compaction = rocksdb::BottommostLevelCompaction::kForce;

	// Save and restore the existing filter callback so we can allow our
	// caller to use theirs. Note that this manual compaction should be
	// exclusive for this column (no background compaction should be
	// occurring, at least one relying on this filter).
	auto their_filter(std::move(c.cfilter.user));
	const unwind unfilter{[&c, &their_filter]
	{
		c.cfilter.user = std::move(their_filter);
	}};

	c.cfilter.user = cb;

	log::debug
	{
		log, "[%s]'%s' @%lu COMPACT [%s, %s] -> L:%d (Lmax:%d Lbase:%d)",
		name(d),
		name(c),
		sequence(d),
		range.first,
		range.second,
		opts.target_level,
		d.d->NumberLevels(c),
		d.d->MaxMemCompactionLevel(c),
	};

	throw_on_error
	{
		d.d->CompactRange(opts, c, b, e)
	};
}

void
ircd::db::setopt(column &column,
                 const string_view &key,
                 const string_view &val)
{
	database &d(column);
	database::column &c(column);
	const std::unordered_map<std::string, std::string> options
	{
		{ std::string{key}, std::string{val} }
	};

	throw_on_error
	{
		d.d->SetOptions(c, options)
	};
}

void
ircd::db::ingest(column &column,
                 const string_view &path)
{
	database &d(column);
	database::column &c(column);

	rocksdb::IngestExternalFileOptions opts;
	opts.allow_global_seqno = true;
	opts.allow_blocking_flush = true;

	// Automatically determine if we can avoid issuing new sequence
	// numbers by considering this ingestion as "backfill" of missing
	// data which did actually exist but was physically removed.
	const auto &copts{d.d->GetOptions(c)};
	opts.ingest_behind = copts.allow_ingest_behind;

	const std::vector<std::string> files
	{
		{ std::string{path} }
	};

	const std::lock_guard lock{write_mutex};
	const ctx::uninterruptible::nothrow ui;
	throw_on_error
	{
		d.d->IngestExternalFile(c, files, opts)
	};
}

void
ircd::db::del(column &column,
              const std::pair<string_view, string_view> &range,
              const sopts &sopts)
{
	database &d(column);
	database::column &c(column);
	auto opts(make_opts(sopts));

	const std::lock_guard lock{write_mutex};
	const ctx::uninterruptible::nothrow ui;
	const ctx::stack_usage_assertion sua;
	log::debug
	{
		log, "'%s' %lu '%s' RANGE DELETE",
		name(d),
		sequence(d),
		name(c),
	};

	throw_on_error
	{
		d.d->DeleteRange(opts, c, slice(range.first), slice(range.second))
	};
}

void
ircd::db::del(column &column,
              const string_view &key,
              const sopts &sopts)
{
	database &d(column);
	database::column &c(column);
	auto opts(make_opts(sopts));

	const std::lock_guard lock{write_mutex};
	const ctx::uninterruptible::nothrow ui;
	const ctx::stack_usage_assertion sua;
	log::debug
	{
		log, "'%s' %lu '%s' DELETE key(%zu B)",
		name(d),
		sequence(d),
		name(c),
		key.size()
	};

	throw_on_error
	{
		d.d->Delete(opts, c, slice(key))
	};
}

void
ircd::db::write(column &column,
                const string_view &key,
                const const_buffer &val,
                const sopts &sopts)
{
	database &d(column);
	database::column &c(column);
	auto opts(make_opts(sopts));

	const std::lock_guard lock{write_mutex};
	const ctx::uninterruptible::nothrow ui;
	const ctx::stack_usage_assertion sua;
	log::debug
	{
		log, "'%s' %lu '%s' PUT key(%zu B) val(%zu B)",
		name(d),
		sequence(d),
		name(c),
		size(key),
		size(val)
	};

	throw_on_error
	{
		d.d->Put(opts, c, slice(key), slice(val))
	};
}

size_t
ircd::db::bytes_value(column &column,
                      const string_view &key,
                      const gopts &gopts)
{
	size_t ret{0};
	column(key, std::nothrow, gopts, [&ret]
	(const string_view &value)
	{
		ret = value.size();
	});

	return ret;
}

size_t
ircd::db::bytes(column &column,
                const std::pair<string_view, string_view> &key,
                const gopts &gopts)
{
	database &d(column);
	database::column &c(column);
	const rocksdb::Range range[1]
	{
		{ slice(key.first), slice(key.second) }
	};

	uint64_t ret[1] {0};
	d.d->GetApproximateSizes(c, range, 1, ret);
	return ret[0];
}

//
// db::prefetch
//

bool
ircd::db::prefetch(column &column,
                   const string_view &key,
                   const gopts &gopts)
{
	static construction instance
	{
		[] { prefetcher = new struct prefetcher(); }
	};

	assert(prefetcher);
	return (*prefetcher)(column, key, gopts);
}

//
// db::cached
//

#if 0
bool
ircd::db::cached(column &column,
                 const string_view &key,
                 const gopts &gopts)
{
	return exists(cache(column), key);
}
#endif

bool
ircd::db::cached(column &column,
                 const string_view &key,
                 const gopts &gopts)
{
	database &d(column);
	database::column &c(column);

	auto opts(make_opts(gopts));
	opts.read_tier = NON_BLOCKING;
	opts.fill_cache = false;

	std::unique_ptr<rocksdb::Iterator> it;
	if(!seek(c, key, opts, it))
		return false;

	assert(bool(it));
	return valid_eq(*it, key);
}

bool
ircd::db::has(column &column,
              const string_view &key,
              const gopts &gopts)
{
	database &d(column);
	database::column &c(column);

	// Perform a co-RP query to the filtration
	//
	// NOTE disabled for rocksdb >= v5.15 due to a regression
	// where rocksdb does not init SuperVersion data in the column
	// family handle and this codepath triggers null derefs and ub.
	//
	// NOTE works on rocksdb 6.6.4 but unconditionally copies value.
	auto opts(make_opts(gopts));
	if(c.table_opts.filter_policy && (false))
	{
		auto opts(make_opts(gopts));
		const scope_restore read_tier
		{
			opts.read_tier, NON_BLOCKING
		};

		const scope_restore fill_cache
		{
			opts.fill_cache, false
		};

		std::string discard;
		bool value_found {false};
		const bool key_may_exist
		{
			d.d->KeyMayExist(opts, c, slice(key), &discard, &value_found)
		};

		if(!key_may_exist)
			return false;

		if(value_found)
			return true;
	}

	std::unique_ptr<rocksdb::Iterator> it;
	if(!seek(c, key, opts, it))
		return false;

	assert(bool(it));
	return valid_eq(*it, key);
}

uint64_t
ircd::db::has(column &column,
              const vector_view<const string_view> &key,
              const gopts &opts)
{
	const size_t num(key.size());
	return has({&column, 1}, key, opts);
}

uint64_t
ircd::db::has(const vector_view<column> &c,
              const vector_view<const string_view> &key,
              const gopts &gopts)
{
	if(c.empty())
		return 0UL;

	const size_t num(key.size());
	if(unlikely(!num || num > 64))
		throw std::out_of_range
		{
			"db::has() :too many columns or vector size mismatch"
		};

	_read_op op[num];
	for(size_t i(0); i < num; ++i)
		op[i] =
		{
			c[std::min(c.size() - 1, i)], key[i]
		};

	uint64_t i(0), ret(0);
	auto opts(make_opts(gopts));
	_read({op, num}, opts, [&i, &ret, &opts]
	(column &, const column::delta &, const rocksdb::Status &s)
	{
		uint64_t found {0};
		found |= s.ok();
		found |= s.IsIncomplete() & (opts.read_tier == NON_BLOCKING);
		ret |= (found << i++);
		return true;
	});

	return ret;
}

//
// column
//

ircd::db::column::column(database &d,
                         const string_view &column_name,
                         const std::nothrow_t)
:c{[&d, &column_name]
{
	const int32_t cfid
	{
		d.cfid(std::nothrow, column_name)
	};

	return cfid >= 0?
		&d[cfid]:
		nullptr;
}()}
{
}

ircd::db::column::column(database &d,
                         const string_view &column_name)
:column
{
	d[column_name]
}
{
}

ircd::db::column::column(database::column &c)
:c{&c}
{
}

void
ircd::db::column::operator()(const delta &delta,
                             const sopts &sopts)
{
	operator()(&delta, &delta + 1, sopts);
}

void
ircd::db::column::operator()(const sopts &sopts,
                             const std::initializer_list<delta> &deltas)
{
	operator()(deltas, sopts);
}

void
ircd::db::column::operator()(const std::initializer_list<delta> &deltas,
                             const sopts &sopts)
{
	operator()(std::begin(deltas), std::end(deltas), sopts);
}

void
ircd::db::column::operator()(const delta *const &begin,
                             const delta *const &end,
                             const sopts &sopts)
{
	database &d(*this);

	rocksdb::WriteBatch batch;
	std::for_each(begin, end, [this, &batch]
	(const delta &delta)
	{
		append(batch, *this, delta);
	});

	commit(d, batch, sopts);
}

void
ircd::db::column::operator()(const string_view &key,
                             const gopts &gopts,
                             const view_closure &func)
{
	return operator()(key, func, gopts);
}

void
ircd::db::column::operator()(const string_view &key,
                             const view_closure &func,
                             const gopts &gopts)
{
	const auto it(seek(*this, key, gopts));
	valid_eq_or_throw(*it, key);
	func(val(*it));
}

bool
ircd::db::column::operator()(const string_view &key,
                             const std::nothrow_t,
                             const gopts &gopts,
                             const view_closure &func)
{
	return operator()(key, std::nothrow, func, gopts);
}

bool
ircd::db::column::operator()(const string_view &key,
                             const std::nothrow_t,
                             const view_closure &func,
                             const gopts &gopts)
{
	const auto it(seek(*this, key, gopts));
	if(!valid_eq(*it, key))
		return false;

	func(val(*it));
	return true;
}

ircd::db::cell
ircd::db::column::operator[](const string_view &key)
const
{
	return { *this, key };
}

ircd::db::column::operator
bool()
const
{
	return c?
		!dropped(*c):
		false;
}

ircd::db::column::operator
const descriptor &()
const
{
	assert(c->descriptor);
	return *c->descriptor;
}

//
// column::const_iterator
//

ircd::db::column::const_iterator
ircd::db::column::end(gopts gopts)
{
	const_iterator ret
	{
		c, {}, std::move(gopts)
	};

	seek(ret, pos::END);
	return ret;
}

ircd::db::column::const_iterator
ircd::db::column::last(gopts gopts)
{
	const_iterator ret
	{
		c, {}, std::move(gopts)
	};

	seek(ret, pos::BACK);
	return ret;
}

ircd::db::column::const_iterator
ircd::db::column::begin(gopts gopts)
{
	const_iterator ret
	{
		c, {}, std::move(gopts)
	};

	seek(ret, pos::FRONT);
	return ret;
}

ircd::db::column::const_reverse_iterator
ircd::db::column::rend(gopts gopts)
{
	const_reverse_iterator ret
	{
		c, {}, std::move(gopts)
	};

	seek(ret, pos::END);
	return ret;
}

ircd::db::column::const_reverse_iterator
ircd::db::column::rbegin(gopts gopts)
{
	const_reverse_iterator ret
	{
		c, {}, std::move(gopts)
	};

	seek(ret, pos::BACK);
	return ret;
}

ircd::db::column::const_iterator
ircd::db::column::upper_bound(const string_view &key,
                              gopts gopts)
{
	auto it(lower_bound(key, std::move(gopts)));
	if(it && it.it->key().compare(slice(key)) == 0)
		++it;

	return it;
}

ircd::db::column::const_iterator
ircd::db::column::find(const string_view &key,
                       gopts gopts)
{
	auto it(lower_bound(key, gopts));
	if(!it || it.it->key().compare(slice(key)) != 0)
		return end(gopts);

	return it;
}

ircd::db::column::const_iterator
ircd::db::column::lower_bound(const string_view &key,
                              gopts gopts)
{
	const_iterator ret
	{
		c, {}, std::move(gopts)
	};

	seek(ret, key);
	return ret;
}

ircd::db::column::const_iterator &
ircd::db::column::const_iterator::operator--()
{
	if(likely(bool(*this)))
		seek(*this, pos::PREV);
	else
		seek(*this, pos::BACK);

	return *this;
}

ircd::db::column::const_iterator &
ircd::db::column::const_iterator::operator++()
{
	if(likely(bool(*this)))
		seek(*this, pos::NEXT);
	else
		seek(*this, pos::FRONT);

	return *this;
}

ircd::db::column::const_reverse_iterator &
ircd::db::column::const_reverse_iterator::operator--()
{
	if(likely(bool(*this)))
		seek(*this, pos::NEXT);
	else
		seek(*this, pos::FRONT);

	return *this;
}

ircd::db::column::const_reverse_iterator &
ircd::db::column::const_reverse_iterator::operator++()
{
	if(likely(bool(*this)))
		seek(*this, pos::PREV);
	else
		seek(*this, pos::BACK);

	return *this;
}

ircd::db::column::const_iterator_base::const_iterator_base(const_iterator_base &&o)
noexcept
:c{std::move(o.c)}
,opts{std::move(o.opts)}
,it{std::move(o.it)}
,val{std::move(o.val)}
{
}

ircd::db::column::const_iterator_base &
ircd::db::column::const_iterator_base::operator=(const_iterator_base &&o)
noexcept
{
	c = std::move(o.c);
	opts = std::move(o.opts);
	it = std::move(o.it);
	val = std::move(o.val);
	return *this;
}

// linkage for incmplete rocksdb::Iterator
ircd::db::column::const_iterator_base::const_iterator_base()
noexcept
{
}

// linkage for incmplete rocksdb::Iterator
ircd::db::column::const_iterator_base::~const_iterator_base()
noexcept
{
}

ircd::db::column::const_iterator_base::const_iterator_base(database::column *const &c,
                                                           std::unique_ptr<rocksdb::Iterator> &&it,
                                                           gopts opts)
noexcept
:c{c}
,opts{std::move(opts)}
,it{std::move(it)}
{
}

const ircd::db::column::const_iterator_base::value_type &
ircd::db::column::const_iterator_base::operator*()
const
{
	assert(it && valid(*it));
	val.first = db::key(*it);
	val.second = db::val(*it);
	return val;
}

const ircd::db::column::const_iterator_base::value_type *
ircd::db::column::const_iterator_base::operator->()
const
{
	return &operator*();
}

bool
ircd::db::column::const_iterator_base::operator!()
const noexcept
{
	return !static_cast<bool>(*this);
}

ircd::db::column::const_iterator_base::operator bool()
const noexcept
{
	if(!it)
		return false;

	if(!valid(*it))
		return false;

	return true;
}

bool
ircd::db::operator!=(const column::const_iterator_base &a, const column::const_iterator_base &b)
noexcept
{
	return !(a == b);
}

bool
ircd::db::operator==(const column::const_iterator_base &a, const column::const_iterator_base &b)
noexcept
{
	if(a && b)
	{
		const auto &ak(a.it->key());
		const auto &bk(b.it->key());
		return ak.compare(bk) == 0;
	}

	if(!a && !b)
		return true;

	return false;
}

bool
ircd::db::operator>(const column::const_iterator_base &a, const column::const_iterator_base &b)
noexcept
{
	if(a && b)
	{
		const auto &ak(a.it->key());
		const auto &bk(b.it->key());
		return ak.compare(bk) == 1;
	}

	if(!a && b)
		return true;

	if(!a && !b)
		return false;

	assert(!a && b);
	return false;
}

bool
ircd::db::operator<(const column::const_iterator_base &a, const column::const_iterator_base &b)
noexcept
{
	if(a && b)
	{
		const auto &ak(a.it->key());
		const auto &bk(b.it->key());
		return ak.compare(bk) == -1;
	}

	if(!a && b)
		return false;

	if(!a && !b)
		return false;

	assert(a && !b);
	return true;
}

template<class pos>
bool
ircd::db::seek(column::const_iterator_base &it,
               const pos &p)
{
	database::column &c(it);
	return seek(c, p, make_opts(it.opts), it.it);
}
template bool ircd::db::seek<ircd::db::pos>(column::const_iterator_base &, const pos &);
template bool ircd::db::seek<ircd::string_view>(column::const_iterator_base &, const string_view &);

///////////////////////////////////////////////////////////////////////////////
//
// opts.h
//

//
// options
//

ircd::db::options::options(const database &d)
:options{d.d->GetDBOptions()}
{
}

ircd::db::options::options(const database::column &c)
:options
{
	rocksdb::ColumnFamilyOptions
	{
		c.d->d->GetOptions(c.handle.get())
	}
}{}

ircd::db::options::options(const rocksdb::DBOptions &opts)
{
	throw_on_error
	{
		rocksdb::GetStringFromDBOptions(this, opts)
	};
}

ircd::db::options::options(const rocksdb::ColumnFamilyOptions &opts)
{
	throw_on_error
	{
		rocksdb::GetStringFromColumnFamilyOptions(this, opts)
	};
}

ircd::db::options::operator rocksdb::PlainTableOptions()
const
{
	rocksdb::PlainTableOptions ret;
	throw_on_error
	{
		rocksdb::GetPlainTableOptionsFromString(ret, *this, &ret)
	};

	return ret;
}

ircd::db::options::operator rocksdb::BlockBasedTableOptions()
const
{
	rocksdb::BlockBasedTableOptions ret;
	throw_on_error
	{
		rocksdb::GetBlockBasedTableOptionsFromString(ret, *this, &ret)
	};

	return ret;
}

ircd::db::options::operator rocksdb::ColumnFamilyOptions()
const
{
	rocksdb::ColumnFamilyOptions ret;
	throw_on_error
	{
		rocksdb::GetColumnFamilyOptionsFromString(ret, *this, &ret)
	};

	return ret;
}

ircd::db::options::operator rocksdb::DBOptions()
const
{
	rocksdb::DBOptions ret;
	throw_on_error
	{
		rocksdb::GetDBOptionsFromString(ret, *this, &ret)
	};

	return ret;
}

ircd::db::options::operator rocksdb::Options()
const
{
	rocksdb::Options ret;
	throw_on_error
	{
		rocksdb::GetOptionsFromString(ret, *this, &ret)
	};

	return ret;
}

//
// options::map
//

ircd::db::options::map::map(const options &o)
{
	throw_on_error
	{
		rocksdb::StringToMap(o, this)
	};
}

ircd::db::options::map::operator rocksdb::PlainTableOptions()
const
{
	rocksdb::PlainTableOptions ret;
	throw_on_error
	{
		rocksdb::GetPlainTableOptionsFromMap(ret, *this, &ret)
	};

	return ret;
}

ircd::db::options::map::operator rocksdb::BlockBasedTableOptions()
const
{
	rocksdb::BlockBasedTableOptions ret;
	throw_on_error
	{
		rocksdb::GetBlockBasedTableOptionsFromMap(ret, *this, &ret)
	};

	return ret;
}

ircd::db::options::map::operator rocksdb::ColumnFamilyOptions()
const
{
	rocksdb::ColumnFamilyOptions ret;
	throw_on_error
	{
		rocksdb::GetColumnFamilyOptionsFromMap(ret, *this, &ret)
	};

	return ret;
}

ircd::db::options::map::operator rocksdb::DBOptions()
const
{
	rocksdb::DBOptions ret;
	throw_on_error
	{
		rocksdb::GetDBOptionsFromMap(ret, *this, &ret)
	};

	return ret;
}

///////////////////////////////////////////////////////////////////////////////
//
// cache.h
//

void
ircd::db::clear(rocksdb::Cache &cache)
{
	cache.EraseUnRefEntries();
}

bool
ircd::db::remove(rocksdb::Cache &cache,
                 const string_view &key)
{
	cache.Erase(slice(key));
	return true;
}

bool
ircd::db::insert(rocksdb::Cache &cache,
                 const string_view &key,
                 const string_view &value)
{
	unique_buffer<const_buffer> buf
	{
		const_buffer{value}
	};

	return insert(cache, key, std::move(buf));
}

bool
ircd::db::insert(rocksdb::Cache &cache,
                 const string_view &key,
                 unique_buffer<const_buffer> &&value)
{
	const size_t value_size
	{
		size(value)
	};

	static const auto deleter{[]
	(const rocksdb::Slice &key, void *const value)
	{
		delete[] reinterpret_cast<const char *>(value);
	}};

	// Note that because of the nullptr handle argument below, rocksdb
	// will run the deleter if the insert throws; just make sure
	// the argument execution doesn't throw after release()
	throw_on_error
	{
		cache.Insert(slice(key),
		             const_cast<char *>(data(value.release())),
		             value_size,
		             deleter,
		             nullptr)
	};

	return true;
}

void
ircd::db::for_each(const rocksdb::Cache &cache,
                   const cache_closure &closure)
{
	// Due to the use of the global variables which are required when using a
	// C-style callback for RocksDB, we have to make use of this function
	// exclusive for different contexts.
	thread_local ctx::mutex mutex;
	const std::lock_guard lock{mutex};

	thread_local rocksdb::Cache *_cache;
	_cache = const_cast<rocksdb::Cache *>(&cache);

	thread_local const cache_closure *_closure;
	_closure = &closure;

	_cache->ApplyToAllCacheEntries([]
	(void *const value_buffer, const size_t buffer_size)
	noexcept
	{
		assert(_cache);
		assert(_closure);
		const const_buffer buf
		{
			reinterpret_cast<const char *>(value_buffer), buffer_size
		};

		(*_closure)(buf);
	},
	true);
}

#ifdef IRCD_DB_HAS_CACHE_GETCHARGE
size_t
ircd::db::charge(const rocksdb::Cache &cache_,
                 const string_view &key)
{
	auto &cache
	{
		const_cast<rocksdb::Cache &>(cache_)
	};

	const custom_ptr<rocksdb::Cache::Handle> handle
	{
		cache.Lookup(slice(key)), [&cache](auto *const &handle)
		{
			cache.Release(handle);
		}
	};

	return cache.GetCharge(handle);
}
#else
size_t
ircd::db::charge(const rocksdb::Cache &cache,
                 const string_view &key)
{
	return 0UL;
}
#endif

[[gnu::hot]]
bool
ircd::db::exists(const rocksdb::Cache &cache_,
                 const string_view &key)
{
	auto &cache
	{
		const_cast<rocksdb::Cache &>(cache_)
	};

	const custom_ptr<rocksdb::Cache::Handle> handle
	{
		cache.Lookup(slice(key)), [&cache](auto *const &handle)
		{
			cache.Release(handle);
		}
	};

	return bool(handle);
}

size_t
ircd::db::pinned(const rocksdb::Cache &cache)
{
	return cache.GetPinnedUsage();
}

size_t
ircd::db::usage(const rocksdb::Cache &cache)
{
	return cache.GetUsage();
}

void
ircd::db::capacity(rocksdb::Cache &cache,
                   const size_t &cap)
{
	cache.SetCapacity(cap);
}

size_t
ircd::db::capacity(const rocksdb::Cache &cache)
{
	return cache.GetCapacity();
}

const uint64_t &
ircd::db::ticker(const rocksdb::Cache &cache,
                 const uint32_t &ticker_id)
{
	const auto &c
	{
		dynamic_cast<const database::cache &>(cache)
	};

	static const uint64_t &zero
	{
		0ULL
	};

	return c.stats?
		c.stats->ticker.at(ticker_id):
		zero;
}

///////////////////////////////////////////////////////////////////////////////
//
// error.h
//

//
// error::not_found
//

decltype(ircd::db::error::not_found::_not_found_)
ircd::db::error::not_found::_not_found_
{
	rocksdb::Status::NotFound()
};

//
// error::not_found::not_found
//

ircd::db::error::not_found::not_found()
:error
{
	generate_skip, _not_found_
}
{
	strlcpy(buf, "NotFound");
}

//
// error
//

decltype(ircd::db::error::_no_code_)
ircd::db::error::_no_code_
{
	rocksdb::Status::OK()
};

//
// error::error
//

ircd::db::error::error(internal_t,
                       const rocksdb::Status &s,
                       const string_view &fmt,
                       const va_rtti &ap)
:error
{
	s
}
{
	const string_view &msg{buf};
	const mutable_buffer remain
	{
		buf + size(msg), sizeof(buf) - size(msg)
	};

	fmt::vsprintf
	{
		remain, fmt, ap
	};
}

ircd::db::error::error(const rocksdb::Status &s)
:error
{
	generate_skip, s
}
{
	fmt::sprintf
	{
		buf, "(%u:%u:%u) %s %s :%s",
		this->code,
		this->subcode,
		this->severity,
		reflect(rocksdb::Status::Severity(this->severity)),
		reflect(rocksdb::Status::Code(this->code)),
		s.getState(),
	};
}

ircd::db::error::error(generate_skip_t,
                       const rocksdb::Status &s)
:ircd::error
{
	generate_skip
}
,code
{
	s.code()
}
,subcode
{
	s.subcode()
}
,severity
{
	s.severity()?
		s.severity():

	code == rocksdb::Status::kCorruption?
		rocksdb::Status::kHardError:

	rocksdb::Status::kNoError
}
{
}

///////////////////////////////////////////////////////////////////////////////
//
// merge.h
//

std::string
__attribute__((noreturn))
ircd::db::merge_operator(const string_view &key,
                         const std::pair<string_view, string_view> &delta)
{
	//ircd::json::index index{delta.first};
	//index += delta.second;
	//return index;

	throw ircd::not_implemented
	{
		"db::merge_operator()"
	};
}

///////////////////////////////////////////////////////////////////////////////
//
// comparator.h
//

//
// linkage placements for integer comparators so they all have the same addr
//

ircd::db::cmp_int64_t::cmp_int64_t()
{
}

ircd::db::cmp_int64_t::~cmp_int64_t()
noexcept
{
}

ircd::db::cmp_uint64_t::cmp_uint64_t()
{
}

ircd::db::cmp_uint64_t::~cmp_uint64_t()
noexcept
{
}

ircd::db::reverse_cmp_int64_t::reverse_cmp_int64_t()
{
}

ircd::db::reverse_cmp_int64_t::~reverse_cmp_int64_t()
noexcept
{
}

ircd::db::reverse_cmp_uint64_t::reverse_cmp_uint64_t()
{
}

ircd::db::reverse_cmp_uint64_t::~reverse_cmp_uint64_t()
noexcept
{
}

//
// cmp_string_view
//

ircd::db::cmp_string_view::cmp_string_view()
:db::comparator{"string_view", &less, &equal}
{
}

//
// reverse_cmp_string_view
//

ircd::db::reverse_cmp_string_view::reverse_cmp_string_view()
:db::comparator{"reverse_string_view", &less, &equal}
{
}

bool
ircd::db::reverse_cmp_string_view::less(const string_view &a,
                                        const string_view &b)
noexcept
{
	/// RocksDB sez things will not work correctly unless a shorter string
	/// result returns less than a longer string even if one intends some
	/// reverse ordering
	if(a.size() < b.size())
		return true;

	/// Furthermore, b.size() < a.size() returning false from this function
	/// appears to not be correct. The reversal also has to also come in
	/// the form of a bytewise forward iteration.
	return std::memcmp(a.data(), b.data(), std::min(a.size(), b.size())) > 0;
}

///////////////////////////////////////////////////////////////////////////////
//
// delta.h
//

bool
ircd::db::value_required(const op &op)
{
	switch(op)
	{
		case op::SET:
		case op::MERGE:
		case op::DELETE_RANGE:
			return true;

		case op::GET:
		case op::DELETE:
		case op::SINGLE_DELETE:
			return false;
	}

	assert(0);
	return false;
}

///////////////////////////////////////////////////////////////////////////////
//
// db.h (internal)
//

//
// throw_on_error
//

ircd::db::throw_on_error::throw_on_error(const rocksdb::Status &status)
{
	using rocksdb::Status;

	switch(status.code())
	{
		case Status::kOk:
			return;

		case Status::kNotFound:
			throw not_found{};

		#ifdef RB_DEBUG
		//case Status::kCorruption:
		case Status::kNotSupported:
		case Status::kInvalidArgument:
			debugtrap();
			[[fallthrough]];
		#endif

		default:
			throw error
			{
				status
			};
	}
}

//
// error_to_status
//

ircd::db::error_to_status::error_to_status(const std::exception &e)
:rocksdb::Status
{
	Status::Aborted(slice(string_view(e.what())))
}
{
}

ircd::db::error_to_status::error_to_status(const std::system_error &e)
:error_to_status{e.code()}
{
}

ircd::db::error_to_status::error_to_status(const std::error_code &e)
:rocksdb::Status{[&e]
{
	using std::errc;

	switch(e.value())
	{
		case 0:
			return Status::OK();

		case int(errc::no_such_file_or_directory):
			return Status::NotFound();

		case int(errc::not_supported):
			return Status::NotSupported();

		case int(errc::invalid_argument):
			return Status::InvalidArgument();

		case int(errc::io_error):
			 return Status::IOError();

		case int(errc::timed_out):
			return Status::TimedOut();

		case int(errc::device_or_resource_busy):
			return Status::Busy();

		case int(errc::resource_unavailable_try_again):
			return Status::TryAgain();

		case int(errc::no_space_on_device):
			return Status::NoSpace();

		case int(errc::not_enough_memory):
			return Status::MemoryLimit();

		default:
			return Status::Aborted(slice(string_view(e.message())));
	}
}()}
{
}

//
// writebatch suite
//

void
ircd::db::append(rocksdb::WriteBatch &batch,
                 const cell::delta &delta)
{
	auto &column
	{
		std::get<cell *>(delta)->c
	};

	append(batch, column, column::delta
	{
		std::get<op>(delta),
		std::get<cell *>(delta)->key(),
		std::get<string_view>(delta)
	});
}

void
ircd::db::append(rocksdb::WriteBatch &batch,
                 column &column,
                 const column::delta &delta)
{
	if(unlikely(!column))
	{
		// Note: Unknown at this time whether allowing attempts at writing
		// to a null column should be erroneous or silently ignored. It's
		// highly likely this log message will be removed soon to allow
		// toggling database columns for optimization without touching calls.
		log::critical
		{
			log, "Attempting to transact a delta for a null column"
		};

		return;
	}

	database::column &c(column);
	const auto k(slice(std::get<1>(delta)));
	const auto v(slice(std::get<2>(delta)));
	switch(std::get<0>(delta))
	{
		case op::GET:            assert(0);                    break;
		case op::SET:            batch.Put(c, k, v);           break;
		case op::MERGE:          batch.Merge(c, k, v);         break;
		case op::DELETE:         batch.Delete(c, k);           break;
		case op::DELETE_RANGE:   batch.DeleteRange(c, k, v);   break;
		case op::SINGLE_DELETE:  batch.SingleDelete(c, k);     break;
	}
}

void
ircd::db::commit(database &d,
                 rocksdb::WriteBatch &batch,
                 const sopts &sopts)
{
	const auto opts(make_opts(sopts));
	commit(d, batch, opts);
}

void
ircd::db::commit(database &d,
                 rocksdb::WriteBatch &batch,
                 const rocksdb::WriteOptions &opts)
{
	#ifdef RB_DEBUG
	ircd::timer timer;
	#endif

	const std::lock_guard lock{write_mutex};
	const ctx::uninterruptible ui;
	const ctx::stack_usage_assertion sua;
	throw_on_error
	{
		d.d->Write(opts, &batch)
	};

	#ifdef RB_DEBUG
	log::debug
	{
		log, "[%s] %lu COMMIT %s in %ld$us",
		d.name,
		sequence(d),
		debug(batch),
		timer.at<microseconds>().count()
	};
	#endif
}

std::string
ircd::db::debug(const rocksdb::WriteBatch &batch)
{
	return ircd::string(512, [&batch]
	(const mutable_buffer &ret)
	{
		return snprintf(data(ret), size(ret)+1,
		                "%d deltas; size:%zuB %s+%s+%s+%s+%s+%s+%s+%s+%s",
		                batch.Count(),
		                batch.GetDataSize(),
		                batch.HasPut()? "PUT" : "",
		                batch.HasDelete()? "DEL" : "",
		                batch.HasSingleDelete()? "SDL" : "",
		                batch.HasDeleteRange()? "DRG" : "",
		                batch.HasMerge()? "MRG" : "",
		                batch.HasBeginPrepare()? "BEG" : "",
		                batch.HasEndPrepare()? "END" : "",
		                batch.HasCommit()? "COM-" : "",
		                batch.HasRollback()? "RB^" : "");
	});
}

bool
ircd::db::has(const rocksdb::WriteBatch &wb,
              const op &op)
{
	switch(op)
	{
		case op::GET:              assert(0); return false;
		case op::SET:              return wb.HasPut();
		case op::MERGE:            return wb.HasMerge();
		case op::DELETE:           return wb.HasDelete();
		case op::DELETE_RANGE:     return wb.HasDeleteRange();
		case op::SINGLE_DELETE:    return wb.HasSingleDelete();
	}

	return false;
}

//
// read suite
//

namespace ircd::db
{
	static rocksdb::Status _seek(database::column &, rocksdb::PinnableSlice &, const string_view &, const rocksdb::ReadOptions &);
}

rocksdb::Status
ircd::db::_read(column &column,
                const string_view &key,
                const rocksdb::ReadOptions &opts,
                const column::view_closure &closure)
{
	std::string buf;
	rocksdb::PinnableSlice ps
	{
		&buf
	};

	database::column &c(column);
	const rocksdb::Status ret
	{
		_seek(c, ps, key, opts)
	};

	if(!valid(ret))
		return ret;

	const string_view value
	{
		slice(ps)
	};

	if(likely(closure))
		closure(value);

	// Update stats about whether the pinnable slices we obtained have internal
	// copies or referencing the cache copy.
	database &d(column);
	c.stats->get_referenced += buf.empty();
	d.stats->get_referenced += buf.empty();
	c.stats->get_copied += !buf.empty();
	d.stats->get_copied += !buf.empty();
	return ret;
}

rocksdb::Status
ircd::db::_seek(database::column &c,
                rocksdb::PinnableSlice &s,
                const string_view &key,
                const rocksdb::ReadOptions &ropts)
{
	const ctx::uninterruptible::nothrow ui;
	const ctx::stack_usage_assertion sua;

	rocksdb::ColumnFamilyHandle *const &cf(c);
	database &d(*c.d);

	#ifdef RB_DEBUG_DB_SEEK
	const ircd::timer timer;
	#endif

	const rocksdb::Status ret
	{
		d.d->Get(ropts, cf, slice(key), &s)
	};

	#ifdef RB_DEBUG_DB_SEEK
	log::debug
	{
		log, "[%s] %lu:%lu SEEK %s in %ld$us '%s'",
		name(d),
		sequence(d),
		sequence(ropts.snapshot),
		ret.ToString(),
		timer.at<microseconds>().count(),
		name(c)
	};
	#endif

	return ret;
}

//
// parallel read suite
//

namespace ircd::db
{
	static void _seek(const vector_view<_read_op> &, const vector_view<rocksdb::Status> &, const vector_view<rocksdb::PinnableSlice> &, const rocksdb::ReadOptions &);
}

bool
ircd::db::_read(const vector_view<_read_op> &op,
                const rocksdb::ReadOptions &ropts,
                const _read_closure &closure)
{
	assert(op.size() >= 1);
	assert(op.size() <= IOV_MAX);
	const size_t &num
	{
		op.size()
	};

	std::string buf[num];
	rocksdb::PinnableSlice val[num];
	for(size_t i(0); i < num; ++i)
		new (val + i) rocksdb::PinnableSlice
		{
			buf + i
		};

	const bool parallelize
	{
		#ifdef IRCD_DB_HAS_MULTIGET_DIRECT
			true && num > 1
		#else
			false
		#endif
	};

	rocksdb::Status status[num];
	if(!parallelize)
		for(size_t i(0); i < num; ++i)
		{
			database::column &column(std::get<column>(op[i]));
			status[i] = _seek(column, val[i], std::get<1>(op[i]), ropts);
		}
	else
		_seek(op, {status, num}, {val, num}, ropts);

	bool ret(true);
	if(closure)
		for(size_t i(0); i < num && ret; ++i)
		{
			const column::delta delta(std::get<1>(op[i]), slice(val[i]));
			ret = closure(std::get<column>(op[i]), delta, status[i]);
		}

	// Update stats about whether the pinnable slices we obtained have internal
	// copies or referencing the cache copy.
	for(size_t i(0); i < num; ++i)
	{
		database &d(std::get<column>(op[i]));
		database::column &c(std::get<column>(op[i]));

		// Find the correct stats to update, one for the specific column and
		// one for the database total.
		ircd::stats::item<uint64_t *> *item_[2]
		{
			parallelize && buf[i].empty()?    &c.stats->multiget_referenced:
			parallelize?                      &c.stats->multiget_copied:
			buf[i].empty()?                   &c.stats->get_referenced:
			                                  &c.stats->get_copied,

			parallelize && buf[i].empty()?    &d.stats->multiget_referenced:
			parallelize?                      &d.stats->multiget_copied:
			buf[i].empty()?                   &d.stats->get_referenced:
			                                  &d.stats->get_copied,
		};

		for(auto *const &item : item_)
			++(*item);
	}

	return ret;
}

void
ircd::db::_seek(const vector_view<_read_op> &op,
                const vector_view<rocksdb::Status> &ret,
                const vector_view<rocksdb::PinnableSlice> &val,
                const rocksdb::ReadOptions &ropts)
{
	assert(ret.size() == op.size());
	assert(ret.size() == val.size());

	const ctx::stack_usage_assertion sua;
	const ctx::uninterruptible::nothrow ui;

	assert(op.size() >= 1);
	database &d(std::get<0>(op[0]));
	const size_t &num
	{
		op.size()
	};

	rocksdb::Slice key[num];
	std::transform(begin(op), end(op), key, []
	(const auto &op)
	{
		return slice(std::get<1>(op));
	});

	rocksdb::ColumnFamilyHandle *cf[num];
	std::transform(begin(op), end(op), cf, []
	(auto &op_)
	{
		auto &op(const_cast<_read_op &>(op_));
		database::column &c(std::get<column>(op));
		return static_cast<rocksdb::ColumnFamilyHandle *>(c);
	});

	#ifdef RB_DEBUG_DB_SEEK
	const ircd::timer timer;
	#endif

	#ifdef IRCD_DB_HAS_MULTIGET_BATCHED
	d.d->MultiGet(ropts, num, cf, key, val.data(), ret.data());
	#endif

	#ifdef RB_DEBUG_DB_SEEK
	log::debug
	{
		log, "[%s] %lu:%lu SEEK parallel:%zu ok:%zu nf:%zu inc:%zu in %ld$us",
		name(d),
		sequence(d),
		sequence(ropts.snapshot),
		ret.size(),
		std::count_if(begin(ret), end(ret), [](auto&& s) { return s.ok(); }),
		std::count_if(begin(ret), end(ret), [](auto&& s) { return s.IsNotFound(); }),
		std::count_if(begin(ret), end(ret), [](auto&& s) { return s.IsIncomplete(); }),
		timer.at<microseconds>().count(),
	};
	#endif
}

//
// iterator seek suite
//

namespace ircd::db
{
	static rocksdb::Iterator &_seek_(rocksdb::Iterator &, const pos &);
	static rocksdb::Iterator &_seek_(rocksdb::Iterator &, const string_view &);
	static rocksdb::Iterator &_seek_lower_(rocksdb::Iterator &, const string_view &);
	static rocksdb::Iterator &_seek_upper_(rocksdb::Iterator &, const string_view &);
	static bool _seek(database::column &, const pos &, const rocksdb::ReadOptions &, rocksdb::Iterator &it);
	static bool _seek(database::column &, const string_view &, const rocksdb::ReadOptions &, rocksdb::Iterator &it);
}

std::unique_ptr<rocksdb::Iterator>
ircd::db::seek(column &column,
               const string_view &key,
               const gopts &opts)
{
	database &d(column);
	database::column &c(column);

	std::unique_ptr<rocksdb::Iterator> ret;
	seek(c, key, make_opts(opts), ret);
	return ret;
}

template<class pos>
bool
ircd::db::seek(database::column &c,
               const pos &p,
               const rocksdb::ReadOptions &opts,
               std::unique_ptr<rocksdb::Iterator> &it)
{
	if(!it)
	{
		const ctx::uninterruptible::nothrow ui;

		database &d(*c.d);
		rocksdb::ColumnFamilyHandle *const &cf(c);
		it.reset(d.d->NewIterator(opts, cf));
	}

	return _seek(c, p, opts, *it);
}

bool
ircd::db::_seek(database::column &c,
                const string_view &p,
                const rocksdb::ReadOptions &opts,
                rocksdb::Iterator &it)
try
{
	const ctx::uninterruptible ui;

	#ifdef RB_DEBUG_DB_SEEK
	database &d(*c.d);
	const ircd::timer timer;
	#endif

	_seek_(it, p);

	#ifdef RB_DEBUG_DB_SEEK
	log::debug
	{
		log, "[%s] %lu:%lu SEEK %s %s in %ld$us '%s'",
		name(d),
		sequence(d),
		sequence(opts.snapshot),
		valid(it)? "VALID" : "INVALID",
		it.status().ToString(),
		timer.at<microseconds>().count(),
		name(c)
	};
	#endif

	return valid(it);
}
catch(const error &e)
{
	const database &d(*c.d);
	log::critical
	{
		log, "[%s][%s] %lu:%lu SEEK key :%s",
		name(d),
		name(c),
		sequence(d),
		sequence(opts.snapshot),
		e.what(),
	};

	throw;
}

bool
ircd::db::_seek(database::column &c,
                const pos &p,
                const rocksdb::ReadOptions &opts,
                rocksdb::Iterator &it)
try
{
	const ctx::stack_usage_assertion sua;

	#ifdef RB_DEBUG_DB_SEEK
	database &d(*c.d);
	const ircd::timer timer;
	const bool valid_it
	{
		valid(it)
	};
	#endif

	_seek_(it, p);

	#ifdef RB_DEBUG_DB_SEEK
	log::debug
	{
		log, "[%s] %lu:%lu SEEK[%s] %s -> %s in %ld$us '%s'",
		name(d),
		sequence(d),
		sequence(opts.snapshot),
		reflect(p),
		valid_it? "VALID" : "INVALID",
		it.status().ToString(),
		timer.at<microseconds>().count(),
		name(c)
	};
	#endif

	return valid(it);
}
catch(const error &e)
{
	const database &d(*c.d);
	log::critical
	{
		log, "[%s][%s] %lu:%lu SEEK %s %s :%s",
		name(d),
		name(c),
		sequence(d),
		sequence(opts.snapshot),
		reflect(p),
		it.Valid()? "VALID" : "INVALID",
		e.what(),
	};

	throw;
}

/// Seek to entry NOT GREATER THAN key. That is, equal to or less than key
rocksdb::Iterator &
ircd::db::_seek_lower_(rocksdb::Iterator &it,
                       const string_view &sv)
{
	it.SeekForPrev(slice(sv));
	return it;
}

/// Seek to entry NOT LESS THAN key. That is, equal to or greater than key
rocksdb::Iterator &
ircd::db::_seek_upper_(rocksdb::Iterator &it,
                       const string_view &sv)
{
	it.Seek(slice(sv));
	return it;
}

/// Defaults to _seek_upper_ because it has better support from RocksDB.
rocksdb::Iterator &
ircd::db::_seek_(rocksdb::Iterator &it,
                 const string_view &sv)
{
	return _seek_upper_(it, sv);
}

rocksdb::Iterator &
ircd::db::_seek_(rocksdb::Iterator &it,
                 const pos &p)
{
	switch(p)
	{
		case pos::NEXT:     it.Next();           break;
		case pos::PREV:     it.Prev();           break;
		case pos::FRONT:    it.SeekToFirst();    break;
		case pos::BACK:     it.SeekToLast();     break;
		default:
		case pos::END:
		{
			it.SeekToLast();
			if(it.Valid())
				it.Next();

			break;
		}
	}

	return it;
}

//
// validation suite
//

void
ircd::db::valid_eq_or_throw(const rocksdb::Iterator &it,
                            const string_view &sv)
{
	assert(!empty(sv));
	if(!valid_eq(it, sv))
	{
		throw_on_error(it.status());
		throw not_found{};
	}
}

void
ircd::db::valid_or_throw(const rocksdb::Iterator &it)
{
	if(!valid(it))
	{
		throw_on_error(it.status());
		throw not_found{};
		//assert(0); // status == ok + !Valid() == ???
	}
}

bool
ircd::db::valid_lte(const rocksdb::Iterator &it,
                    const string_view &sv)
{
	return valid(it, [&sv](const auto &it)
	{
		return it.key().compare(slice(sv)) <= 0;
	});
}

bool
ircd::db::valid_gt(const rocksdb::Iterator &it,
                   const string_view &sv)
{
	return valid(it, [&sv](const auto &it)
	{
		return it.key().compare(slice(sv)) > 0;
	});
}

bool
ircd::db::valid_eq(const rocksdb::Iterator &it,
                   const string_view &sv)
{
	return valid(it, [&sv](const auto &it)
	{
		return it.key().compare(slice(sv)) == 0;
	});
}

bool
ircd::db::valid(const rocksdb::Iterator &it,
                const valid_proffer &proffer)
{
	return valid(it)? proffer(it) : false;
}

bool
ircd::db::operator!(const rocksdb::Iterator &it)
{
	return !valid(it);
}

bool
ircd::db::valid(const rocksdb::Iterator &it)
{
	switch(it.status().code())
	{
		using rocksdb::Status;

		case Status::kOk:
		case Status::kNotFound:
		case Status::kIncomplete:
			return it.Valid();

		default:
			throw_on_error
			{
				it.status()
			};

			__builtin_unreachable();
	}
}

bool
ircd::db::valid(const rocksdb::Status &s)
{
	switch(s.code())
	{
		using rocksdb::Status;

		case Status::kOk:
			return true;

		case Status::kNotFound:
		case Status::kIncomplete:
			return false;

		default:
			throw_on_error{s};
			__builtin_unreachable();
	}
}

//
// column_names
//

std::vector<std::string>
ircd::db::column_names(const std::string &path,
                       const std::string &options)
{
	const rocksdb::DBOptions opts
	{
		db::options(options)
	};

	return column_names(path, opts);
}

/// Note that if there is no database found at path we still return a
/// vector containing the column name "default". This function is not
/// to be used as a test for whether the database exists. It returns
/// the columns required to be described at `path`. That will always
/// include the default column (RocksDB sez) even if database doesn't
/// exist yet.
std::vector<std::string>
ircd::db::column_names(const std::string &path,
                       const rocksdb::DBOptions &opts)
try
{
	std::vector<std::string> ret;

	throw_on_error
	{
		rocksdb::DB::ListColumnFamilies(opts, path, &ret)
	};

	return ret;
}
catch(const not_found &)
{
	return // No database found at path.
	{
		{ rocksdb::kDefaultColumnFamilyName }
	};
}

//
// Misc
//

rocksdb::CompressionType
ircd::db::find_supported_compression(const std::string &list)
{
	rocksdb::CompressionType ret
	{
		rocksdb::kNoCompression
	};

	tokens(list, ';', [&ret]
	(const string_view &requested)
	{
		if(ret != rocksdb::kNoCompression)
			return;

		for(const auto &[name, type] : db::compressions)
			if(type != 0L && name == requested)
			{
				ret = rocksdb::CompressionType(type);
				break;
			}
	});

	return ret;
}

rocksdb::DBOptions
ircd::db::make_dbopts(std::string optstr,
                      std::string *const &out,
                      bool *const read_only,
                      bool *const fsck)
{
	// RocksDB doesn't parse a read_only option, so we allow that to be added
	// to open the database as read_only and then remove that from the string.
	if(read_only)
		*read_only |= optstr_find_and_remove(optstr, "read_only=true;"s);
	else
		optstr_find_and_remove(optstr, "read_only=true;"s);

	// We also allow the user to specify fsck=true to run a repair operation on
	// the db. This may be expensive to do by default every startup.
	if(fsck)
		*fsck |= optstr_find_and_remove(optstr, "fsck=true;"s);
	else
		optstr_find_and_remove(optstr, "fsck=true;"s);

	// Generate RocksDB options from string
	rocksdb::DBOptions opts
	{
		db::options(optstr)
	};

	if(out)
		*out = std::move(optstr);

	return opts;
}

bool
ircd::db::optstr_find_and_remove(std::string &optstr,
                                 const std::string &what)
{
	const auto pos(optstr.find(what));
	if(pos == std::string::npos)
		return false;

	optstr.erase(pos, what.size());
	return true;
}

/// Convert our options structure into RocksDB's options structure.
rocksdb::ReadOptions
ircd::db::make_opts(const gopts &opts)
{
	rocksdb::ReadOptions ret;
	assert(ret.fill_cache);
	assert(ret.read_tier == BLOCKING);

	// slice* for exclusive upper bound. when prefixes are used this value must
	// have the same prefix because ordering is not guaranteed between prefixes
	ret.iterate_lower_bound = opts.lower_bound;
	ret.iterate_upper_bound = opts.upper_bound;

	ret += opts;
	return ret;
}

ircd::conf::item<bool>
read_checksum
{
	{ "name",     "ircd.db.read.checksum" },
	{ "default",  false                   }
};

/// Update a RocksDB options structure with our options structure. We use
/// operator+= for fun here; we can avoid reconstructing and returning a new
/// options structure in some cases by breaking out this function from
/// make_opts().
rocksdb::ReadOptions &
ircd::db::operator+=(rocksdb::ReadOptions &ret,
                     const gopts &opts)
{
	ret.pin_data = test(opts, get::PIN);
	ret.fill_cache |= test(opts, get::CACHE);
	ret.fill_cache &= !test(opts, get::NO_CACHE);
	ret.tailing = test(opts, get::NO_SNAPSHOT);
	ret.prefix_same_as_start = test(opts, get::PREFIX);
	ret.total_order_seek = test(opts, get::ORDERED);
	ret.verify_checksums = bool(read_checksum);
	ret.verify_checksums |= test(opts, get::CHECKSUM);
	ret.verify_checksums &= !test(opts, get::NO_CHECKSUM);

	ret.readahead_size = opts.readahead;
	ret.iter_start_seqnum = opts.seqnum;

	ret.read_tier = test(opts, get::NO_BLOCKING)?
		rocksdb::ReadTier::kBlockCacheTier:
		rocksdb::ReadTier::kReadAllTier;

	if(opts.snapshot && !test(opts, get::NO_SNAPSHOT))
		ret.snapshot = opts.snapshot;

	return ret;
}

rocksdb::WriteOptions
ircd::db::make_opts(const sopts &opts)
{
	rocksdb::WriteOptions ret;
	//ret.no_slowdown = true;    // read_tier = NON_BLOCKING for writes
	ret += opts;
	return ret;
}

rocksdb::WriteOptions &
ircd::db::operator+=(rocksdb::WriteOptions &ret,
                     const sopts &opts)
{
	ret.sync = test(opts, set::FSYNC);
	ret.disableWAL = test(opts, set::NO_JOURNAL);
	ret.ignore_missing_column_families = test(opts, set::NO_COLUMN_ERR);
	ret.no_slowdown = test(opts, set::NO_BLOCKING);
	ret.low_pri = test(opts, set::PRIO_LOW);
	return ret;
}

//
//
//

std::vector<std::string>
ircd::db::available()
{
	const string_view &prefix
	{
		fs::base::db
	};

	const auto dirs
	{
		fs::ls(prefix)
	};

	std::vector<std::string> ret;
	for(const auto &dir : dirs)
	{
		if(!fs::is_dir(dir))
			continue;

		const auto name
		{
			lstrip(dir, prefix)
		};

		const auto checkpoints
		{
			fs::ls(dir)
		};

		for(const auto &cpdir : checkpoints) try
		{
			const auto checkpoint
			{
				lstrip(lstrip(cpdir, dir), '/') //TODO: x-platform
			};

			auto path
			{
				db::path(name, lex_cast<uint64_t>(checkpoint))
			};

			ret.emplace_back(std::move(path));
		}
		catch(const bad_lex_cast &e)
		{
			continue;
		}
	}

	return ret;
}

std::string
ircd::db::path(const string_view &name)
{
	const auto pair
	{
		namepoint(name)
	};

	return path(pair.first, pair.second);
}

std::string
ircd::db::path(const string_view &name,
               const uint64_t &checkpoint)
{
	const auto &prefix
	{
		fs::base::db
	};

	const string_view parts[]
	{
		prefix, name, lex_cast(checkpoint)
	};

	return fs::path_string(parts);
}

std::pair<ircd::string_view, uint64_t>
ircd::db::namepoint(const string_view &name_)
{
	const auto s
	{
		split(name_, ':')
	};

	return
	{
		s.first,
		s.second? lex_cast<uint64_t>(s.second) : uint64_t(-1)
	};
}

std::string
ircd::db::namepoint(const string_view &name,
                    const uint64_t &checkpoint)
{
	return std::string{name} + ':' + std::string{lex_cast(checkpoint)};
}

//
// Iterator
//

std::pair<ircd::string_view, ircd::string_view>
ircd::db::operator*(const rocksdb::Iterator &it)
{
	return { key(it), val(it) };
}

ircd::string_view
ircd::db::key(const rocksdb::Iterator &it)
{
	return slice(it.key());
}

ircd::string_view
ircd::db::val(const rocksdb::Iterator &it)
{
	return slice(it.value());
}

//
// PinnableSlice
//

size_t
ircd::db::size(const rocksdb::PinnableSlice &ps)
{
	return size(static_cast<const rocksdb::Slice &>(ps));
}

const char *
ircd::db::data(const rocksdb::PinnableSlice &ps)
{
	return data(static_cast<const rocksdb::Slice &>(ps));
}

ircd::string_view
ircd::db::slice(const rocksdb::PinnableSlice &ps)
{
	return slice(static_cast<const rocksdb::Slice &>(ps));
}

//
// Slice
//

size_t
ircd::db::size(const rocksdb::Slice &slice)
{
	return slice.size();
}

const char *
ircd::db::data(const rocksdb::Slice &slice)
{
	return slice.data();
}

rocksdb::Slice
ircd::db::slice(const string_view &sv)
{
	return { sv.data(), sv.size() };
}

ircd::string_view
ircd::db::slice(const rocksdb::Slice &sk)
{
	return { sk.data(), sk.size() };
}

//
// reflect
//

const std::string &
ircd::db::reflect(const rocksdb::Tickers &type)
{
	const auto &names(rocksdb::TickersNameMap);
	const auto it(std::find_if(begin(names), end(names), [&type]
	(const auto &pair)
	{
		return pair.first == type;
	}));

	static const auto empty{"<ticker>?????"s};
	return it != end(names)? it->second : empty;
}

const std::string &
ircd::db::reflect(const rocksdb::Histograms &type)
{
	const auto &names(rocksdb::HistogramsNameMap);
	const auto it(std::find_if(begin(names), end(names), [&type]
	(const auto &pair)
	{
		return pair.first == type;
	}));

	static const auto empty{"<histogram>?????"s};
	return it != end(names)? it->second : empty;
}

ircd::string_view
ircd::db::reflect(const pos &pos)
{
	switch(pos)
	{
		case pos::NEXT:     return "NEXT";
		case pos::PREV:     return "PREV";
		case pos::FRONT:    return "FRONT";
		case pos::BACK:     return "BACK";
		case pos::END:      return "END";
	}

	return "?????";
}

ircd::string_view
ircd::db::reflect(const op &op)
{
	switch(op)
	{
		case op::GET:             return "GET";
		case op::SET:             return "SET";
		case op::MERGE:           return "MERGE";
		case op::DELETE_RANGE:    return "DELETE_RANGE";
		case op::DELETE:          return "DELETE";
		case op::SINGLE_DELETE:   return "SINGLE_DELETE";
	}

	return "?????";
}

ircd::string_view
ircd::db::reflect(const rocksdb::FlushReason &r)
{
	using FlushReason = rocksdb::FlushReason;

	switch(r)
	{
		case FlushReason::kOthers:                 return "Others";
		case FlushReason::kGetLiveFiles:           return "GetLiveFiles";
		case FlushReason::kShutDown:               return "ShutDown";
		case FlushReason::kExternalFileIngestion:  return "ExternalFileIngestion";
		case FlushReason::kManualCompaction:       return "ManualCompaction";
		case FlushReason::kWriteBufferManager:     return "WriteBufferManager";
		case FlushReason::kWriteBufferFull:        return "WriteBufferFull";
		case FlushReason::kTest:                   return "Test";
		case FlushReason::kDeleteFiles:            return "DeleteFiles";
		case FlushReason::kAutoCompaction:         return "AutoCompaction";
		case FlushReason::kManualFlush:            return "ManualFlush";
		case FlushReason::kErrorRecovery:          return "kErrorRecovery";
	}

	return "??????";
}

ircd::string_view
ircd::db::reflect(const rocksdb::CompactionReason &r)
{
	using CompactionReason = rocksdb::CompactionReason;

	switch(r)
	{
		case CompactionReason::kUnknown:                      return "Unknown";
		case CompactionReason::kLevelL0FilesNum:              return "LevelL0FilesNum";
		case CompactionReason::kLevelMaxLevelSize:            return "LevelMaxLevelSize";
		case CompactionReason::kUniversalSizeAmplification:   return "UniversalSizeAmplification";
		case CompactionReason::kUniversalSizeRatio:           return "UniversalSizeRatio";
		case CompactionReason::kUniversalSortedRunNum:        return "UniversalSortedRunNum";
		case CompactionReason::kFIFOMaxSize:                  return "FIFOMaxSize";
		case CompactionReason::kFIFOReduceNumFiles:           return "FIFOReduceNumFiles";
		case CompactionReason::kFIFOTtl:                      return "FIFOTtl";
		case CompactionReason::kManualCompaction:             return "ManualCompaction";
		case CompactionReason::kFilesMarkedForCompaction:     return "FilesMarkedForCompaction";
		case CompactionReason::kBottommostFiles:              return "BottommostFiles";
		case CompactionReason::kTtl:                          return "Ttl";
		case CompactionReason::kFlush:                        return "Flush";
		case CompactionReason::kExternalSstIngestion:         return "ExternalSstIngestion";

		#ifdef IRCD_DB_HAS_PERIODIC_COMPACTIONS
		case CompactionReason::kPeriodicCompaction:           return "kPeriodicCompaction";
		#endif

		case CompactionReason::kNumOfReasons:
			break;
	}

	return "??????";
}

ircd::string_view
ircd::db::reflect(const rocksdb::BackgroundErrorReason &r)
{
	using rocksdb::BackgroundErrorReason;

	switch(r)
	{
		case BackgroundErrorReason::kFlush:          return "FLUSH";
		case BackgroundErrorReason::kCompaction:     return "COMPACTION";
		case BackgroundErrorReason::kWriteCallback:  return "WRITE";
		case BackgroundErrorReason::kMemTable:       return "MEMTABLE";
		#if 0 // unreleased
		case BackgroundErrorReason::kManifestWrite:  return "MANIFESTWRITE";
		#endif
	}

	return "??????";
}

ircd::string_view
ircd::db::reflect(const rocksdb::WriteStallCondition &c)
{
	using rocksdb::WriteStallCondition;

	switch(c)
	{
		case WriteStallCondition::kNormal:   return "NORMAL";
		case WriteStallCondition::kDelayed:  return "DELAYED";
		case WriteStallCondition::kStopped:  return "STOPPED";
	}

	return "??????";
}

ircd::string_view
ircd::db::reflect(const rocksdb::Env::Priority &p)
{
	switch(p)
	{
		case rocksdb::Env::Priority::BOTTOM:  return "BOTTOM"_sv;
		case rocksdb::Env::Priority::LOW:     return "LOW"_sv;
		case rocksdb::Env::Priority::HIGH:    return "HIGH"_sv;
		#ifdef IRCD_DB_HAS_ENV_PRIO_USER
		case rocksdb::Env::Priority::USER:    return "USER"_sv;
		#endif
		case rocksdb::Env::Priority::TOTAL:   assert(0); break;
	}

	return "????"_sv;
}

ircd::string_view
ircd::db::reflect(const rocksdb::Env::IOPriority &p)
{
	switch(p)
	{
		case rocksdb::Env::IOPriority::IO_LOW:     return "IO_LOW"_sv;
		case rocksdb::Env::IOPriority::IO_HIGH:    return "IO_HIGH"_sv;
		case rocksdb::Env::IOPriority::IO_TOTAL:   assert(0); break;
	}

	return "IO_????"_sv;
}

ircd::string_view
ircd::db::reflect(const rocksdb::Env::WriteLifeTimeHint &h)
{
	using WriteLifeTimeHint = rocksdb::Env::WriteLifeTimeHint;

	switch(h)
	{
		case WriteLifeTimeHint::WLTH_NOT_SET:   return "NOT_SET";
		case WriteLifeTimeHint::WLTH_NONE:      return "NONE";
		case WriteLifeTimeHint::WLTH_SHORT:     return "SHORT";
		case WriteLifeTimeHint::WLTH_MEDIUM:    return "MEDIUM";
		case WriteLifeTimeHint::WLTH_LONG:      return "LONG";
		case WriteLifeTimeHint::WLTH_EXTREME:   return "EXTREME";
	}

	return "WLTH_????"_sv;
}

ircd::string_view
ircd::db::reflect(const rocksdb::Status::Severity &s)
{
	using Severity = rocksdb::Status::Severity;

	switch(s)
	{
		case Severity::kNoError:             return "NONE";
		case Severity::kSoftError:           return "SOFT";
		case Severity::kHardError:           return "HARD";
		case Severity::kFatalError:          return "FATAL";
		case Severity::kUnrecoverableError:  return "UNRECOVERABLE";
		case Severity::kMaxSeverity:         break;
	}

	return "?????";
}

ircd::string_view
ircd::db::reflect(const rocksdb::Status::Code &s)
{
	using Code = rocksdb::Status::Code;

	switch(s)
	{
		case Code::kOk:                    return "Ok";
		case Code::kNotFound:              return "NotFound";
		case Code::kCorruption:            return "Corruption";
		case Code::kNotSupported:          return "NotSupported";
		case Code::kInvalidArgument:       return "InvalidArgument";
		case Code::kIOError:               return "IOError";
		case Code::kMergeInProgress:       return "MergeInProgress";
		case Code::kIncomplete:            return "Incomplete";
		case Code::kShutdownInProgress:    return "ShutdownInProgress";
		case Code::kTimedOut:              return "TimedOut";
		case Code::kAborted:               return "Aborted";
		case Code::kBusy:                  return "Busy";
		case Code::kExpired:               return "Expired";
		case Code::kTryAgain:              return "TryAgain";
		case Code::kCompactionTooLarge:    return "CompactionTooLarge";

		#if ROCKSDB_MAJOR > 6 \
		|| (ROCKSDB_MAJOR == 6 && ROCKSDB_MINOR > 3) \
		|| (ROCKSDB_MAJOR == 6 && ROCKSDB_MINOR == 3 && ROCKSDB_PATCH >= 6)
		case Code::kColumnFamilyDropped:   return "ColumnFamilyDropped";
		case Code::kMaxCode:               break;
		#endif
	}

	return "?????";
}

ircd::string_view
ircd::db::reflect(const rocksdb::RandomAccessFile::AccessPattern &p)
{
	switch(p)
	{
		case rocksdb::RandomAccessFile::AccessPattern::NORMAL:      return "NORMAL"_sv;
		case rocksdb::RandomAccessFile::AccessPattern::RANDOM:      return "RANDOM"_sv;
		case rocksdb::RandomAccessFile::AccessPattern::SEQUENTIAL:  return "SEQUENTIAL"_sv;
		case rocksdb::RandomAccessFile::AccessPattern::WILLNEED:    return "WILLNEED"_sv;
		case rocksdb::RandomAccessFile::AccessPattern::DONTNEED:    return "DONTNEED"_sv;
	}

	return "??????"_sv;
}
