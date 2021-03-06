// Matrix Construct
//
// Copyright (C) Matrix Construct Developers, Authors & Contributors
// Copyright (C) 2016-2018 Jason Volk <jason@zemos.net>
//
// Permission to use, copy, modify, and/or distribute this software for any
// purpose with or without fee is hereby granted, provided that the above
// copyright notice and this permission notice is present in all copies. The
// full license for this software is available in the LICENSE file.

#include <RB_INC_SYS_STAT_H
#include <boost/filesystem.hpp>
#include <ircd/asio.h>

#ifdef IRCD_USE_AIO
	#include "aio.h"
#endif

namespace filesystem = boost::filesystem;

namespace ircd::fs
{
	filesystem::path path(std::string);
	filesystem::path path(const string_view &);
	filesystem::path path(const vector_view<const string_view> &);
}

/// Non-null when aio is available for use
decltype(ircd::fs::aioctx)
ircd::fs::aioctx
{};

ircd::fs::init::init()
{
	#ifdef IRCD_USE_AIO
		assert(!aioctx);
		aioctx = new aio{};
	#else
		log::warning
		{
			"No support for asynchronous local filesystem IO..."
		};
	#endif
}

ircd::fs::init::~init()
noexcept
{
	#ifdef IRCD_USE_AIO
		delete aioctx;
		aioctx = nullptr;
	#endif

	assert(!aioctx);
}

//
// Compile-time path index
//

namespace ircd::fs
{
	struct sysent;
	extern const std::array<struct sysent, num_of<index>()> syspaths;
}

struct ircd::fs::sysent
{
	string_view name;
	string_view path;
};

decltype(ircd::fs::syspaths)
ircd::fs::syspaths
{{
	{ "installation prefix",      RB_PREFIX      },
	{ "binary directory",         RB_BIN_DIR     },
	{ "configuration directory",  RB_CONF_DIR    },
	{ "data directory",           RB_DATA_DIR    },
	{ "database directory",       RB_DB_DIR      },
	{ "log directory",            RB_LOG_DIR     },
	{ "module directory",         RB_MODULE_DIR  },
}};

ircd::string_view
ircd::fs::get(index index)
noexcept try
{
	return syspaths.at(index).path;
}
catch(const std::out_of_range &e)
{
	return {};
}

ircd::string_view
ircd::fs::name(index index)
noexcept try
{
	return syspaths.at(index).name;
}
catch(const std::out_of_range &e)
{
	return {};
}

///////////////////////////////////////////////////////////////////////////////
//
// fs/stdin.h
//

ircd::string_view
ircd::fs::stdin::readline(const mutable_buffer &buf)
{
	boost::asio::posix::stream_descriptor fd
	{
		ios::get(), dup(STDIN_FILENO)
	};

	boost::asio::streambuf sb
	{
		size(buf)
	};

	const auto interruption{[&fd]
	(ctx::ctx *const &interruptor) noexcept
	{
		fd.cancel();
	}};

	const size_t len
	{
		boost::asio::async_read_until(fd, sb, '\n', yield_context{to_asio{interruption}})
	};

	std::istream is{&sb};
	is.get(data(buf), size(buf), '\n');
	return string_view
	{
		data(buf), size_t(is.gcount())
	};
}

//
// tty
//

ircd::fs::stdin::tty::tty()
:fd{[]
{
	thread_local char buf[256];
	syscall(::ttyname_r, STDIN_FILENO, buf, sizeof(buf));
	return fd
	{
		string_view{buf}, std::ios_base::out
	};
}()}
{
}

size_t
ircd::fs::stdin::tty::write(const string_view &buf)
{
	return syscall(::write, int(*this), buf.data(), buf.size());
}

///////////////////////////////////////////////////////////////////////////////
//
// fs/fsync.h
//

ircd::fs::fsync_opts
const ircd::fs::fsync_opts_default
{};

void
ircd::fs::fsync(const fd &fd,
                const fsync_opts &opts)
try
{
	//TODO: AIO fsync is throwing -EINVAL
/*
	#ifdef IRCD_USE_AIO
	if(likely(aioctx))
		return fsync__aio(fd, opts);
	#endif
*/
	syscall(::fsync, fd);
}
catch(const error &e)
{
	throw;
}
catch(const filesystem::filesystem_error &e)
{
	throw error{e};
}
catch(const std::exception &e)
{
	throw error
	{
		"%s", e.what()
	};
}

void
ircd::fs::fdsync(const fd &fd,
                 const fsync_opts &opts)
try
{
	//TODO: AIO fdsync is throwing -EINVAL
/*
	#ifdef IRCD_USE_AIO
	if(likely(aioctx))
		return fdsync__aio(fd, opts);
	#endif
*/
	syscall(::fdatasync, fd);
}
catch(const error &e)
{
	throw;
}
catch(const filesystem::filesystem_error &e)
{
	throw error{e};
}
catch(const std::exception &e)
{
	throw error
	{
		"%s", e.what()
	};
}

///////////////////////////////////////////////////////////////////////////////
//
// fs/prefetch.h
//

///////////////////////////////////////////////////////////////////////////////
//
// fs/read.h
//

ircd::fs::read_opts
const ircd::fs::read_opts_default
{};

#ifdef __linux__
void
ircd::fs::prefetch(const fd &fd,
                   const size_t &count,
                   const read_opts &opts)
{
	const auto flags
	{
		syscall(::fcntl, fd, F_GETFL)
	};

	if(~flags & O_DIRECT)
	{
		syscall(::readahead, fd, opts.offset, count);
		return;
	}

	#ifdef IRCD_USE_AIO
	if(likely(aioctx))
		prefetch__aio(fd, count, opts);
	#endif
}
#else
void
ircd::fs::prefetch(const fd &fd,
                   const size_t &count,
                   const read_opts &opts)
{
}
#endif

std::string
ircd::fs::read(const string_view &path,
               const read_opts &opts)
try
{
	const fd fd
	{
		path
	};

	return read(fd, opts);
}
catch(const error &e)
{
	throw;
}
catch(const filesystem::filesystem_error &e)
{
	throw error{e};
}
catch(const std::exception &e)
{
	throw error
	{
		"%s", e.what()
	};
}

std::string
ircd::fs::read(const fd &fd,
               const read_opts &opts)
{
	return string(size(fd), [&fd, &opts]
	(const mutable_buffer &buf)
	{
		return read(fd, buf, opts);
	});
}

ircd::const_buffer
ircd::fs::read(const string_view &path,
               const mutable_buffer &buf,
               const read_opts &opts)
try
{
	const fd fd
	{
		path
	};

	return read(fd, buf, opts);
}
catch(const error &e)
{
	throw;
}
catch(const filesystem::filesystem_error &e)
{
	throw error{e};
}
catch(const std::exception &e)
{
	throw error
	{
		"%s", e.what()
	};
}

ircd::const_buffer
ircd::fs::read(const fd &fd,
               const mutable_buffer &buf,
               const read_opts &opts)
try
{
	#ifdef IRCD_USE_AIO
	if(likely(aioctx))
		return read__aio(fd, buf, opts);
	#endif

	return
	{
		data(buf),
		size_t(syscall(::pread, fd, data(buf), size(buf), opts.offset))
	};
}
catch(const error &e)
{
	throw;
}
catch(const filesystem::filesystem_error &e)
{
	throw error{e};
}
catch(const std::exception &e)
{
	throw error
	{
		"%s", e.what()
	};
}

///////////////////////////////////////////////////////////////////////////////
//
// fs/write.h
//

ircd::fs::write_opts
const ircd::fs::write_opts_default
{};

void
ircd::fs::allocate(const fd &fd,
                   const size_t &size,
                   const write_opts &opts)
{
	int mode{0};
	mode |= opts.keep_size? FALLOC_FL_KEEP_SIZE : 0;
	syscall(::fallocate, fd, mode, opts.offset, size);
}

void
ircd::fs::truncate(const string_view &path,
                   const size_t &size,
                   const write_opts &opts)
{
	const fd fd
	{
		path, std::ios::out | std::ios::trunc
	};

	return truncate(fd, size, opts);
}

void
ircd::fs::truncate(const fd &fd,
                   const size_t &size,
                   const write_opts &opts)
{
	syscall(::ftruncate, fd, size);
}

ircd::const_buffer
ircd::fs::overwrite(const string_view &path,
                    const const_buffer &buf,
                    const write_opts &opts)
{
	const fd fd
	{
		path, std::ios::out | std::ios::trunc
	};

	return overwrite(fd, buf, opts);
}

ircd::const_buffer
ircd::fs::overwrite(const fd &fd,
                    const const_buffer &buf,
                    const write_opts &opts)
{
	return write(fd, buf, opts);
}

ircd::const_buffer
ircd::fs::append(const string_view &path,
                 const const_buffer &buf,
                 const write_opts &opts)
{
	const fd fd
	{
		path, std::ios::out | std::ios::app
	};

	return write(fd, buf, opts);
}

ircd::const_buffer
ircd::fs::append(const fd &fd,
                 const const_buffer &buf,
                 const write_opts &opts_)
try
{
	auto opts(opts_);
	if(!opts.offset)
		opts.offset = syscall(::lseek, fd, 0, SEEK_END);

	return write(fd, buf, opts);
}
catch(const error &e)
{
	throw;
}
catch(const filesystem::filesystem_error &e)
{
	throw error{e};
}
catch(const std::exception &e)
{
	throw error
	{
		"%s", e.what()
	};
}

ircd::const_buffer
ircd::fs::write(const string_view &path,
                const const_buffer &buf,
                const write_opts &opts)
try
{
	const fd fd
	{
		path, std::ios::out
	};

	return write(fd, buf, opts);
}
catch(const error &e)
{
	throw;
}
catch(const filesystem::filesystem_error &e)
{
	throw error{e};
}
catch(const std::exception &e)
{
	throw error
	{
		"%s", e.what()
	};
}

ircd::const_buffer
ircd::fs::write(const fd &fd,
                const const_buffer &buf,
                const write_opts &opts)
try
{
	#ifdef IRCD_USE_AIO
	if(likely(aioctx))
		return write__aio(fd, buf, opts);
	#endif

	return
	{
		data(buf),
		size_t(syscall(::pwrite, fd, data(buf), size(buf), opts.offset))
	};
}
catch(const error &e)
{
	throw;
}
catch(const filesystem::filesystem_error &e)
{
	throw error{e};
}
catch(const std::exception &e)
{
	throw error
	{
		"%s", e.what()
	};
}

///////////////////////////////////////////////////////////////////////////////
//
// fs/fd.h
//
// TODO: x-platform
//

namespace ircd::fs
{
	thread_local char path_buf[PATH_MAX];
	static const char *path_str(const string_view &);
	static uint posix_flags(const std::ios::openmode &mode);
}

#ifdef __linux__
size_t
ircd::fs::block_size(const fd &fd)
{
	return 512UL;
}
#elif defined(HAVE_SYS_STAT_H)
size_t
ircd::fs::block_size(const fd &fd)
{
	struct stat st;
	syscall(::fstat, fd, &st);
	return st.st_blksize;
}
#else
size_t
ircd::fs::block_size(const fd &fd)
{
	return info::page_size;
}
#endif

/// This is not a standard UUID of any sort; this is custom, and intended for
/// rocksdb (though rocksdb has no requirement for its format specifically).
/// The format is plain-text, fs major and minor number, inode number, and
/// a three letter file type; all obtained from fstat(2).
ircd::string_view
ircd::fs::uuid(const fd &fd,
               const mutable_buffer &buf)
{
	struct stat stat;
	syscall(::fstat, fd, &stat);
	return fmt::sprintf
	{
		buf, "%u-%u-%lu-%s",
		gnu_dev_major(stat.st_dev),
		gnu_dev_minor(stat.st_dev),
		stat.st_ino,
		S_ISREG(stat.st_mode)? "reg":
		S_ISDIR(stat.st_mode)? "dir":
		                       "???"
	};
}

size_t
ircd::fs::size(const fd &fd)
{
	const off_t cur
	{
		syscall(::lseek, fd, 0, SEEK_CUR)
	};

	const off_t end
	{
		syscall(::lseek, fd, 0, SEEK_END)
	};

	syscall(::lseek, fd, cur, SEEK_SET);
	return end;
}

uint
ircd::fs::posix_flags(const std::ios::openmode &mode)
{
	static const auto rdwr
	{
		std::ios::in | std::ios::out
	};

	uint ret{0};
	if((mode & rdwr) == rdwr)
		ret |= O_RDWR;
	else if(mode & std::ios::out)
		ret |= O_WRONLY;
	else
		ret |= O_RDONLY;

	ret |= mode & std::ios::trunc? O_TRUNC : 0;
	ret |= mode & std::ios::app? O_APPEND : 0;
	ret |= ret & O_WRONLY? O_CREAT : 0;
	ret |= ret & O_RDWR && ret & (O_TRUNC | O_APPEND)? O_CREAT : 0;
	return ret;
}

const char *
ircd::fs::path_str(const string_view &s)
{
	return data(strlcpy(path_buf, s));
}

//
// fd::opts
//

ircd::fs::fd::opts::opts(const std::ios::openmode &mode)
:mode
{
	mode
}
,flags
{
	posix_flags(mode)
}
,mask
{
	flags & O_CREAT?
		S_IRUSR | S_IWUSR:
		0U
}
,ate
{
	bool(mode & std::ios::ate)
}
{
}

//
// fd::fd
//

ircd::fs::fd::fd(const string_view &path)
:fd{path, opts{}}
{
}

ircd::fs::fd::fd(const string_view &path,
                 const opts &opts)
:fdno{[&path, &opts]
() -> int
{
	uint flags(opts.flags);
	flags |= opts.direct? O_DIRECT : 0UL;
	flags |= opts.cloexec? O_CLOEXEC : 0UL;
	flags &= opts.nocreate? ~O_CREAT : flags;

	const mode_t &mode(opts.mask);
	assert((flags & ~O_CREAT) || mode != 0);

	const char *const &p(path_str(path));
	return syscall(::open, p, flags, mode);
}()}
{
	if(opts.ate)
		syscall(::lseek, fdno, 0, SEEK_END);
}

ircd::fs::fd::fd(fd &&o)
noexcept
:fdno
{
	std::move(o.fdno)
}
{
	o.fdno = -1;
}

ircd::fs::fd &
ircd::fs::fd::operator=(fd &&o)
noexcept
{
	this->~fd();
	fdno = std::move(o.fdno);
	o.fdno = -1;
	return *this;
}

ircd::fs::fd::~fd()
noexcept(false)
{
	if(fdno < 0)
		return;

	syscall(::close, fdno);
}

///////////////////////////////////////////////////////////////////////////////
//
// fs.h / misc
//

std::string
ircd::fs::cwd()
try
{
	return filesystem::current_path().string();
}
catch(const filesystem::filesystem_error &e)
{
	throw error{e};
}

void
ircd::fs::chdir(const string_view &path)
try
{
	filesystem::current_path(fs::path(path));
}
catch(const filesystem::filesystem_error &e)
{
	throw error
	{
		e, "`%s' :%s", path, e.what()
	};
}

bool
ircd::fs::mkdir(const string_view &path)
try
{
	return filesystem::create_directories(fs::path(path));
}
catch(const filesystem::filesystem_error &e)
{
	throw error
	{
		e, "`%s' :%s", path, e.what()
	};
}

bool
ircd::fs::remove(const string_view &path)
try
{
	return filesystem::remove(fs::path(path));
}
catch(const filesystem::filesystem_error &e)
{
	throw error
	{
		e, "`%s' :%s", path, e.what()
	};
}

bool
ircd::fs::remove(std::nothrow_t,
                 const string_view &path)
{
	boost::system::error_code ec;
	return filesystem::remove(fs::path(path), ec);
}

void
ircd::fs::rename(const string_view &old,
                 const string_view &new_)
try
{
	filesystem::rename(path(old), path(new_));
}
catch(const filesystem::filesystem_error &e)
{
	throw error
	{
		e, "`%s' -> `%s' :%s", old, new_, e.what()
	};
}

bool
ircd::fs::rename(std::nothrow_t,
                 const string_view &old,
                 const string_view &new_)
{
	boost::system::error_code ec;
	filesystem::rename(path(old), path(new_), ec);
	return !ec;
}

std::vector<std::string>
ircd::fs::ls_recursive(const string_view &path)
try
{
	const filesystem::recursive_directory_iterator end;
	filesystem::recursive_directory_iterator it
	{
		fs::path(path)
	};

	std::vector<std::string> ret;
	std::transform(it, end, std::back_inserter(ret), []
	(const auto &ent)
	{
		return ent.path().string();
	});

	return ret;
}
catch(const filesystem::filesystem_error &e)
{
	throw error{e};
}

std::vector<std::string>
ircd::fs::ls(const string_view &path)
try
{
	static const filesystem::directory_iterator end;
	filesystem::directory_iterator it
	{
		fs::path(path)
	};

	std::vector<std::string> ret;
	std::transform(it, end, std::back_inserter(ret), []
	(const auto &ent)
	{
		return ent.path().string();
	});

	return ret;
}
catch(const filesystem::filesystem_error &e)
{
	throw error{e};
}

bool
ircd::fs::direct_io_support(const string_view &path)
try
{
	fd::opts opts{std::ios::out};
	opts.direct = true;
	fd{path, opts};
	return true;
}
catch(const std::system_error &e)
{
	const auto &code(e.code());
	if(code.category() == std::system_category()) switch(code.value())
	{
		case int(std::errc::invalid_argument):
			return false;

		default:
			break;
	}

	throw;
}

size_t
ircd::fs::size(const string_view &path)
{
	return filesystem::file_size(fs::path(path));
}

bool
ircd::fs::is_reg(const string_view &path)
try
{
	return filesystem::is_regular_file(fs::path(path));
}
catch(const filesystem::filesystem_error &e)
{
	throw error{e};
}

bool
ircd::fs::is_dir(const string_view &path)
try
{
	return filesystem::is_directory(fs::path(path));
}
catch(const filesystem::filesystem_error &e)
{
	throw error{e};
}

bool
ircd::fs::exists(const string_view &path)
try
{
	return filesystem::exists(fs::path(path));
}
catch(const filesystem::filesystem_error &e)
{
	throw error{e};
}

std::string
ircd::fs::make_path(const vector_view<const std::string> &list)
{
	filesystem::path ret;
	for(const auto &s : list)
		ret /= path(s);

	return ret.string();
}

std::string
ircd::fs::make_path(const vector_view<const string_view> &list)
{
	filesystem::path ret;
	for(const auto &s : list)
		ret /= path(s);

	return ret.string();
}

filesystem::path
ircd::fs::path(const vector_view<const string_view> &list)
{
	filesystem::path ret;
	for(const auto &s : list)
		ret /= path(s);

	return ret.string();
}

filesystem::path
ircd::fs::path(const string_view &s)
{
	return path(std::string{s});
}

filesystem::path
ircd::fs::path(std::string s)
{
	return filesystem::path(std::move(s));
}

//
// fs/error.h
//

std::error_code
ircd::make_error_code(const boost::filesystem::filesystem_error &e)
{
	const boost::system::error_code &ec
	{
		e.code()
	};

	return make_error_code(ec);
}
