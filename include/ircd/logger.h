/*
 * ircd-ratbox: an advanced Internet Relay Chat Daemon(ircd).
 *
 * Copyright (C) 2003 Lee Hardy <lee@leeh.co.uk>
 * Copyright (C) 2003-2004 ircd-ratbox development team
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1.Redistributions of source code must retain the above copyright notice,
 *   this list of conditions and the following disclaimer.
 * 2.Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the distribution.
 * 3.The name of the author may not be used to endorse or promote products
 *   derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
 * IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

#pragma once
#define HAVE_IRCD_LOGGER_H

#ifdef __cplusplus
namespace ircd {

typedef enum ilogfile
{
	L_MAIN,
	L_USER,
	L_FUSER,
	L_OPERED,
	L_FOPER,
	L_SERVER,
	L_KILL,
	L_KLINE,
	L_OPERSPY,
	L_IOERROR,
	LAST_LOGFILE
} ilogfile;

struct Client;

extern void init_main_logfile(void);
extern void open_logfiles(void);
extern void close_logfiles(void);
void ilog(ilogfile dest, const char *fmt, ...) AFP(2, 3);
extern void idebug(const char *fmt, ...) AFP(1, 2);
extern void inotice(const char *fmt, ...) AFP(1, 2);
extern void iwarn(const char *fmt, ...) AFP(1, 2);
extern void ierror(const char *fmt, ...) AFP(1, 2);
extern void report_operspy(struct Client *, const char *, const char *);
extern const char *smalldate(time_t);
extern void ilog_error(const char *);

void vslog(ilogfile dest, unsigned int snomask, const char *fmt, va_list ap);
void slog(ilogfile dest, unsigned int snomask, const char *fmt, ...) AFP(3, 4);

}      // namespace ircd
#endif // __cplusplus
