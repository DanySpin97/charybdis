/*
 * charybdis: an advanced Internet Relay Chat Daemon(ircd).
 *
 * Copyright (C) 2004-2005 Lee Hardy <lee@leeh.co.uk>
 * Copyright (C) 2005-2010 Jilles Tjoelker <jilles@stack.nl>
 * Copyright (C) 2004-2005 ircd-ratbox development team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
 *  USA
 */

#pragma once
#define HAVE_IRCD_TGCHANGE_H

#ifdef __cplusplus
namespace ircd {

/* finds a channel where source_p has op or voice and target_p is a member */
chan::chan *find_allowing_channel(client::client *source_p, client::client *target_p);
/* checks if source_p is allowed to send to target_p */
int add_target(client::client *source_p, client::client *target_p);
/* checks if source_p is allowed to send to chptr */
int add_channel_target(client::client *source_p, chan::chan *chptr);
/* allows source_p to send to target_p */
void add_reply_target(client::client *source_p, client::client *target_p);

}      // namespace ircd
#endif // __cplusplus
