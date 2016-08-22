/*
 *  ircd-ratbox: A slightly useful ircd.
 *  m_capab.c: Negotiates capabilities with a remote server.
 *
 *  Copyright (C) 1990 Jarkko Oikarinen and University of Oulu, Co Center
 *  Copyright (C) 1996-2002 Hybrid Development Team
 *  Copyright (C) 2002-2005 ircd-ratbox development team
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

using namespace ircd;

static const char capab_desc[] = "Provides the commands used for server-to-server capability negotiation";

static void mr_capab(struct MsgBuf *, client::client *, client::client *, int, const char **);
static void me_gcap(struct MsgBuf *, client::client *, client::client *, int, const char **);

struct Message capab_msgtab = {
	"CAPAB", 0, 0, 0, 0,
	{{mr_capab, 2}, mg_ignore, mg_ignore, mg_ignore, mg_ignore, mg_ignore}
};
struct Message gcap_msgtab = {
	"GCAP", 0, 0, 0, 0,
	{mg_ignore, mg_ignore, mg_ignore, mg_ignore, {me_gcap, 2}, mg_ignore}
};

mapi_clist_av1 capab_clist[] = { &capab_msgtab, &gcap_msgtab, NULL };

DECLARE_MODULE_AV2(capab, NULL, NULL, capab_clist, NULL, NULL, NULL, NULL, capab_desc);

/*
 * mr_capab - CAPAB message handler
 *      parv[1] = space-separated list of capabilities
 */
static void
mr_capab(struct MsgBuf *msgbuf_p, client::client *client_p, client::client *source_p, int parc, const char *parv[])
{
	int i;
	char *p;
	char *s;

	/* ummm, this shouldn't happen. Could argue this should be logged etc. */
	if(client_p->localClient == NULL)
		return;

	if(client_p->user)
		return;

	/* CAP_TS6 is set in PASS, so is valid.. */
	if((client_p->localClient->caps & ~CAP_TS6) != 0)
	{
		exit_client(client_p, client_p, client_p, "CAPAB received twice");
		return;
	}
	else
		client_p->localClient->caps |= CAP_CAP;

	rb_free(client_p->localClient->fullcaps);
	client_p->localClient->fullcaps = rb_strdup(parv[1]);

	for (i = 1; i < parc; i++)
	{
		char *t = LOCAL_COPY(parv[i]);
		for (s = rb_strtok_r(t, " ", &p); s; s = rb_strtok_r(NULL, " ", &p))
			client_p->localClient->caps |= serv_capindex.get(s, NULL);
	}
}

static void
me_gcap(struct MsgBuf *msgbuf_p, client::client *client_p, client::client *source_p,
		int parc, const char *parv[])
{
	char *t = LOCAL_COPY(parv[1]);
	char *s;
	char *p;

	if(!IsServer(source_p))
		return;

	/* already had GCAPAB?! */
	if (fullcaps(serv(*source_p)).size())
		caps(serv(*source_p)) = 0;

	fullcaps(serv(*source_p)) = parv[1];

	for (s = rb_strtok_r(t, " ", &p); s; s = rb_strtok_r(NULL, " ", &p))
		caps(serv(*source_p)) |= serv_capindex.get(s, NULL);
}
