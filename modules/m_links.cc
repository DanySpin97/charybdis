/*
 *  ircd-ratbox: A slightly useful ircd.
 *  m_links.c: Shows what servers are currently connected.
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

static const char links_desc[] =
	"Provides the LINKS command to view servers linked to the host server";

static void m_links(struct MsgBuf *, client::client &, client::client &, int, const char **);
static void mo_links(struct MsgBuf *, client::client &, client::client &, int, const char **);
static char * clean_string(char *dest, const unsigned char *src, size_t len);

struct Message links_msgtab = {
	"LINKS", 0, 0, 0, 0,
	{mg_unreg, {m_links, 0}, {mo_links, 0}, mg_ignore, mg_ignore, {mo_links, 0}}
};

int doing_links_hook;

mapi_clist_av1 links_clist[] = { &links_msgtab, NULL };
mapi_hlist_av1 links_hlist[] = {
	{ "doing_links",	&doing_links_hook },
	{ NULL, NULL }
};

DECLARE_MODULE_AV2(links, NULL, NULL, links_clist, links_hlist, NULL, NULL, NULL, links_desc);

/*
 * m_links - LINKS message handler
 *      parv[1] = servername mask
 * or
 *      parv[1] = server to query
 *      parv[2] = servername mask
 */
static void
m_links(struct MsgBuf *msgbuf_p, client::client &client, client::client &source, int parc, const char *parv[])
{
	if(ConfigServerHide.flatten_links && !is_exempt_shide(source))
		scache_send_flattened_links(&source);
	else
		mo_links(msgbuf_p, client, source, parc, parv);
}

static void
mo_links(struct MsgBuf *msgbuf_p, client::client &client, client::client &source, int parc, const char *parv[])
{
	const char *mask = "";
	client::client *target_p;
	char clean_mask[2 * HOSTLEN + 4];
	hook_cdata hd;

	rb_dlink_node *ptr;

	if(parc > 2)
	{
		if(strlen(parv[2]) > HOSTLEN)
			return;
		if(hunt_server(&client, &source, ":%s LINKS %s :%s", 1, parc, parv)
		   != HUNTED_ISME)
			return;

		mask = parv[2];
	}
	else if(parc == 2)
		mask = parv[1];

	if(*mask)		/* only necessary if there is a mask */
		mask = collapse(clean_string
				(clean_mask, (const unsigned char *) mask, 2 * HOSTLEN));

	hd.client = &source;
	hd.arg1 = mask;
	hd.arg2 = NULL;

	call_hook(doing_links_hook, &hd);

	RB_DLINK_FOREACH(ptr, global_serv_list.head)
	{
		target_p = (client::client *)ptr->data;

		if(*mask && !match(mask, target_p->name))
			continue;

		/* We just send the reply, as if theyre here theres either no SHIDE,
		 * or theyre an oper..
		 */
		sendto_one_numeric(&source, RPL_LINKS, form_str(RPL_LINKS),
				   target_p->name, target_p->servptr->name,
				   target_p->hopcount,
				   target_p->info[0] ? target_p->info : "(Unknown Location)");
	}

	sendto_one_numeric(&source, RPL_ENDOFLINKS, form_str(RPL_ENDOFLINKS),
			   EmptyString(mask) ? "*" : mask);
}

static char *
clean_string(char *dest, const unsigned char *src, size_t len)
{
	char *d = dest;
	s_assert(0 != dest);
	s_assert(0 != src);

	if(dest == NULL || src == NULL)
		return NULL;

	while (*src && (len > 1))
	{
		if(*src & 0x80)	/* if high bit is set */
		{
			*d++ = '.';
			--len;
			if(len <= 1)
				break;
		}
		else if(!rfc1459::is_print(*src)) // if NOT printable
		{
			*d++ = '^';
			--len;
			if(len <= 1)
				break;
			*d++ = 0x40 + *src;	/* turn it into a printable */
		}
		else
			*d++ = *src;
		++src;
		--len;
	}
	*d = '\0';
	return dest;
}
