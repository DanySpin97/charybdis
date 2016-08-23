/*
 * m_sendbans.c: sends all permanent resvs and xlines to given server
 *
 * Copyright (C) 2008 Jilles Tjoelker
 * Copyright (C) 2008 charybdis development team
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

using namespace ircd;

static const char sendbands_desc[] =
	"Adds the ability to send all permanent RESVs and XLINEs to given server";

static void mo_sendbans(struct MsgBuf *msgbuf_p, client::client &client, client::client &source, int parc, const char *parv[]);

struct Message sendbans_msgtab = {
	"SENDBANS", 0, 0, 0, 0,
	{mg_unreg, mg_not_oper, mg_ignore, mg_ignore, mg_ignore, {mo_sendbans, 2}}
};

mapi_clist_av1 sendbans_clist[] = {
	&sendbans_msgtab,
	NULL
};

DECLARE_MODULE_AV2(sendbans, NULL, NULL, sendbans_clist, NULL, NULL, NULL, NULL, sendbands_desc);

static const char *expand_xline(const char *mask)
{
	static char buf[512];
	const char *p;
	char *q;

	if (!strchr(mask, ' '))
		return mask;
	if (strlen(mask) > 250)
		return NULL;
	p = mask;
	q = buf;
	while (*p != '\0')
	{
		if (*p == ' ')
			*q++ = '\\', *q++ = 's';
		else
			*q++ = *p;
		p++;
	}
	*q = '\0';
	return buf;
}

static void
mo_sendbans(struct MsgBuf *msgbuf_p, client::client &client, client::client &source, int parc, const char *parv[])
{
	struct ConfItem *aconf;
	rb_dlink_node *ptr;
	int count;
	const char *target, *mask2;
	client::client *server_p;
	struct rb_radixtree_iteration_state state;

	if (!IsOperRemoteBan(&source))
	{
		sendto_one(&source, form_str(ERR_NOPRIVS),
			me.name, source.name, "remoteban");
		return;
	}
	if (!IsOperXline(&source))
	{
		sendto_one(&source, form_str(ERR_NOPRIVS),
			me.name, source.name, "xline");
		return;
	}
	if (!IsOperResv(&source))
	{
		sendto_one(&source, form_str(ERR_NOPRIVS),
			me.name, source.name, "resv");
		return;
	}

	target = parv[1];
	count = 0;
	RB_DLINK_FOREACH(ptr, global_serv_list.head)
	{
		server_p = (client::client *)ptr->data;
		if (is_me(*server_p))
			continue;
		if (match(target, server_p->name))
			count++;
	}
	if (count == 0)
	{
		sendto_one_numeric(&source, ERR_NOSUCHSERVER,
				form_str(ERR_NOSUCHSERVER), target);
		return;
	}

	sendto_realops_snomask(SNO_GENERAL, L_NETWIDE,
			"%s!%s@%s is sending resvs and xlines to %s",
			source.name, source.username, source.host,
			target);

	RB_DLINK_FOREACH(ptr, resv_conf_list.head)
	{
		aconf = (ConfItem *)ptr->data;
		if (aconf->hold)
			continue;
		sendto_match_servs(&source, target,
				CAP_ENCAP, NOCAPS,
				"ENCAP %s RESV 0 %s 0 :%s",
				target, aconf->host, aconf->passwd);
	}

	void *elem;
	RB_RADIXTREE_FOREACH(elem, &state, resv_tree)
	{
		aconf = (ConfItem *)elem;
		if (aconf->hold)
			continue;
		sendto_match_servs(&source, target,
				CAP_ENCAP, NOCAPS,
				"ENCAP %s RESV 0 %s 0 :%s",
				target, aconf->host, aconf->passwd);
	}

	RB_DLINK_FOREACH(ptr, xline_conf_list.head)
	{
		aconf = (ConfItem *)ptr->data;
		if (aconf->hold)
			continue;
		mask2 = expand_xline(aconf->host);
		if (mask2 == NULL)
		{
			sendto_one_notice(&source, ":Skipping xline [%s]",
					aconf->host);
			continue;
		}
		sendto_match_servs(&source, target,
				CAP_ENCAP, NOCAPS,
				"ENCAP %s XLINE 0 %s 2 :%s",
				target, mask2, aconf->passwd);
	}
}
