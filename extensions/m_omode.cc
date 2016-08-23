/*
 *  Charybdis: an advanced Internet Relay Chat Daemon(ircd).
 *  m_omode.c: allows oper mode hacking
 *
 *  Copyright (C) 1990 Jarkko Oikarinen and University of Oulu, Co Center
 *  Copyright (C) 1996-2002 Hybrid Development Team
 *  Copyright (C) 2002-2004 ircd-ratbox development team
 *  Copyright (C) 2006 Charybdis development team
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

static const char omode_desc[] = "Allow admins to forcibly change modes on channels with the OMODE command";

static void mo_omode(struct MsgBuf *, client::client &, client::client &, int, const char **);

struct Message omode_msgtab = {
	"OMODE", 0, 0, 0, 0,
	{mg_unreg, mg_not_oper, mg_ignore, mg_ignore, mg_ignore, {mo_omode, 3}}
};

mapi_clist_av1 omode_clist[] = { &omode_msgtab, NULL };

DECLARE_MODULE_AV2(omode, NULL, NULL, omode_clist, NULL, NULL, NULL, NULL, omode_desc);

/*
 * mo_omode - MODE command handler
 * parv[1] - channel
 */
static void
mo_omode(struct MsgBuf *msgbuf_p, client::client &client, client::client &source, int parc, const char *parv[])
{
	chan::chan *chptr = NULL;
	chan::membership *msptr;
	char params[512];
	int i;
	int wasonchannel;

	/* admins only */
	if(!IsOperAdmin(&source))
	{
		sendto_one(&source, form_str(ERR_NOPRIVS), me.name, source.name, "admin");
		return;
	}

	/* Now, try to find the channel in question */
	if(!rfc1459::is_chan_prefix(parv[1][0]) || !chan::valid_name(parv[1]))
	{
		sendto_one_numeric(&source, ERR_BADCHANNAME,
				form_str(ERR_BADCHANNAME), parv[1]);
		return;
	}

	chptr = chan::get(parv[1], std::nothrow);

	if(chptr == NULL)
	{
		sendto_one_numeric(&source, ERR_NOSUCHCHANNEL,
				form_str(ERR_NOSUCHCHANNEL), parv[1]);
		return;
	}

	/* Now know the channel exists */
	msptr = get(chptr->members, source, std::nothrow);
	wasonchannel = msptr != NULL;

	if (is_chanop(msptr))
	{
		sendto_one_notice(&source, ":Use a normal MODE you idiot");
		return;
	}

	params[0] = '\0';
	for (i = 2; i < parc; i++)
	{
		if (i != 2)
			rb_strlcat(params, " ", sizeof params);
		rb_strlcat(params, parv[i], sizeof params);
	}

	sendto_wallops_flags(umode::WALLOP, &me,
			     "OMODE called for [%s] [%s] by %s!%s@%s",
			     parv[1], params, source.name, source.username, source.host);
	ilog(L_MAIN, "OMODE called for [%s] [%s] by %s",
	     parv[1], params, get_oper_name(&source));

	if(chptr->name[0] != '&')
		sendto_server(NULL, NULL, NOCAPS, NOCAPS,
			      ":%s WALLOPS :OMODE called for [%s] [%s] by %s!%s@%s",
			      me.name, parv[1], params, source.name, source.username,
			      source.host);

#if 0
	set_channel_mode(&client, source.servptr, chptr, msptr,
			 parc - 2, parv + 2);
#else
	if (parc == 4 && !strcmp(parv[2], "+o") && !irccmp(parv[3], source.name))
	{
		/* Opping themselves */
		if (!wasonchannel)
		{
			sendto_one_numeric(&source, ERR_USERNOTINCHANNEL,
					   form_str(ERR_USERNOTINCHANNEL), parv[3], chptr->name.c_str());
			return;
		}
		sendto_channel_local(chan::ALL_MEMBERS, chptr, ":%s MODE %s +o %s",
				me.name, parv[1], source.name);
		sendto_server(NULL, chptr, CAP_TS6, NOCAPS,
				":%s TMODE %ld %s +o %s",
				me.id, (long) chptr->channelts, parv[1],
				source.id);
		msptr->flags |= chan::CHANOP;
	}
	else
	{
		/* Hack it so set_channel_mode() will accept */
		if (wasonchannel)
			msptr->flags |= chan::CHANOP;
		else
		{
			add(*chptr, source, chan::CHANOP);
			msptr = get(chptr->members, source, std::nothrow);
		}
		set_channel_mode(&client, &source, chptr, msptr,
				parc - 2, parv + 2);
		/* We know they were not opped before and they can't have opped
		 * themselves as set_channel_mode() does not allow that
		 * -- jilles */
		if (wasonchannel)
			msptr->flags &= ~chan::CHANOP;
		else
			del(*chptr, get_client(*msptr));
	}
#endif
}
