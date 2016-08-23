/*
 *  ircd-ratbox: A slightly useful ircd.
 *  m_away.c: Sets/removes away status on a user.
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

static const char away_desc[] = "Provides the AWAY command to set yourself away";

static void m_away(struct MsgBuf *, client::client &, client::client &, int, const char **);

struct Message away_msgtab = {
	"AWAY", 0, 0, 0, 0,
	{mg_unreg, {m_away, 0}, {m_away, 0}, mg_ignore, mg_ignore, {m_away, 0}}
};

mapi_clist_av1 away_clist[] = { &away_msgtab, NULL };

DECLARE_MODULE_AV2(away, NULL, NULL, away_clist, NULL, NULL, NULL, NULL, away_desc);

/***********************************************************************
 * m_away() - Added 14 Dec 1988 by jto.
 *            Not currently really working, I don't like this
 *            call at all...
 *
 *            ...trying to make it work. I don't like it either,
 *            but perhaps it's worth the load it causes to net.
 *            This requires flooding of the whole net like NICK,
 *            USER, MODE, etc messages...  --msa
 *
 *            The above comments have long since irrelvant, but
 *            are kept for historical purposes now ;)
 ***********************************************************************/

/*
** m_away
**      parv[1] = away message
*/
static void
m_away(struct MsgBuf *msgbuf_p, client::client &client, client::client &source, int parc, const char *parv[])
{
	if(my(source) && source.localClient->next_away &&
			!is_flood_done(source))
		flood_endgrace(&source);

	if(!is_client(source))
		return;

	if(parc < 2 || EmptyString(parv[1]))
	{
		/* Marking as not away */
		if(away(user(source)).size())
		{
			/* we now send this only if they were away before --is */
			sendto_server(&client, NULL, CAP_TS6, NOCAPS,
				      ":%s AWAY", use_id(&source));

			sendto_common_channels_local_butone(&source, CLICAP_AWAY_NOTIFY, NOCAPS, ":%s!%s@%s AWAY",
							    source.name, source.username, source.host);
		}
		if(my_connect(source))
			sendto_one_numeric(&source, RPL_UNAWAY, form_str(RPL_UNAWAY));
		return;
	}

	/* Rate limit this because it is sent to common channels. */
	if (my(source))
	{
		if(!is(source, umode::OPER) &&
				source.localClient->next_away > rb_current_time())
		{
			sendto_one(&source, form_str(RPL_LOAD2HI),
					me.name, source.name, "AWAY");
			return;
		}
		if(source.localClient->next_away < rb_current_time() -
				ConfigFileEntry.away_interval)
			source.localClient->next_away = rb_current_time();
		else
			source.localClient->next_away = rb_current_time() +
				ConfigFileEntry.away_interval;
	}

	std::string p1(parv[1]);
	if (p1.size() >= AWAYLEN)
		p1.resize(AWAYLEN-1);

	if (away(user(source)) != p1)
	{
		away(user(source)) = std::move(p1);
		sendto_server(&client, NULL, CAP_TS6, NOCAPS,
		              ":%s AWAY :%s",
		              use_id(&source),
		              away(user(source)).c_str());

		sendto_common_channels_local_butone(&source,
					            CLICAP_AWAY_NOTIFY, NOCAPS,
						    ":%s!%s@%s AWAY :%s",
						    source.name,
						    source.username,
						    source.host,
						    away(user(source)).c_str());
	}

	if(my_connect(source))
		sendto_one_numeric(&source, RPL_NOWAWAY, form_str(RPL_NOWAWAY));
}
