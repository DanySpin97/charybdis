/*
 * Treat cmode +-S as +-b $~z.
 */

using namespace ircd;

static const char chm_sslonly_compat_desc[] =
	"Adds an emulated channel mode +S which is converted into mode +b $~z";

static int _modinit(void);
static void _moddeinit(void);
static void chm_sslonly(struct Client *source_p, struct Channel *chptr,
	int alevel, int parc, int *parn,
	const char **parv, int *errors, int dir, char c, long mode_type);

DECLARE_MODULE_AV2(chm_sslonly_compat, _modinit, _moddeinit, NULL, NULL, NULL, NULL, NULL, chm_sslonly_compat_desc);

static int
_modinit(void)
{
	chmode_table['S'].set_func = chm_sslonly;
	chmode_table['S'].mode_type = 0;
	chmode_table['S'].mode_class = ChmClass{CHM_D};

	return 0;
}

static void
_moddeinit(void)
{
	chmode_table['S'].set_func = chm_nosuch;
	chmode_table['S'].mode_type = 0;
	chmode_table['S'].mode_class = ChmClass(0);
}

static void
chm_sslonly(struct Client *source_p, struct Channel *chptr,
	int alevel, int parc, int *parn,
	const char **parv, int *errors, int dir, char c, long mode_type)
{
	int newparn = 0;
	const char *newparv[] = { "$~z" };

	if (MyClient(source_p))
		chm_ban(source_p, chptr, alevel, 1, &newparn, newparv,
				errors, dir, 'b', CHFL_BAN);
	else
		chm_nosuch(source_p, chptr, alevel, parc, parn, parv,
				errors, dir, c, mode_type);
}
