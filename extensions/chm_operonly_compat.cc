/*
 * Treat cmode +-O as +-iI $o.
 */

using namespace ircd;

static const char chm_operonly_compat[] =
	"Adds an emulated channel mode +O which is converted into mode +i and +I $o";

static int _modinit(void);
static void _moddeinit(void);
static void chm_operonly(struct Client *source_p, struct Channel *chptr,
	int alevel, int parc, int *parn,
	const char **parv, int *errors, int dir, char c, long mode_type);

DECLARE_MODULE_AV2(chm_operonly_compat, _modinit, _moddeinit, NULL, NULL, NULL, NULL, NULL, chm_operonly_compat);

static int
_modinit(void)
{
	chmode_table['O'].set_func = chm_operonly;
	chmode_table['O'].mode_type = 0;
	chmode_table['O'].mode_class = ChmClass{CHM_D};

	return 0;
}

static void
_moddeinit(void)
{
	chmode_table['O'].set_func = chm_nosuch;
	chmode_table['O'].mode_type = 0;
	chmode_table['O'].mode_class = ChmClass(0);
}

static void
chm_operonly(struct Client *source_p, struct Channel *chptr,
	int alevel, int parc, int *parn,
	const char **parv, int *errors, int dir, char c, long mode_type)
{
	int newparn = 0;
	const char *newparv[] = { "$o" };

	if (MyClient(source_p)) {
		chm_simple(source_p, chptr, alevel, parc, parn, parv,
				errors, dir, 'i', MODE_INVITEONLY);
		chm_ban(source_p, chptr, alevel, 1, &newparn, newparv,
				errors, dir, 'I', CHFL_INVEX);
	} else
		chm_nosuch(source_p, chptr, alevel, parc, parn, parv,
				errors, dir, c, mode_type);
}
