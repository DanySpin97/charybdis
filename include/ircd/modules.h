/*
 *  ircd-ratbox: A slightly useful ircd.
 *  modules.h: A header for the modules functions.
 *
 *  Copyright (C) 1990 Jarkko Oikarinen and University of Oulu, Co Center
 *  Copyright (C) 1996-2002 Hybrid Development Team
 *  Copyright (C) 2002-2004 ircd-ratbox development team
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
#define HAVE_IRCD_MODULES_H

#define MAPI_CHARYBDIS 2
typedef struct lt__handle *lt_dlhandle;

#ifdef __cplusplus
namespace ircd {

struct module
{
	const char *name;
	const char *path;
	const char *version;
	const char *description;
	lt_dlhandle address;
	int core;	/* This is int for backwards compat reasons */
	int origin;	/* Ditto */
	int mapi_version;
	void *mapi_header; /* actually struct mapi_mheader_av<mapi_version> */
	rb_dlink_node node;
};

#define MAPI_MAGIC_HDR	0x4D410000

#define MAPI_V1		(MAPI_MAGIC_HDR | 0x1)
#define MAPI_V2		(MAPI_MAGIC_HDR | 0x2)
#define MAPI_V3		(MAPI_MAGIC_HDR | 0x3)

#define MAPI_MAGIC(x)	((x) & 0xffff0000)
#define MAPI_VERSION(x)	((x) & 0x0000ffff)

typedef struct Message* mapi_clist_av1;

typedef struct
{
	const char *hapi_name;
	int *hapi_id;
} mapi_hlist_av1;

typedef struct
{
	const char *hapi_name;
	hookfn fn;
} mapi_hfn_list_av1;


#define MAPI_CAP_CLIENT		1
#define MAPI_CAP_SERVER		2

typedef struct
{
	int cap_index;		/* Which cap index does this belong to? */
	const char *cap_name;	/* Capability name */
	void *cap_ownerdata;	/* Not used much but why not... */
	unsigned int *cap_id;	/* May be set to non-NULL to store cap id */
} mapi_cap_list_av2;

struct mapi_mheader_av1
{
	int mapi_version;			/* Module API version */
	int (*mapi_register)(void);		/* Register function; ret -1 = failure (unload) */
	void (*mapi_unregister)(void);		/* Unregister function.	*/
	mapi_clist_av1 *mapi_command_list;	/* List of commands to add. */
	mapi_hlist_av1 *mapi_hook_list;		/* List of hooks to add. */
	mapi_hfn_list_av1 *mapi_hfn_list;	/* List of hook_add_hook's to do */
	const char *mapi_module_version;	/* Module's version (freeform) */
};

#define MAPI_ORIGIN_UNKNOWN	0		/* Unknown provenance (AV1 etc.) */
#define MAPI_ORIGIN_EXTENSION	1		/* Charybdis extension */
#define MAPI_ORIGIN_CORE	2		/* Charybdis core module */

struct mapi_mheader_av2
{
	int mapi_version;			/* Module API version */
	int (*mapi_register)(void);		/* Register function; ret -1 = failure (unload) */
	void (*mapi_unregister)(void);		/* Unregister function.	*/
	mapi_clist_av1 *mapi_command_list;	/* List of commands to add. */
	mapi_hlist_av1 *mapi_hook_list;		/* List of hooks to add. */
	mapi_hfn_list_av1 *mapi_hfn_list;	/* List of hook_add_hook's to do */
	mapi_cap_list_av2 *mapi_cap_list;	/* List of CAPs to add */
	const char *mapi_module_version;	/* Module's version (freeform), replaced with ircd version if NULL */
	const char *mapi_module_description;	/* Module's description (freeform) */
	time_t mapi_datecode;	/* Unix timestamp of module's build */
};

#define DECLARE_MODULE_AV1(name, reg, unreg, cl, hl, hfnlist, v) \
	struct mapi_mheader_av1 _mheader = { MAPI_V1, reg, unreg, cl, hl, hfnlist, v}

#define DECLARE_MODULE_AV2(name, reg, unreg, cl, hl, hfnlist, caplist, v, desc) \
	struct mapi_mheader_av2 _mheader = { MAPI_V2, reg, unreg, cl, hl, hfnlist, caplist, v, desc, RB_DATECODE}


/***
Version 3 modules utilize a flexible key/value vector.

Example:
DECLARE_MODULE_AV3
(
	MOD_ATTR { "name",    "mymodule"                       },
	MOD_ATTR { "mtab",    &message_foo                     },
	MOD_ATTR { "mtab",    &message_unfoo                   },
	MOD_ATTR { "init",    modinitfunc                      },
	MOD_ATTR { "hook",    MOD_HOOK   { "myhook", &id }     },
	MOD_ATTR { "hookfn",  MOD_HOOKFN { "myhook", hookfun } },
)

Notes:
- Multiple keys with the same name will have different behavior depending on the logic for that key.
- On load, the order in which keys are specified is the order they will be evaluated (top to bottom).
- On unload, the evaluation is the REVERSE order (bottom to top).
- If an init function returns false, or other error occurs, no further keys are evaluated and the
  unload rolls back from that point.
***/

struct mapi_av3_attr
{
	#define MAPI_V3_KEY_MAXLEN 16                /* Maximum length for a key string */

	const char *key;
	union { const void *cvalue;  void *value;  int (*init)(void);  void (*fini)(void); };
};

struct mapi_mheader_av3
{
	int mapi_version;                            // Module API version
	struct mapi_av3_attr **attrs;                // A vector of attributes, NULL terminated
};

#define MOD_ATTR    &(struct mapi_av3_attr)
#define MOD_HOOK    &(mapi_hlist_av1)
#define MOD_HOOKFN  &(mapi_hfn_list_av1)

#define DECLARE_MODULE_AV3(...)          \
struct mapi_mheader_av3 _mheader =       \
{                                        \
    MAPI_V3, (struct mapi_av3_attr *[])  \
    {                                    \
        MOD_ATTR { "time", RB_DATECODE },\
        __VA_ARGS__,                     \
        NULL                             \
    }                                    \
};


// Prefixes your slog() message with module info
void module_log(struct module *mod, const char *fmt, ...) AFP(2, 3);

/* add a path */
void mod_add_path(const char *path);
void mod_clear_paths(void);

/* load a module */
extern void load_module(char *path);

/* load all modules */
extern void load_all_modules(bool warn);

/* load core modules */
extern void load_core_modules(bool);

extern bool unload_one_module(const char *, bool);
bool load_one_module(const char *, int, bool);
extern bool load_a_module(const char *, bool, int, bool);
struct module *findmodule_byname(const char *);
extern void init_modules(void);

extern rb_dlink_list module_list;
extern rb_dlink_list mod_paths;

}      // namespace ircd
#endif // __cplusplus
