# IRCd Module Tree

This directory contains dynamically loadable functionality to libircd. Many of
these modules provide essential application functionality, but are not always
required to be directly linked and loaded into libircd proper. Most application-
specific functionality (i.e "business logic") is contained in modules within this
tree.

#### Layout

The `modules/` directory tree is primarily shaped the same as the HTTP resource
tree in which most of its modules register themselves in.

Note that the installation layout is not the same as the development source
layout (i.e in git). Upon installation, the module tree is collapsed into a
single directory and installed into
`$prefix/lib/modules/construct/$directory_$module.so`; where directories are
replaced by underscores in the final `SONAME`. this may be subject to
improvement.

#### Approach

Unlike most of the module systems found in traditional free software projects,
our approach is oriented around *global symbol* availability to the address
space rather than explicit imports from self-contained modules. This direction
is made viable by C++ and advances in the compiler and linker toolchains. The
result is significantly simpler and more convenient for developers to work with.

- Modules are loaded with `RTLD_GLOBAL` on both posix and windows platforms.
Use of C++ namespaces, visibility attributes, `STB_GNU_UNIQUE`, etc are
adequate to make this modernization.

- All project code is built to silently weaken undefined symbols. This means
a complicated interface declared in a header, like a class interface with
public and private and static members -- typical in C++ -- can be included
by itself into any part of the project without knowing where the definitions
of that interface are until they are *first used* at runtime. If said
definitions are not available because they are in an unloaded module, a C++
exception is thrown directly from the callsite.

## Getting started

The header `mods/mapi.h` is specific to modules and included for modules in
addition to the core `ircd.h`. Both of these are included automatically
via the compiler's command-line and the developer does not have to `#include`
either in the module.

1. Every loadable module requires a static `ircd::mapi::header` with the explicit
name of `IRCD_MODULE`. This is an object which will be sought by the module
loader in libircd.

```
// Example of a trivial module

ircd::mapi::header
IRCD_MODULE
{
	"My Module", []
	{
		puts("hi\n");
	}
};

```

2. Add an `_la_SOURCES` entry for your module in the appropriate place in
Makefile.am.

3. Add the module `.la` name to the appropriate LTLIBRARIES list
in Makefile.am.

4. At this time, most modules are listed explicitly in `ircd/m.cc` to
provide a strict load and unload ordering based on dependency. Note that
if the module is not `m::` related there may be similar lists for other
subsystems.
