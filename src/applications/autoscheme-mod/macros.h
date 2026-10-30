/*  This file is part of the 'AutoScheme' project.
 *  Copyright (c) 2026 AutoBSD Contributors.
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#define QUOTE(name) #name
#define STR(macro) QUOTE(macro)

#ifndef INSTALL_PATH
#  error INSTALL_PATH is undefined
#endif
#define INSTALL_PATH_STR STR(INSTALL_PATH)

#ifndef STATE_PATH
#  error STATE_PATH is undefined
#endif
#define STATE_PATH_STR STR(STATE_PATH)
