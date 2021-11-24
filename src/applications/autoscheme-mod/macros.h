/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#define QUOTE(name) #name
#define STR(macro) QUOTE(macro)

#ifndef STATE_PATH
#  error STATE_PATH is undefined
#endif

#define STATE_PATH_STR STR(STATE_PATH)
