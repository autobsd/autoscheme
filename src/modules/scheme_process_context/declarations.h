/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#include <limits.h>
#include <unistd.h>
#include <string.h>

extern char **environ;

static pointer ff_get_environment_variables( pointer args );
