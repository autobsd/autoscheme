/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#include <unistd.h>
#include <errno.h>
#include <dirent.h> 
#include <limits.h>
#include <string.h>

pointer ff_file_exists_p( pointer args );

pointer ff_delete_file( pointer args );
