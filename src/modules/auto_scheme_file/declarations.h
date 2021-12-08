/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#include <stdlib.h>
#include <sys/stat.h>

#include <string.h>
#include <dirent.h> 
#include <limits.h>



foreign_function ff_delete_file;
foreign_function ff_file_exists_p;

static pointer ff_rename_file( pointer args );
static pointer ff_copy_file( pointer args );
