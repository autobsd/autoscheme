/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#include <limits.h>
#include <unistd.h>
#include <string.h>

#include <dirent.h> 

#include <sys/types.h>
#include <sys/stat.h>


#include <errno.h>

pointer ff_create_directory( pointer args );
pointer ff_current_directory( pointer args );
pointer ff_directory_files( pointer args );
pointer ff_directory_p( pointer args );

