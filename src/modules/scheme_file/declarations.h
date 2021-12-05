/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#include <unistd.h>
#include <errno.h>

static pointer ff_file_exists_p( pointer args );

static pointer ff_delete_file( pointer args );
