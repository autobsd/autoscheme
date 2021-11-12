/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
pointer INITIALIZE_LIBRARY__auto_scheme_base( pointer environment );

foreign_function ff_current_directory;
foreign_function ff_path_absolute_p;
foreign_function ff_path_make_absolute;
foreign_function ff_path_directory;

foreign_function ff_environment_define_d;
