/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
scheme_register_foreign_func( "make-environment",            make_environment               );
scheme_register_foreign_func( "environment-define!"  ,       ff_environment_define_d        );
scheme_register_foreign_func( "environment-undefine!",       ff_environment_undefine_d      );
scheme_register_foreign_func( "environment-defined-symbols", ff_environment_defined_symbols );
scheme_register_foreign_func( "environment-assoc",           ff_environment_assoc           );
scheme_register_foreign_func( "environment-ref",             ff_environment_ref             );
scheme_register_foreign_func( "environment-update!",         ff_environment_update_d        );
scheme_register_foreign_func( "environment-import!",         ff_environment_import_d        );
scheme_register_foreign_func( "environment-only",            ff_environment_only            );
scheme_register_foreign_func( "environment-except",          ff_environment_except          );
scheme_register_foreign_func( "environment-prefix",          ff_environment_prefix          );
scheme_register_foreign_func( "environment-rename",          ff_environment_rename          );


