;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(foreign-declare "pointer LOAD_MODULE__auto_scheme_library( pointer environment );\n"
		 "pointer INITIALIZE_LIBRARY__auto_scheme_environment( pointer environment );\n"
		 )

(foreign-initialize "pointer module_environment = environment;\n"
		    "pointer library = LOAD_MODULE__auto_scheme_library( environment );\n"
		    "INITIALIZE_LIBRARY__auto_scheme_environment( library );\n"
		    "environment = library;\n"
		    )


(environment-import! (foreign-pointer module_environment) (environment-only (current-environment) 
									    ((foreign-procedure OP_STR2SYM) "define-library")
									    ((foreign-procedure OP_STR2SYM) "import")
									    ))

(environment-delete! (foreign-pointer module_environment) ((foreign-procedure OP_STR2SYM) "(auto scheme library)"))


