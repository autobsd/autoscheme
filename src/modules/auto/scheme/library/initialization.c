/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
pointer library = make_environment( NIL );
pointer name = mk_symbol( "(auto scheme library)" );

pointer binding = cons( name, library );

car( environment ) = cons( binding, car( environment ));  
INITIALIZE_LIBRARY__auto_scheme_environment( library );
INITIALIZE_LIBRARY__auto_scheme_base( library );
environment = library;




