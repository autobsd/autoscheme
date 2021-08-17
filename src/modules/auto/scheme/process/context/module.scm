(foreign-initialization "s7_define_function( s7, \"command-line\", command_line, 0, 0, false, \"(command-line) returns a list of command-line arguments\" );\n")

(foreign-declaration "static s7_pointer command_line( s7_scheme *sc, s7_pointer args );\n")

(foreign-definition "static s7_pointer command_line( s7_scheme *sc, s7_pointer args )\n"
		    "{\n"
		    "    if( !s7_is_null( sc, args ))\n"
		    "	return s7_wrong_type_arg_error( sc, \"command-line\", 0, args, \"null\" );\n"
		    "    else\n"
		    "    {\n"
		    "	s7_pointer arguments = s7_nil( sc );\n"
		    "	int i;\n"
		    "	for( i = auto_argc - 1; i >= 0; i-- )\n"
		    "	{\n"
		    "	    arguments = s7_cons( sc, s7_make_string( sc, auto_argv[i] ), arguments );\n"
		    "	}\n"
		    "	return arguments;\n"
		    "    }\n"
		    "}\n"
		    )





(foreign-initialization "s7_define_function( s7, \"current-directory\", current_directory, 0, 0, false, \"(current-directory) returns the current working directory\" );\n")

(foreign-declaration "static s7_pointer current_directory( s7_scheme *sc, s7_pointer args );\n")

(foreign-definition "static s7_pointer current_directory( s7_scheme *sc, s7_pointer args )\n"
		    "{\n"
		    "    if( !s7_is_null( sc, args ))\n"
		    "	return s7_wrong_type_arg_error( sc, \"current-directory\", 0, args, \"null\" );\n"
		    "    {\n"
		    "       char buff[FILENAME_MAX];\n"
		    "       GetCurrentDir( buff, FILENAME_MAX );\n"
		    "       return s7_make_string( sc, buff );\n"
		    "    }\n"
		    "}\n")


