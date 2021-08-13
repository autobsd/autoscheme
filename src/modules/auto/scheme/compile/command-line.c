static s7_pointer command_line( s7_scheme *sc, s7_pointer args )
{
    if( !s7_is_null( sc, args ))
	return s7_wrong_type_arg_error( sc, "command-line", 0, args, "null" );
    else
    {
	s7_pointer arguments = s7_nil( sc );
	int i;
	for( i = auto_argc - 1; i >= 0; i-- )
	{
	    arguments = s7_cons( sc, s7_make_string( sc, auto_argv[i] ), arguments );
	}
	return arguments;
    }
}
