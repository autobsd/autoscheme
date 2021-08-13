static s7_pointer current_directory( s7_scheme *sc, s7_pointer args )
{
    if( !s7_is_null( sc, args ))
	return s7_wrong_type_arg_error( sc, "current-directory", 0, args, "null" );
    {
       char buff[FILENAME_MAX];
       GetCurrentDir( buff, FILENAME_MAX );
       return s7_make_string( sc, buff );
    }
}
