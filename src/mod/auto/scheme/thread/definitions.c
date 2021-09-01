/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
static s7_pointer make_thread( s7_scheme *sc, s7_pointer args )
{
    thread *obj = (thread *)malloc( sizeof( thread ));

    /* obj->sc = s7_init(); */

    obj->proc = s7_car( args );

    if ( s7_cdr( args ) != s7_nil( sc ))
	obj->name = s7_cadr( args );
    else 
	obj->name = s7_unspecified( sc );

    return( s7_make_c_object( sc, thread_type_tag, (void *)obj ));
}


static s7_pointer free_thread( s7_scheme *sc, s7_pointer arg )
{
    /* thread *obj = (thread *)s7_c_object_value( arg ); */

    /* s7_free( obj->sc ); */

    free( s7_c_object_value( arg ));
    return( NULL );

    (void)sc;
}

static void *run_thread( void *ptr )
{
    thread *obj = (thread *)ptr;

    obj->sc = s7_init();
    s7_gc_protect( obj->sc, mod_env );

    obj->proc_loc = s7_gc_protect( obj->sc, obj->proc );

    s7_call( obj->sc, obj->proc, s7_nil( obj->sc ));
    s7_gc_unprotect_at( obj->sc, obj->proc_loc );
    s7_free( obj->sc );

    return NULL;
}
static s7_pointer thread_start( s7_scheme *sc, s7_pointer args )
{
    thread *obj = (thread *)s7_c_object_value( s7_car( args ));
    obj->proc_loc = s7_gc_protect( sc, obj->proc );
    pthread_create( &(obj->t) , NULL, run_thread, (void *)obj );

    return( (s7_pointer)obj );

    (void)sc;
}
static s7_pointer _sleep(s7_scheme *sc, s7_pointer args)
{
  int seconds = s7_integer( s7_car(args));
  sleep( seconds );
  return( s7_unspecified( sc ));
}


static s7_pointer mark_thread( s7_scheme *sc, s7_pointer arg )
{
    thread *obj = (thread *)s7_c_object_value( arg );
    s7_mark( obj->proc );
    s7_mark( obj->name );
    return( NULL );

    (void)sc;
}


static s7_pointer thread_is_equal( s7_scheme *sc, s7_pointer args )
{
    return( s7_make_boolean( sc, s7_car( args ) == s7_cadr( args )));
}


static s7_pointer thread_to_string( s7_scheme *sc, s7_pointer args )
{
    char *name_str, *str;

    s7_pointer result;

    int name_str_len;
    thread *obj = (thread *)s7_c_object_value( s7_car( args ));

    name_str = s7_object_to_c_string( sc, obj->name );

    name_str_len = strlen( name_str );

    str = (char *)calloc( name_str_len + 32, sizeof( char ));

    snprintf( str, name_str_len + 32, "#<thread %s>", name_str);

    free( name_str );

    result = s7_make_string( sc, str );

    free( str );


    return( result );
}


static s7_pointer is_thread(s7_scheme *sc, s7_pointer args)
{
    return( s7_make_boolean( sc, s7_is_c_object( s7_car( args )) && s7_c_object_type( s7_car( args )) == thread_type_tag ));
}


static s7_pointer thread_proc( s7_scheme *sc, s7_pointer args )
{
    thread *obj = (thread *)s7_c_object_value( s7_car( args ));
    return( obj->proc );

    (void)sc;
}
static s7_pointer set_thread_proc( s7_scheme *sc, s7_pointer args )
{
    thread *obj = (thread *)s7_c_object_value( s7_car( args ));
    obj->proc = s7_cadr( args );
    return( obj->proc );

    (void)sc;
}


static s7_pointer thread_name( s7_scheme *sc, s7_pointer args )
{
    thread *obj = (thread *)s7_c_object_value( s7_car( args ));
    return( obj->name );

    (void)sc;
}
static s7_pointer set_thread_name( s7_scheme *sc, s7_pointer args )
{
    thread *obj = (thread *)s7_c_object_value( s7_car( args ));
    obj->name = s7_cadr( args );
    return( obj->name );

    (void)sc;
}



