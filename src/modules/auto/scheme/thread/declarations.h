#include <stdlib.h>
#include <string.h>

#include <pthread.h>

s7_pointer mod_env;

typedef struct {
    s7_pointer proc;
    s7_int proc_loc;
    s7_pointer name;
    s7_scheme *sc;
    pthread_t t;
} thread;


static int thread_type_tag = 1001;


static s7_pointer make_thread( s7_scheme *sc, s7_pointer args );

static s7_pointer free_thread( s7_scheme *sc, s7_pointer arg );

static s7_pointer mark_thread( s7_scheme *sc, s7_pointer arg );

static s7_pointer thread_is_equal( s7_scheme *sc, s7_pointer args );

static s7_pointer thread_to_string( s7_scheme *sc, s7_pointer args );

static s7_pointer is_thread(s7_scheme *sc, s7_pointer args);

static s7_pointer thread_proc( s7_scheme *sc, s7_pointer args );

static s7_pointer set_thread_proc( s7_scheme *sc, s7_pointer args );

static s7_pointer thread_name( s7_scheme *sc, s7_pointer args );

static s7_pointer set_thread_name( s7_scheme *sc, s7_pointer args );


static s7_pointer thread_start( s7_scheme *sc, s7_pointer args );
