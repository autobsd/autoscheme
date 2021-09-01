/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
printf( "thread module: initializing...\n" );


thread_type_tag = s7_make_c_type( s7, "thread" );


s7_c_type_set_gc_free( s7, thread_type_tag, free_thread );

s7_c_type_set_gc_mark( s7, thread_type_tag, mark_thread );


s7_c_type_set_is_equal( s7, thread_type_tag, thread_is_equal );


s7_c_type_set_to_string(s7, thread_type_tag, thread_to_string);


s7_define_function(s7, "make-thread", make_thread, 1, 1, false, "(make-thread thunk [name]) makes a new thread");


s7_define_function(s7, "thread?", is_thread, 1, 0, false, "(thread? object) returns #t if its argument is a thread");


s7_define_function(s7, "thread-start!", thread_start, 1, 0, false, "(thread-start! thread) makes thread runnable");



s7_define_variable(s7, "thread-proc", s7_dilambda(s7, "thread-proc", thread_proc, 1, 0, set_thread_proc, 2, 0, "thread proc field"));
s7_define_variable(s7, "thread-name", s7_dilambda(s7, "thread-name", thread_name, 1, 0, set_thread_name, 2, 0, "thread name field"));



s7_define_function(s7, "sleep", _sleep, 1, 0, false, "(sleep seconds) sleep for the specified number of seconds");
