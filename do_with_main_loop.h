#ifndef DO_WITH_MAIN_LOOP_H
#define DO_WITH_MAIN_LOOP_H
#include <glib.h>

struct do_with_main_loop_process;

typedef gpointer (*do_with_main_loop_fn)(gpointer data, struct do_with_main_loop_process *p);

// runs given computation in a new thread while running glib main loop
// when computation completes exits that loop and returns computed value
gpointer do_with_main_loop(do_with_main_loop_fn fn, gpointer data, struct do_with_main_loop_process **p);

// aborts running glib main loop.
// passed function should signal abortion of current computation somehow
// Can be called only from main thread.
// Running in separate thread computation should be interrupted
// by some other means.
void do_with_main_loop_quit(struct do_with_main_loop_process *p);

void do_with_main_loop_close(struct do_with_main_loop_process *p);

// must be called from do_with_main_loop_fn to signal
// that it's safe to run main loop
//
// This function allows computation-specific abort function to work
// reliably without races.
void do_with_main_loop_init_complete(struct do_with_main_loop_process *p);

#endif
