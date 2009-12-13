#ifndef LOADING_H
#define LOADING_H

#include <glib.h>
#include "vector.h"

extern
struct vector files_vector;

extern char *project_type;
extern char *project_dir;
extern gboolean disable_bzr;
extern gboolean disable_hg;
extern char *name_separator;
extern char *dir_separator;
extern gboolean read_stdin;
extern char *eat_prefix;
extern gboolean multiselect;

extern int gpicker_bytes_readen;
extern gboolean gpicker_loading_completed;

void read_filenames(int fd);

void read_filenames_with_main_loop(int fd);
void read_filenames_abort(void);

void read_filenames_from_mlocate_db(int fd);

#endif
