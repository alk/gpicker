#ifndef LOADING_H
#define LOADING_H

#include "vector.h"

extern
struct vector files_vector;

extern char *name_separator;
extern char *dir_separator;
extern char *eat_prefix;

extern int gpicker_bytes_readen;
extern int gpicker_load_stdin_too;

extern int dont_sort_initial;

void init_loading(void);
void read_filenames(int fd);
void read_filenames_abort(void);
void read_filenames_from_mlocate_db(int fd);

#endif
