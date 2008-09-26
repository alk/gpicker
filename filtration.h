#ifndef FILTRATION_H
#define FILTRATION_H

#include "scorer.h"

struct filename {
	char *p;
	int dirlength;
};

int filter_filename(struct filename *name,
		    const void *_pattern,
		    struct filter_result *result,
		    unsigned *ematch);

int filter_filename_with_dir(struct filename *name,
			     const void *_pattern,
			     struct filter_result *result,
			     unsigned *ematch);

typedef void (*filter_destructor)(const void *);
typedef int (*filter_func)(struct filename *, const void *, struct filter_result *, unsigned *);

const void *prepare_filter(const char *filter, filter_func *func, filter_destructor *destructor);

int compare_filter_result(struct filter_result *a, struct filter_result *b);


#endif
