#include <stdlib.h>
#include "vector.h"

void *vector_append(struct vector *v)
{
	if (v->used == v->avail) {
		int new_avail = v->avail * 2;
		if (!new_avail)
			new_avail = 4096 / v->eltsize;
		v->buffer = realloc(v->buffer, new_avail * v->eltsize);
		v->avail = new_avail;
	}

	return v->buffer + v->eltsize * (v->used++);
}

void vector_clear(struct vector *v)
{
	v->used = 0;
	v->buffer = realloc(v->buffer, 0);
	v->avail = 0;
}
