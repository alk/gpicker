#ifndef VECTOR_H
#define VECTOR_H

struct vector {
	char *buffer;
	int eltsize;
	int used;
	int avail;
};

void *vector_append(struct vector *v);
void vector_clear(struct vector *v);

struct vector files_vector;
struct vector filtered;

#define nfiles (files_vector.used)
#define files ((struct filename *)(files_vector.buffer))

#endif
