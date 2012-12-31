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
struct vector *vector_splice_into(struct vector *src, struct vector *dst);
struct vector *vector_concat_into(struct vector *src, struct vector *dst);

extern
struct vector files_vector;

#define files ((struct filename *)(files_vector.buffer))

#endif
