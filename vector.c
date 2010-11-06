/*
 * Copyright (C) 2008,2009 Aliaksey Kandratsenka
 *
 * This program is free software: you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see
 * `http://www.gnu.org/licenses/'.
 */
#include <stdlib.h>
#include <assert.h>
#include <memory.h>
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

struct vector *vector_splice_into(struct vector *src, struct vector *dst)
{
	assert(src->eltsize == dst->eltsize);

	free(dst->buffer);
	*dst = *src;
	src->used = 0;
	src->buffer = 0;
	src->avail = 0;

	return dst;
}

struct vector *vector_concat_into(struct vector *src, struct vector *dst)
{
	assert(src->eltsize == dst->eltsize);

	int new_demand = dst->used + src->used;
	if (new_demand > dst->avail) {
		int v = ((new_demand - 1) >> 16) | (new_demand - 1);
		v |= (v >> 8);
		v |= (v >> 4);
		v |= (v >> 2);
		v |= (v >> 1);
		v += 1;
		if (!v)
			v = 4096 / dst->eltsize;
		dst->buffer = realloc(dst->buffer, v * dst->eltsize);
		dst->avail = v;
	}
	memcpy(dst->buffer + dst->eltsize * dst->used, src->buffer, src->used * dst->eltsize);
	dst->used = new_demand;

	free(src->buffer);
	src->buffer = 0;
	src->avail = src->used = 0;

	return dst;
}
