/*
 * Copyright (C) 2008 Aliaksey Kandratsenka
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
#include <stdio.h>
#include <stdlib.h>
#include "xmalloc.h"

__attribute__((noreturn))
void memory_exhausted(void)
{
	fputs("memory exhausted", stderr);
	abort();
}

void *xmalloc(size_t size)
{
	void *rv = malloc(size);
	if (!rv)
		memory_exhausted();
	return rv;
}

void *xrealloc(void *ptr, size_t size)
{
	ptr = realloc(ptr, size);
	if (!ptr)
		memory_exhausted();
	return ptr;
}
