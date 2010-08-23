/*
 * Copyright (C) 2009 Aliaksey Kandratsenka
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
#include <string.h>
#include <stdlib.h>
#include <glib.h>
#include "gpicker_xmalloc.h"

#include "refcounted_str.h"

struct refcounted_str *refcounted_str_dup(char *str)
{
	struct refcounted_str *rv = xmalloc(sizeof(struct refcounted_str));
	str = xstrdup(str);

	rv->refcnt = 1;
	rv->str = str;
	return rv;
}

void refcounted_str_get(struct refcounted_str **ptr, struct refcounted_str *src) {
	if (src)
		src->refcnt++;
	*ptr = src;
}

void refcounted_str_put(struct refcounted_str **ptr) {
	struct refcounted_str *rstr = *ptr;
	*ptr = 0;
	if (rstr)
		if (--rstr->refcnt == 0) {
			free(rstr->str);
			free(rstr);
		}
}
