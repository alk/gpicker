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
#include <string.h>
#include <stdlib.h>
#include <glib.h>
#include "config.h"
#include "inline_qsort.h"
#include "filtration.h"
#include "vector.h"
#include "timing.h"

static
int filter_filename(struct filename *name,
		    const void *_pattern,
		    struct filter_result *result,
		    unsigned *ematch)
{
	const char *pattern = _pattern;
	int patlen = strlen(pattern);
	unsigned match[patlen];
	int score = score_simple_string(name->p + name->dirlength, pattern, match);
	if (score < 0)
		return 0;
	memset(result, 0, sizeof(*result));
	result->score = score;
	result->last_match_pos = (patlen > 0) ? match[patlen-1] : 0;
	if (ematch) {
		int i;
		for (i=0;i<patlen;i++)
			ematch[i] = match[i] + name->dirlength;
	}
	return 1;
}

struct split_pattern {
	char *basename;
	char *dirname;
};

static
int filter_filename_with_dir(struct filename *name,
			     const void *_pattern,
			     struct filter_result *result,
			     unsigned *ematch)
{
	const struct split_pattern *pattern = (struct split_pattern *)_pattern;
	int baselen = strlen(pattern->basename);
	int namelen = strlen(name->p);
	int dirlen = strlen(pattern->dirname);
	unsigned base_match[baselen];
	unsigned dir_match[dirlen];
	struct scorer_query qry;

	if (!name->dirlength)
		return 0;

	qry.pattern = pattern->basename;
	qry.right_match = 0;
	result->score = score_string(name->p+name->dirlength, &qry, namelen-name->dirlength, base_match);
	if (result->score < 0)
		return 0;
	result->last_match_pos = baselen ? base_match[baselen-1] : 0;
	qry.pattern = pattern->dirname;
	qry.right_match = 1;
	result->dirscore = score_string(name->p, &qry, name->dirlength-1, dir_match);
	if (result->dirscore < 0)
		return 0;
	result->first_dir_match_pos = (name->dirlength-1) ? dir_match[0]-name->dirlength : 0;

	if (ematch) {
		int i;
		int dirlen = strlen(pattern->dirname);
		for (i=0;i<dirlen;i++)
			ematch[i] = dir_match[i];
		ematch[dirlen] = name->dirlength-1;
		for (i=0;i<baselen;i++)
			ematch[i+dirlen+1] = base_match[i]+name->dirlength;
	}

	return 1;
}

static
void destroy_filter_with_dir(const void *data)
{
	struct split_pattern *pattern = (struct split_pattern *)data;
	free(pattern->basename);
	free(pattern->dirname);
	free(pattern);
}

const void *prepare_filter(const char *filter, filter_func *func, filter_destructor *destructor)
{
	char *last_slash = strrchr(filter, '/');
	if (!last_slash) {
		*destructor = NULL;
		*func = filter_filename;
		return filter;
	} else {
		*destructor = destroy_filter_with_dir;
		*func = filter_filename_with_dir;
		struct split_pattern *pat = malloc(sizeof(struct split_pattern));
		pat->basename = strdup(last_slash+1);
		pat->dirname = g_strndup(filter, last_slash-filter);
		return pat;
	}
}

int compare_filter_result(struct filter_result *a, struct filter_result *b)
{
	int rv = b->score - a->score;
	struct filename *filea, *fileb;
	if (rv)
		return rv;
	rv = b->dirscore - a->dirscore;
	if (rv)
		return rv;
	rv = a->last_match_pos - b->last_match_pos;
	if (rv)
		return rv;
	filea = files + a->index;
	fileb = files + b->index;
	rv = strlen(filea->p+filea->dirlength) - strlen(fileb->p+fileb->dirlength);
	if (rv)
		return rv;
	rv = b->first_dir_match_pos - a->first_dir_match_pos;
	if (rv)
		return rv;
	return filea->dirlength - fileb->dirlength;
}

struct filter_result *filter_files(char *pattern)
{
	struct filter_result *results;
	const void *filter;
	filter_func filter_func;
	filter_destructor destructor = 0;
	timing_t start;
	int i;

	start = start_timing();
	filter = prepare_filter(pattern, &filter_func, &destructor);
	finish_timing(start, "prepare_filter");

	start = start_timing();
	vector_clear(&filtered);
	finish_timing(start, "vector_clear");

	start = start_timing();
	for (i=0; i<nfiles; i++) {
		struct filter_result result;
		int passes = filter_func(files + i, filter, &result, 0);
		struct filter_result *place;
		if (!passes)
			continue;
		place = vector_append(&filtered);
		result.index = i;
		*place = result;
	}
	finish_timing(start, "actual filtration");

	start = start_timing();
	if (destructor)
		destructor(filter);
	finish_timing(start, "destructor");

	start = start_timing();
	results = (struct filter_result *)filtered.buffer;
	_quicksort(results, filtered.used, sizeof(struct filter_result), (int (*)(const void *, const void *))compare_filter_result);
	finish_timing(start, "qsort");

	return results;
}
