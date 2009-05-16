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
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <glib.h>
#include "config.h"
#include "inline_qsort.h"
#include "filtration.h"
#include "vector.h"
#include "timing.h"

struct simple_filter_state {
	struct scorer_query query;
	struct prepared_pattern *prep;
};

static
int filter_filename(struct filename *name,
		    const void *_state,
		    struct filter_result *result,
		    unsigned *ematch)
{
	const struct simple_filter_state *state = _state;
	unsigned patlen = state->prep->pat_length;
	unsigned match[patlen];
	const char *string = name->p + name->dirlength;
	int score = score_string_prepared(string, &state->query, state->prep, strlen(string), match);
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

static
void free_simple_filter_state(struct simple_filter_state *p) {
	if (!p)
		return;
	free_prepared_pattern(p->prep);
	free(p);
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
void destroy_filter_with_dir(void *data)
{
	struct split_pattern *pattern = (struct split_pattern *)data;
	free(pattern->basename);
	free(pattern->dirname);
	free(pattern);
}

void *prepare_filter(const char *filter, filter_func *func, filter_destructor *destructor)
{
	char *last_slash = strrchr(filter, '/');
	if (!last_slash) {
		*destructor = (filter_destructor)free_simple_filter_state;
		*func = filter_filename;
		struct simple_filter_state *state = malloc(sizeof(struct simple_filter_state));
		state->query.pattern = filter;
		state->query.right_match = 0;
		state->prep = prepare_pattern(&state->query);
		return state;
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

struct vector partial_filtered = {.eltsize = sizeof(struct filter_result)};

static volatile
unsigned abort_filtration_request;

static
struct filter_result *do_filter_files(const char *pattern)
{
	struct filter_result *results;
	void *filter;
	filter_func filter_func;
	filter_destructor destructor = 0;
	timing_t start, whole;
	int i;

	whole = start = start_timing();
	vector_clear(&partial_filtered);
	finish_timing(start, "vector_clear");

	if (pattern[0] == 0) {
		int limit = (nfiles > FILTER_LIMIT) ? FILTER_LIMIT : nfiles;
		start = start_timing();
		for (i=0; i < limit; i++) {
			struct filter_result result = {.index = i, .score = 0};
			struct filter_result *place;
			place = vector_append(&partial_filtered);
			*place = result;
		}
		finish_timing(start, "prepare_filter:blank pattern case");
		return (struct filter_result *)partial_filtered.buffer;
	}

	start = start_timing();
	filter = prepare_filter(pattern, &filter_func, &destructor);
	finish_timing(start, "prepare_filter");

	start = start_timing();
	for (i=0; i<nfiles; i++) {
		if (!(i % 16384) && abort_filtration_request)
			break;
		struct filter_result result;
		int passes = filter_func(files + i, filter, &result, 0);
		struct filter_result *place;
		if (!passes)
			continue;
		place = vector_append(&partial_filtered);
		result.index = i;
		*place = result;
	}
	finish_timing(start, "actual filtration");

	start = start_timing();
	if (destructor)
		destructor(filter);
	finish_timing(start, "destructor");

	if (abort_filtration_request) {
		timing_printf("this is aborted filtration\n");
		return 0;
	}

	start = start_timing();
	results = (struct filter_result *)partial_filtered.buffer;
	_quicksort_top(results, partial_filtered.used, sizeof(struct filter_result), (int (*)(const void *, const void *))compare_filter_result, results + FILTER_LIMIT);
	finish_timing(start, "qsort");

	finish_timing(whole, "whole do_filter_files");
	timing_printf("Result-set size is %d\n", partial_filtered.used);

	return results;
}

static char *ft_pattern;
static char *ft_applied_pattern;
static GMutex *ft_mutex;
static GCond *ft_filtation_cond;
static unsigned ft_idle_pending;
static void (*ft_callback)(char *);

static
gpointer filtration_thread(gpointer);

static void init_ft_state()
{
	if (ft_mutex)
		return;
	ft_mutex = g_mutex_new();
	ft_filtation_cond = g_cond_new();
	g_thread_create(filtration_thread, 0, 0, 0);
}

static
gboolean filtration_idle(gpointer _pattern)
{
	g_mutex_lock(ft_mutex);
	ft_callback(ft_applied_pattern);
	if (ft_applied_pattern)
		free(ft_applied_pattern);
	ft_applied_pattern = 0;
	ft_idle_pending = 0;
	g_mutex_unlock(ft_mutex);
	return FALSE;
}

static
gpointer filtration_thread(gpointer _dummy)
{
	char *pattern;
	g_mutex_lock(ft_mutex);

again:
	while (!ft_pattern)
		g_cond_wait(ft_filtation_cond, ft_mutex);
	pattern = ft_pattern;
	g_mutex_unlock(ft_mutex);

	timing_printf("filtration_thread: waked up for '%s'\n", pattern);

	struct filter_result *rv = do_filter_files(pattern);

	g_mutex_lock(ft_mutex);

	if (!abort_filtration_request) {
		ft_pattern = 0;
	}

	abort_filtration_request = 0;

	if (!rv) {
		free(pattern);
		goto again;
	}

	vector_clear(&filtered);
	vector_splice_into(&partial_filtered, &filtered);
	if (ft_applied_pattern)
		free(ft_applied_pattern);
	ft_applied_pattern = pattern;

	if (!ft_idle_pending) {
		g_idle_add(filtration_idle, 0);
		ft_idle_pending = 1;
	}

	goto again;

	return 0;
}

void filter_files(char *pattern, void (*callback)(char *))
{
#if 1
	init_ft_state();
	g_mutex_lock(ft_mutex);
	if (ft_pattern)
		abort_filtration_request = 1;
	ft_pattern = strdup(pattern);
	ft_callback = callback;
	g_cond_broadcast(ft_filtation_cond);
	g_mutex_unlock(ft_mutex);
#else
	do_filter_files(pattern);

	vector_clear(&filtered);
	vector_splice_into(&partial_filtered, &filtered);

	callback(pattern);
#endif
}
