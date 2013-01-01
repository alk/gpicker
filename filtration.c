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
#ifndef NO_CONFIG
#include "config.h"
#endif

#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#ifdef _OPENMP
#include <omp.h>
#endif

#ifdef WITH_GUI
#include <glib.h>
#endif

#include "inline_qsort.h"
#include "filtration.h"
#include "vector.h"
#include "timing.h"
#include "refcounted_str.h"
#include "xmalloc.h"

#ifndef WITH_GUI
#define NO_PARALLEL_FILTRATION
#endif

struct simple_filter_state {
	struct scorer_query query;
	struct prepared_pattern *prep;
};

char filter_dir_separator = '/';
int dont_sort;
int ignore_positions;

struct vector filtered = {.eltsize = sizeof(struct filter_result)};

static
char *cheap_strndup(const char *s, size_t n)
{
	char *rv = xmalloc(n+1);
	rv[n] = 0;
	strncpy(rv, s, n);
	return rv;
}

static
int filter_filename(struct filename *name,
		    const void *_state,
		    struct filter_result *result,
		    unsigned *ematch)
{
	const struct simple_filter_state *state = _state;
	unsigned patlen = state->prep ? state->prep->pat_length : 0;
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

static
void *prepare_dir_filter(char *basename, char *dirname, filter_func *func, filter_destructor *destructor)
{
	*destructor = destroy_filter_with_dir;
	*func = filter_filename_with_dir;
	struct split_pattern *pat = malloc(sizeof(struct split_pattern));
	pat->basename = basename; //xstrdup(last_slash+1);
	pat->dirname = dirname; // cheap_strndup(filter, last_slash-filter);
	return pat;
}

void *prepare_filter(const char *filter, filter_func *func, filter_destructor *destructor)
{
	char *last_slash = strrchr(filter, '\\');
	if (last_slash) {
		// in this case basename comes first
		return prepare_dir_filter(cheap_strndup(filter, last_slash-filter),
					  xstrdup(last_slash+1),
					  func, destructor);
	}

	last_slash = strrchr(filter, filter_dir_separator);
	if (last_slash) {
		return prepare_dir_filter(xstrdup(last_slash+1),
					  cheap_strndup(filter, last_slash-filter),
					  func, destructor);
	}

	*destructor = (filter_destructor)free_simple_filter_state;
	*func = filter_filename;
	struct simple_filter_state *state = malloc(sizeof(struct simple_filter_state));
	state->query.pattern = filter;
	state->query.right_match = 0;
	state->prep = prepare_pattern(&state->query);
	return state;
}

static
int compare_filter_result(struct filter_result *a, struct filter_result *b)
{
	int rv = b->score - a->score;
	struct filename *filea, *fileb;
	if (rv)
		return rv;
	rv = b->dirscore - a->dirscore;
	if (rv)
		return rv;
	if (ignore_positions)
		return 0;

	rv = a->last_match_pos - b->last_match_pos;
	if (rv)
		return rv;
	filea = files + a->index;
	fileb = files + b->index;
	rv = filea->restlen - fileb->restlen;
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
	int j,i;
	int nfiles = files_vector.used;


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

#ifdef _OPENMP
#	pragma omp parallel						\
	shared(files_vector, abort_filtration_request, partial_filtered, filter, nfiles) \
	private(j, i)
#endif
	{
		struct vector *filtered_collection;
#ifdef _OPENMP
		filtered_collection = xmalloc(sizeof(struct vector));
		memset(filtered_collection, 0, sizeof(struct vector));
		filtered_collection -> eltsize = sizeof(struct filter_result);
#else
		filtered_collection = &partial_filtered;
#endif

#ifdef _OPENMP
		/* timing_printf("OMP thread %d started\n", omp_get_thread_num()); */
#		pragma omp for schedule(static) nowait
#endif
		for (j=0; j<nfiles; j += 16384) {
			int bound = j + 16384;
			if (bound >= files_vector.used)
				bound = files_vector.used;

#ifdef _OPENMP
			if (abort_filtration_request)
				continue;
#else
			if (abort_filtration_request)
				break;
#endif

			for (i = j; i < bound; i++) {
				struct filter_result result;
				int passes = filter_func(((struct filename *)(files_vector.buffer)) + i,
							 filter, &result, 0);
				struct filter_result *place;
				if (!passes)
					continue;
				place = vector_append(filtered_collection);
				result.index = i;
				*place = result;
			}
		}

#ifdef _OPENMP
#		pragma omp critical
		{
			vector_concat_into(filtered_collection, &partial_filtered);
		}
		free(filtered_collection);
#endif
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
	if (!dont_sort)
		_quicksort_top(results, partial_filtered.used, sizeof(struct filter_result), (int (*)(const void *, const void *))compare_filter_result, results + FILTER_LIMIT);
	finish_timing(start, "qsort");

	finish_timing(whole, "whole do_filter_files");
	timing_printf("Result-set size is %d\n", partial_filtered.used);

	return results;
}

int obtain_match(const char *pattern, int files_index, unsigned *match)
{
	void *filter;
	filter_func filter_func;
	filter_destructor destructor = 0;
	struct filter_result result;
	int passes;

	filter = prepare_filter(pattern, &filter_func, &destructor);
	passes = filter_func(files + files_index, filter, &result, match);
	if (destructor)
		destructor(filter);

	return passes;
}


#ifdef WITH_GUI

static struct refcounted_str *ft_pattern;
static struct refcounted_str *ft_applied_pattern;
static GMutex *ft_mutex;
static GCond *ft_filtation_cond;
static unsigned ft_idle_pending;
static void (*ft_callback)(char *);

static
gpointer filtration_thread(gpointer);

static
GThread *filtration_thread_id;

static void init_ft_state()
{
	if (ft_mutex)
		return;
	ft_mutex = g_mutex_new();
	ft_filtation_cond = g_cond_new();
	filtration_thread_id = g_thread_create(filtration_thread, 0, 1, 0);
	if (!filtration_thread_id) {
		fputs("failed to create filtration thread\n", stderr);
		abort();
	}
}

static
gboolean filtration_idle(gpointer _pattern)
{
	g_mutex_lock(ft_mutex);
	ft_callback(ft_applied_pattern->str);
	refcounted_str_put(&ft_applied_pattern);
	ft_idle_pending = 0;
	g_mutex_unlock(ft_mutex);
	return FALSE;
}

static volatile
unsigned kill_filtration_thread;

static
gpointer filtration_thread(gpointer _dummy)
{
	struct refcounted_str *pattern;
	g_mutex_lock(ft_mutex);

again:
	while (!ft_pattern && !kill_filtration_thread)
		g_cond_wait(ft_filtation_cond, ft_mutex);

	if (kill_filtration_thread) {
		g_mutex_unlock(ft_mutex);
		return 0;
	}

	refcounted_str_get(&pattern, ft_pattern);
	g_mutex_unlock(ft_mutex);

	timing_printf("filtration_thread: waked up for '%s'\n", pattern->str);

	struct filter_result *rv = do_filter_files(pattern->str);

	g_mutex_lock(ft_mutex);

	if (kill_filtration_thread) {
		refcounted_str_put(&pattern);
		g_mutex_unlock(ft_mutex);
		return 0;
	}

	if (!abort_filtration_request) {
		// no abort request was made, so ft_pattern == pattern
		refcounted_str_put(&ft_pattern); // consume pattern and mark that we're free
	} else {
		// abort request was made. Consume this request.
		abort_filtration_request = 0;

		// if we managed to filter anything
		// (i.e. abort came too late) continue
		// otherwise free pattern we partially filtered for and
		// start new filter cycle
		if (!rv)
			goto put_pattern;
	}

	// put new data for g_idle callback that displays results
	vector_clear(&filtered);
	vector_splice_into(&partial_filtered, &filtered);
	// if it had old unconsumed pattern, free it
	refcounted_str_put(&ft_applied_pattern);
	refcounted_str_get(&ft_applied_pattern, pattern);

	// schedule g_idle execution if not already scheduled
	if (!ft_idle_pending) {
		g_idle_add(filtration_idle, 0);
		ft_idle_pending = 1;
	}

put_pattern:
	refcounted_str_put(&pattern);

	goto again;

	return 0;
}

void uninit_filter(void)
{
	if (!ft_mutex)
		return;

	g_mutex_lock(ft_mutex);

	if (ft_pattern)
		refcounted_str_put(&ft_pattern);
	abort_filtration_request = 1;
	kill_filtration_thread = 1;
	ft_pattern = 0;
	ft_callback = 0;

	g_cond_broadcast(ft_filtation_cond);
	g_mutex_unlock(ft_mutex);

	g_thread_join(filtration_thread_id);
}

#else // !WITH_GUI

void uninit_filter(void)
{
}

#endif // !WITH_GUI

void filter_files_sync(char *pattern)
{
	do_filter_files(pattern);

	vector_clear(&filtered);
	vector_splice_into(&partial_filtered, &filtered);
}

void filter_files(char *pattern, void (*callback)(char *))
{
#ifndef NO_PARALLEL_FILTRATION
	init_ft_state();
	g_mutex_lock(ft_mutex);
	if (ft_pattern) {
		abort_filtration_request = 1;
		refcounted_str_put(&ft_pattern);
	}
	ft_pattern = refcounted_str_dup(pattern);
	ft_callback = callback;
	g_cond_broadcast(ft_filtation_cond);
	g_mutex_unlock(ft_mutex);
#else
	filter_files_sync(pattern);
	callback(pattern);
#endif
}
