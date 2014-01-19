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
#include <stdlib.h>
#include <stdio.h>
#include <pthread.h>
#include <errno.h>
#include "xmalloc.h"
#include "do_with_main_loop.h"

struct do_with_main_loop_process {
	do_with_main_loop_fn fn;
	gpointer data;
	GMainLoop *loop;
	gpointer rv;
	int refcnt;

	pthread_mutex_t mutex;
	pthread_cond_t ready;
};

static
gboolean idle_func(gpointer _loop)
{
	GMainLoop *loop = _loop;
	g_main_loop_quit(loop);
	g_main_loop_unref(loop);
	return FALSE;
}

static
void *worker(void *_p)
{
	struct do_with_main_loop_process *p = _p;
	p->rv = p->fn(p->data, p);
	if (p->fn)
		do_with_main_loop_init_complete(p);
	if (p->loop)
		g_idle_add(idle_func, g_main_loop_ref(p->loop));
	do_with_main_loop_close(p);
	return NULL;
}

void do_with_main_loop_close(struct do_with_main_loop_process *p)
{
	if (!g_atomic_int_dec_and_test(&p->refcnt))
		return;

	free(p);
}

gpointer do_with_main_loop(do_with_main_loop_fn fn,
			   gpointer data,
			   struct do_with_main_loop_process **p_place)
{
	pthread_t thread;
	int rv;
	GMainLoop *loop = g_main_loop_new(0, FALSE);

	struct do_with_main_loop_process *p = xmalloc(sizeof(struct do_with_main_loop_process));
	p->loop = loop;
	p->fn = fn;
	p->data = data;
	p->rv = 0;
	p->refcnt = 1;
	pthread_mutex_init(&p->mutex, NULL);
	pthread_cond_init(&p->ready, NULL);

	if (p_place) {
		*p_place = p;
		p->refcnt++;
	}

	rv = pthread_create(&thread, NULL,
			    worker, p);

	if (rv) {
		errno = rv;
		perror("pthread_create");
		abort();
	}

	pthread_detach(thread);

	pthread_mutex_lock(&p->mutex);
	while (p->fn) {
		pthread_cond_wait(&p->ready, &p->mutex);
	}
	pthread_mutex_unlock(&p->mutex);

	g_main_loop_run(loop);
	g_main_loop_unref(loop);

	return p->rv;
}

void do_with_main_loop_init_complete(struct do_with_main_loop_process *p)
{
	pthread_mutex_lock(&p->mutex);
	p->fn = 0;
	pthread_cond_broadcast(&p->ready);
	pthread_mutex_unlock(&p->mutex);
}

void do_with_main_loop_quit(struct do_with_main_loop_process *p)
{
	g_main_loop_quit(p->loop);
	p->loop = 0;
}
