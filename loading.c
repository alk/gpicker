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
#include <sys/errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdint.h>
#include <memory.h>
#include <assert.h>
#include <arpa/inet.h> // for ntohl

#include "timing.h"
#include "filtration.h"
#include "xmalloc.h"
#include "loading.h"
#include "inline_qsort.h"

struct vector files_vector = {.eltsize = sizeof(struct filename)};

char *name_separator;
char *dir_separator;
char *default_eat_prefix = "./";
char *eat_prefix;

int gpicker_bytes_readen;
int gpicker_load_stdin_too;

int dont_sort_initial;

static volatile
int reading_aborted;

void init_loading(void)
{
	eat_prefix = default_eat_prefix;
}

static
void add_filename(char *p, int dirlength, int restlen)
{
	struct filename *last = vector_append(&files_vector);

	last->p = p;
	last->dirlength = dirlength;
	last->restlen = restlen;
}

#define INIT_BUFSIZE (128*1024)
#define MIN_BUFSIZE_FREE 32768

static
char *input_names(int fd, char **endp)
{
	int bufsize = INIT_BUFSIZE;
	char *buf = xmalloc(bufsize);
	int filled = 0;

	do {
		int readen = read(fd, buf+filled, bufsize-filled);
		if (reading_aborted) {
			*endp = buf;
			buf[0] = 0;
			return buf;
		}
		if (readen == 0) {
			if (gpicker_load_stdin_too && fd != 0) {
				fd = 0;
				continue;
			}
			break;
		}
		else if (readen < 0) {
			if (errno == EINTR)
				continue;
			perror("read_names");
			break;
		}
		filled += readen;
		gpicker_bytes_readen = filled;
		if (bufsize - filled < MIN_BUFSIZE_FREE) {
			bufsize = filled + MIN_BUFSIZE_FREE * 2;
			buf = xrealloc(buf, bufsize);
		}
	} while (1);
	if (filled) {
		if (buf[filled-1] != name_separator[0])
			filled++;
		buf = xrealloc(buf, filled);
		buf[filled-1] = name_separator[0];
	}
	*endp = buf+filled;
	return buf;
}

static
int filename_compare(struct filename *a, struct filename *b)
{
	return strcasecmp(a->p, b->p);
}

void read_filenames(int fd)
{
	char *endp;
	char *buf = input_names(fd, &endp);
	char *p = buf;

	int eat_prefix_len = strlen(eat_prefix);

	timing_t start = start_timing();

	while (p < endp) {
		int dirlength = 0;
		char *start;
		char ch;
		if (strncmp(eat_prefix, p, eat_prefix_len) == 0)
			p += eat_prefix_len;
		start = p;
#if 1
		while ((ch = *p++) != name_separator[0])
			if (ch == filter_dir_separator)
				dirlength = p - start;
#else
		p = rawmemchr(start, name_separator[0]) + 1;
		char *rp = memrchr(start, filter_dir_separator, p - 1 - start);
		dirlength = rp ? rp - start : 0;
#endif
		p[-1] = 0;
		add_filename(start, dirlength, p - 1 - start - dirlength);
	}

	if (!dont_sort && !dont_sort_initial)
		_quicksort_top(files, files_vector.used, sizeof(struct filename),
			       (int (*)(const void *, const void *))filename_compare, files + FILTER_LIMIT);

	finish_timing(start, "filenames parsing");
}

void read_filenames_abort(void)
{
	reading_aborted = 1;
}

#ifndef NO_CONFIG
static
void read_all(int fd, char *data, int size)
{
	int readen;
	while (size > 0) {
		readen = read(fd, data, size);
		if (readen < 0) {
			if (errno == EINTR)
				continue;
			perror("read_all");
			exit(1);
		}
		if (readen == 0)
			abort();
		data += readen;
		size -= readen;
	}
}

void read_filenames_from_mlocate_db(int fd)
{
	static char read_buffer[65536];

	timing_t start;
	struct stat st;
	int rv = fstat(fd, &st);
	char *data;

	start = start_timing();

	if (rv < 0) {
		perror("read_filenames_from_mlocate_db:fstat");
		exit(1);
	}

	data = xmalloc(st.st_size);
	read_all(fd, data, st.st_size);

	/* see man 5 mlocate.db */
	char *prefix = data + 0x10;
	int prefix_len = strlen(prefix);
	char *end = data + st.st_size;
	char *strings = prefix + prefix_len + ntohl(*((uint32_t *)(data + 0x8))) + 0x11;

	if (prefix[prefix_len-2] == '/')
		prefix_len--;

	if (eat_prefix != default_eat_prefix) {
		prefix_len = 0;
		if (eat_prefix[0] != 0) {
			fprintf(stderr, "mlocate project type supports only default and empty prefixes\n");
			exit(1);
		}
	}

	while (strings < end) {
		// dir name is in strings
		int len = strlen(strings);
		int read_buffer_len = strlen(strings + prefix_len);
		char *used_prefix = strings + prefix_len;
		if (prefix_len != 0 && *used_prefix == '/') {
			used_prefix++;
			read_buffer_len--;
		}
		memcpy(read_buffer, used_prefix, read_buffer_len);
		if (read_buffer_len && read_buffer[read_buffer_len-1] != '/')
			read_buffer[read_buffer_len++] = '/';

		char *p = strings + len + 1;

		while (p[0] == 1 || p[0] == 0) {
			p++;
			int p_len = strlen(p);

			if (p[-1] == 0) {
				memcpy(read_buffer+read_buffer_len, p, p_len);
				int total_len = read_buffer_len + p_len;
				char *last_slash = memrchr(read_buffer, '/', total_len);
				int dirlength = last_slash ? last_slash + 1 - read_buffer : 0;
				char *dup = malloc(total_len+1);
				memcpy(dup, read_buffer, total_len);
				dup[total_len] = 0;
				add_filename(dup, dirlength, total_len - dirlength);
			}

			p += p_len + 1;
		}

		assert(p[0] == 2);

		strings = p + 0x11;
	}

	free(data);
	finish_timing(start, "read_filenames_from_mlocate_db");

	if (gpicker_load_stdin_too) {
		read_filenames(0);
	}

	if (!dont_sort) {
		start = start_timing();
		_quicksort_top(files, files_vector.used, sizeof(struct filename),
			       (int (*)(const void *, const void *))filename_compare, files + FILTER_LIMIT);
		finish_timing(start, "initial qsort");
	}
}
#endif /* NO_CONFIG */
