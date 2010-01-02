#include <unistd.h>
#include <sys/types.h>
#include <fcntl.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <assert.h>

#include "loading.h"
#include "filtration.h"
#include "vector.h"
#include "xmalloc.h"

static
char *pattern;
static
int output_match;

static
void process_separator(char **separator_place, char *name, char *def)
{
	char *separator = *separator_place;
	if (separator) {
		if (!strcmp(separator, "\\n"))
			separator = "\n";
		if (!strcmp(separator, "\\r"))
			separator = "\r";
		else if (!strcmp(separator, "\\0"))
			separator = "";
		else if (!strcmp(separator, "\\t"))
			separator = "\t";

		if (strlen(separator) > 1) {
			fprintf(stderr, "%s of more than one character is not supported\n", name);
			exit(1);
		}
	} else
		separator = def;

	*separator_place = separator;
}

static
void usage(void)
{
	fputs("usage: gpicker-simple [-hS] [-d dir-seperator] [-n name-separator] pattern\n", stderr);
	exit(0);
}

static
void parse_options(int argc, char **argv)
{
	int ch;

	name_separator = "\n";

	while ((ch = getopt(argc, argv, "hn:d:SM")) > 0) {
		switch (ch) {
		case 'h':
		case '?':
			usage();
		case 'S':
			dont_sort = 1;
			break;
		case 'n':
			name_separator = optarg;
			break;
		case 'd':
			dir_separator = optarg;
			break;
		case 'M':
			output_match = 1;
		}
	}

	process_separator(&name_separator, "name-separator", "");
	process_separator(&dir_separator, "dir-separator", "/");
	filter_dir_separator = dir_separator[0];

	if (optind >= argc) {
		fputs("Missing pattern argument\n", stderr);
		usage();
	}

	pattern = argv[optind++];

	if (optind < argc) {
		fprintf(stderr, "Unknown option: %s\n", argv[optind]);
		usage();
	}
}

int simple_main(int argc, char **argv)
{
	parse_options(argc, argv);

	read_filenames(0);

	filter_files_sync(pattern);

	struct filter_result *results = (struct filter_result *)filtered.buffer;
	int i, len = filtered.used;

	int patlen = strlen(pattern);
	unsigned match[patlen];

	for (i = 0; i < len; i++) {
		int index = results[i].index;
		fputs(files[index].p, stdout);
		fputc(name_separator[0], stdout);

		if (output_match) {
			int passes = obtain_match(pattern, index, match);
			assert(passes);

			unsigned current_pos = 0;
			int k;
			for (k = 0; k < patlen; k++) {
				for (; current_pos < match[k]; current_pos++)
					fputc(' ', stdout);
				fputc('^', stdout);
				current_pos++;
			}
			fputc(name_separator[0], stdout);
		}
	}

	return 0;
}
