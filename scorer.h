#ifndef SCORER_H
#define SCORER_H
#include <limits.h>

struct scorer_query {
	const char *pattern;
	int right_match;
};

struct filter_result {
	int index;
	int score;
	int dirscore;
	short last_match_pos;
	short first_dir_match_pos;
};

struct prepared_pattern {
	char *translated_pattern;
	char *start_of_pattern_word;
	unsigned pat_length;
	char first_chars[8];
	char fc_count;
};

extern int scorer_utf8_mode; // on by default

static inline
int utf8_continuation_p(char p)
{
	return (p & 0xc0) == 0x80;
}

// signals that there are no highlight-able match in this pattern position
#define SCORER_MATCH_NONE UINT_MAX

int score_string(const char *string, const struct scorer_query *query, const unsigned string_length, unsigned* match);
int score_simple_string(const char *string, const char *pattern, unsigned *match);

struct prepared_pattern *prepare_pattern(const struct scorer_query *query);
void free_prepared_pattern(struct prepared_pattern *p);

int score_string_prepared(const char *string,
			  const struct scorer_query *query,
			  const struct prepared_pattern *prepared_pattern,
			  const unsigned string_length,
			  unsigned* match);

#endif
