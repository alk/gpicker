#ifndef SCORER_H
#define SCORER_H

struct scorer_query {
	const char *pattern;
	int right_match;
};

struct filter_result {
	int index;
	int score;
	int last_match_pos;
	int dirscore;
	int first_dir_match_pos;
};

int score_string(const char *string, const struct scorer_query *query, const unsigned string_length, unsigned* match);
int score_simple_string(const char *string, const char *pattern, unsigned *match);

#endif
