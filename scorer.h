#ifndef SCORER_H
#define SCORER_H

struct scorer_query {
	const char *pattern;
};

int score_string(const char *string, const struct scorer_query *query, const unsigned string_length, unsigned* match);
int score_simple_string(const char *string, const char *pattern, unsigned *match);

#endif
