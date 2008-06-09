#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#include "scorer.h"

#define PROPER_WORD_START 501
#define WILD_WORD_START 1
#define ADJACENT 1000

#define max(a,b) ({__typeof__ (a) ____a = (a); __typeof__ (b) ____b = (b); ____b > ____a ? ____b : ____a; })

#if 0
#define dprintf(...) printf(__VA_ARGS__)
#else
__attribute__((format (printf, 1, 2))) __attribute__((unused))
static inline
void ____empty_printf(char *format, ...)
{
}
#define dprintf(...) ____empty_printf(__VA_ARGS__)
#endif

static inline
char normalize_char(char ch, unsigned *is_delimiter)
{
	ch = tolower(ch);
	if (ch == '-')
		ch = '_';
	if (is_delimiter)
		*is_delimiter = (ch == '.' || ch == '_' || ch == '/');
	return ch;
}

int score_string(const char *string,
		 const struct scorer_query *query,
		 const unsigned string_length,
		 unsigned* match)
{
	struct position {
		int this_score;
		int score;
		int amount;
	};
	const char *pattern = query->pattern;
	const unsigned pat_length = strlen(pattern);
	struct position state[string_length][pat_length];
	unsigned start_of_pattern_word[pat_length];
	char translated_pattern[pat_length];
	unsigned i;
	unsigned k;
	unsigned previous_delimiter;
	int score;

	if (pat_length == 0)
		return 0;

	dprintf("scoring string '%.*s' for pattern '%s'\n", string_length, string, pattern);

	for (i=0; i<string_length; i++)
		for (k=0; k<pat_length; k++)
			state[i][k] = (struct position){.this_score = -1, .score = -1, .amount = 0};

	previous_delimiter = 1;
	for (i=0; i<pat_length; i++) {
		char ch = pattern[i];
		start_of_pattern_word[i] = previous_delimiter || ('A' <= ch && ch <= 'Z');
		translated_pattern[i] = normalize_char(ch, &previous_delimiter);
	}
	previous_delimiter = 0;

	for (i=0; i<string_length; i++) {
		char ch = string[i];
		unsigned at_word_start = (i == 0);
		at_word_start = at_word_start || ('A' <= ch && ch <= 'Z');
		at_word_start = at_word_start || previous_delimiter;
		ch = normalize_char(ch, &previous_delimiter);
		if (ch == translated_pattern[0]) {
			int amount = at_word_start ? PROPER_WORD_START : 0;
			if (i > 0 && state[i-1][0].score > amount)
				state[i][0].score = state[i-1][0].score;
			else
				state[i][0].score = amount;
			state[i][0].this_score = amount;
			state[i][0].amount = amount;
		} else {
			state[i][0].this_score = -1;
			state[i][0].amount = 0;
			state[i][0].score = i > 0 ? state[i-1][0].score : -1;
		}
		if (i == 0)
			continue;
		for (k=1; k<pat_length; k++) {
			char pat_ch = translated_pattern[k];
			int prev_score;
			char prev_pattern;
			int amount;
			int this_score;
			if (ch != pat_ch) {
			cont:
				state[i][k].score = state[i-1][k].score;
				state[i][k].amount = 0;
				state[i][k].this_score = -1;
				continue;
			}
			prev_score = state[i-1][k-1].score;
			if (prev_score < 0)
				goto cont;
			amount = 0;
			this_score = -1;
			prev_pattern = translated_pattern[k-1];
			if (prev_pattern != '_' && prev_pattern != '.' && prev_pattern != '/') {
				if (at_word_start)
					amount = start_of_pattern_word[k] ? PROPER_WORD_START : WILD_WORD_START;
				this_score = prev_score + amount;
			}

			if (state[i-1][k-1].this_score >= 0 && pat_ch != '_' && pat_ch != '.' && pat_ch != '/') {
				int candidate = state[i-1][k-1].this_score + ADJACENT;
				if (candidate > this_score)
					this_score = candidate, amount = ADJACENT;
			}
			state[i][k].this_score = this_score;
			state[i][k].amount = amount;
			state[i][k].score = max(state[i-1][k].score, this_score);
		}
	}

	score = state[string_length-1][pat_length-1].score;

	dprintf("score: %d\n", score);

	if (!match || score < 0)
		return score;

	{
		int t = score;
		for (i=pat_length-1; i<(unsigned)-1; i--) {
			for (k=0; k<string_length; k++)
				if (state[k][i].this_score == t)
					break;
			assert(k<string_length);
			match[i] = k;
			t -= state[k][i].amount;
		}
		assert(t == 0);
	}

	return score;
}

int score_simple_string(const char *string, const char *pattern, unsigned *match)
{
	struct scorer_query qry = {.pattern = pattern};
	unsigned string_length = strlen(string);
	return score_string(string, &qry, string_length, match);
}
