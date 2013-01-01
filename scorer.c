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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>
#include <inttypes.h>

#include "scorer.h"
#include "compiler.h"

int scorer_utf8_mode = 1;

#define PROPER_WORD_START 0x100000
#define WILD_WORD_START 0x201
#define ADJACENT 0x400

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

static
int delimiter_p_full(char ch)
{
	return (ch == '.' || ch == '_' || ch == '/' || ch == '*' || ch == ' ' || ch == '\'' || ch == '"');
}

static
char delimiter_table[256];

static
char tolower_table[256];

void prepare_scorer(void)
{
	int i;
	for (i = 0; i < 256; i++) {
		char delimiter = delimiter_p_full(i);
		delimiter_table[i] = delimiter;
		tolower_table[i] = tolower(i);
		if (i == '-')
			tolower_table[i] = '_';
	}
}

static inline
int delimiter_p(char ch)
{
	return delimiter_table[ch];
}

static inline
char normalize_char(char ch, unsigned *is_delimiter)
{
	ch = tolower_table[ch];
	if (is_delimiter)
		*is_delimiter = delimiter_p(ch);
	return ch;
}

#define MAX_PAT_LENGTH 32

static
void initialize_prepared_pattern(const char *pattern,
				 struct prepared_pattern *rv)
{
	unsigned previous_delimiter = 1;
	int i;

	for (i=0; i<rv->pat_length; i++) {
		char ch = pattern[i];
		rv->start_of_pattern_word[i] = previous_delimiter || ('A' <= ch && ch <= 'Z');
		rv->translated_pattern[i] = normalize_char(ch, &previous_delimiter);
	}

	char fc_count = 0;
	for (i = 0; i < rv->pat_length && fc_count < 4; i++) {
		char ch = rv->translated_pattern[i];
		if (!delimiter_p(ch)) {
			rv->first_chars[fc_count*2] = ch;
			rv->first_chars[fc_count*2+1] = toupper(ch);
			fc_count++;
		}
	}
	rv->fc_count = fc_count;
}

struct prepared_pattern *prepare_pattern(const struct scorer_query *query)
{
	const char *pattern = query->pattern;
	unsigned pat_length = strlen(pattern);

	if (pat_length > MAX_PAT_LENGTH)
		pat_length = MAX_PAT_LENGTH;

	char *start_of_pattern_word = pat_length ? malloc(pat_length) : 0;
	char *translated_pattern = pat_length ? malloc(pat_length) : 0;

	struct prepared_pattern *rv = malloc(sizeof(struct prepared_pattern));
	rv->translated_pattern = translated_pattern;
	rv->start_of_pattern_word = start_of_pattern_word;
	rv->pat_length = pat_length;

	if (pat_length)
		initialize_prepared_pattern(pattern, rv);
	return rv;
}

void free_prepared_pattern(struct prepared_pattern *p)
{
	if (!p)
		return;
	free(p->start_of_pattern_word);
	free(p->translated_pattern);
	free(p);
}

static inline __attribute__((always_inline))
int score_string_prepared_inline(const unsigned pat_length,
				 const char *string,
				 const struct scorer_query *query,
				 const struct prepared_pattern *prepared_pattern,
				 const unsigned string_length,
				 unsigned* match)
{
	struct position {
		int this_score;
		int score;
		int amount;
	};

	struct position state[string_length][__BUILTIN_CONSTANT(pat_length) ? pat_length : MAX_PAT_LENGTH];
	char *start_of_pattern_word = prepared_pattern->start_of_pattern_word;
	char *translated_pattern = prepared_pattern->translated_pattern;
	unsigned i;
	unsigned k;
	unsigned previous_delimiter;
	int score;

	if (pat_length == 0)
		return 0;

	if (string_length == 0)
		return -1;

	dprintf("scoring string '%.*s' for pattern '%s'\n", string_length, string, query->pattern);

#if 1
	if (prepared_pattern->fc_count > 0) {
		const char *p = string;
		char ch;

#define I(i) \
		do { \
			ch = prepared_pattern->first_chars[i*2];	\
			if (ch) {					\
				const char *next = memchr(p, ch, string + string_length - p); \
				const char *next2 = memchr(p, prepared_pattern->first_chars[i*2+1], string + string_length - p); \
				if (!next && !next2)			\
					return -1;			\
				if (!next)				\
					next = next2;			\
				else if (!next2)			\
				next2 = next;				\
				p = ((next < next2) ? next : next2) + 1; \
			} \
		} while (0)

		I(0);

		if (prepared_pattern->fc_count > 1) {
			I(1);
			if (prepared_pattern->fc_count > 2) {
				I(2);
				if (prepared_pattern->fc_count > 3)
					I(3);
			}
		}

#undef I
	}
#endif

	memset(state, -1, sizeof(state));

	previous_delimiter = 1;

	for (i=0; i<string_length; i++) {
		char ch = string[i];
		unsigned max_k;
		unsigned at_word_start = previous_delimiter || ('A' <= ch && ch <= 'Z');
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
		if (!__BUILTIN_CONSTANT(pat_length) || pat_length > 1) {
			max_k = i+1;
			max_k = (max_k > pat_length) ? pat_length : max_k;
			for (k=1; k<max_k; k++) {
				char pat_ch = translated_pattern[k];
				int prev_score;
				char prev_pattern;
				int amount;
				int this_score;
				prev_score = state[i-1][k-1].score;
				if (pat_ch == '_') { // always match '_'
					state[i][k].score = prev_score;
					state[i][k].amount = 0;
					state[i][k].this_score = prev_score;
					continue;
				}
				if (ch != pat_ch) {
				cont:
					state[i][k].score = state[i-1][k].score;
					state[i][k].amount = 0;
					state[i][k].this_score = -1;
					continue;
				}
				if (prev_score < 0)
					goto cont;
				amount = 0;
				prev_pattern = translated_pattern[k-1];
				if (at_word_start)
					amount = start_of_pattern_word[k] ? PROPER_WORD_START : WILD_WORD_START;
				// '_' demands proper word start after itself, and nothing less
				if (prev_pattern == '_')
					if (amount != PROPER_WORD_START)
						goto cont;
				this_score = prev_score + amount;

				// if current pattern byte is
				// continuation of utf8 sequence, then
				// it must match adjacently.
				if (__EXPECT(scorer_utf8_mode, 1) && __EXPECT(utf8_continuation_p(pat_ch), 0)) {
					if (state[i-1][k-1].this_score < 0)
						goto cont;
					this_score = 0; // force win
							// of next candidate
				}
				if (state[i-1][k-1].this_score >= 0 && !delimiter_p(pat_ch)) {
					int candidate = state[i-1][k-1].this_score + ADJACENT;
					if (candidate > this_score)
						this_score = candidate, amount = ADJACENT;
				}
				state[i][k].this_score = this_score;
				state[i][k].amount = amount;
				state[i][k].score = max(state[i-1][k].score, this_score);
			}
		}
	}

	score = state[string_length-1][pat_length-1].score;

	dprintf("score: %d\n", score);

	if (!match || score < 0)
		return score;

	{
		int t = score;
		k = string_length;
		for (i=pat_length-1; i<(unsigned)-1; i--) {
			if (query->right_match) {
				for (k=k-1; k>=i; k--)
					if (state[k][i].this_score == t)
						break;
				assert(k>=i);
			} else {
				int last_k = k;
				for (k=0; k<last_k; k++)
					if (state[k][i].this_score == t)
						break;
				assert(k<string_length);
			}
		match_again:
			match[i] = k;
			if (translated_pattern[i] == '_')
				match[i] = SCORER_MATCH_NONE;
			if (__EXPECT(scorer_utf8_mode, 1)
			    && __EXPECT(utf8_continuation_p(translated_pattern[i]), 0)) {
				match[i] = SCORER_MATCH_NONE;
			}
			t -= state[k][i].amount;
			if (state[k][i].amount == ADJACENT) {
				assert(i>0);
				k--;
				i--;
				goto match_again;
			}
		}
		assert(t == 0);
	}

	return score;
}


/* partial compilation for pat_length == 1 */
static __attribute__((noinline))
int score_string_prepared_1(const char *string,
			    const struct scorer_query *query,
			    const struct prepared_pattern *prepared_pattern,
			    const unsigned string_length,
			    unsigned* match)
{
	return score_string_prepared_inline(1, string, query, prepared_pattern, string_length, match);
}

/* partial compilation for pat_length == 2 */
static __attribute__((noinline))
int score_string_prepared_2(const char *string,
			    const struct scorer_query *query,
			    const struct prepared_pattern *prepared_pattern,
			    const unsigned string_length,
			    unsigned* match)
{
	return score_string_prepared_inline(2, string, query, prepared_pattern, string_length, match);
}

int score_string_prepared(const char *string,
			  const struct scorer_query *query,
			  const struct prepared_pattern *prepared_pattern,
			  const unsigned string_length,
			  unsigned* match)
{
	if (!prepared_pattern)
		return -1;

	if (prepared_pattern->pat_length == 1) {
		return score_string_prepared_1(string, query, prepared_pattern, string_length, match);
	}

	if (prepared_pattern->pat_length == 2) {
		return score_string_prepared_2(string, query, prepared_pattern, string_length, match);
	}

	return score_string_prepared_inline(prepared_pattern->pat_length, string, query, prepared_pattern, string_length, match);
}

int score_string(const char *string,
		 const struct scorer_query *query,
		 const unsigned string_length,
		 unsigned* match)
{
	const char *pattern = query->pattern;
	unsigned pat_length = strlen(pattern);
	char start_of_pattern_word[pat_length];
	char translated_pattern[pat_length];

	if (pat_length == 0)
		return 0;

	if (string_length == 0)
		return -1;

	if (pat_length > MAX_PAT_LENGTH)
		pat_length = MAX_PAT_LENGTH;


	struct prepared_pattern p = {
		.translated_pattern = translated_pattern,
		.start_of_pattern_word = start_of_pattern_word,
		.pat_length = pat_length
	};

	if (pat_length)
		initialize_prepared_pattern(pattern, &p);

	return score_string_prepared(string, query, &p, string_length, match);
}

int score_simple_string(const char *string, const char *pattern, unsigned *match)
{
	struct scorer_query qry = {.pattern = pattern,.right_match = 0};
	unsigned string_length = strlen(string);
	return score_string(string, &qry, string_length, match);
}
