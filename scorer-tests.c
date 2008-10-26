/*
 * Copyright (C) 2008 Aliaksey Kandratsenka
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
#include <memory.h>
#include <check.h>

#include "scorer.h"

static
struct scorer_query qry;

static
void	_assert_scores(char *file, int line, int expected_score, char *string, char *pattern, unsigned *expected_match)
{
	int pat_len = strlen(pattern);
	int real_score;
	int i;
	unsigned match[pat_len];

	qry.pattern = pattern;
	real_score = score_string(string, &qry, strlen(string), match);
	if (real_score != expected_score) {
		_fail_unless(0, file, line, 0, "scoring %s against pattern %s should score 0x%x, not 0x%x",
			     string, pattern, expected_score, real_score,
			     0);
		return;
	}
	if (expected_match)
		for (i=0;i<pat_len;i++)
			if (expected_match[i] != match[i])
				_fail_unless(0, file, line, 0,
					     "match differs in pos %d. Expected %d, got %d",
					     i, expected_match[i], match[i], 0);
	_fail_unless(1, __FILE__, __LINE__, "cannot happen", 0);
}

#define assert_scores(score, string, pattern, match) _assert_scores(__FILE__, __LINE__, (score), (string), (pattern), (match))
#define M(...) ((unsigned[]){__VA_ARGS__})

START_TEST(test_empty_string)
{
	int score;

	qry.pattern = "a";
	score = score_string("", &qry, 0, 0);

	fail_unless(score == -1, NULL);
}
END_TEST

START_TEST(test_capital_pattern_chars_work)
{
	assert_scores(0x200000, "flexi_record", "FR", M(0,6));
}
END_TEST

START_TEST(test_char_after_delimiter_handling)
{
	/* char right after delimiter must match start of word */
	assert_scores(0x200400, "some_owo-word", "s-wo", M(0,4,9,10));
	assert_scores(0x200400, "some_owoWord", "s-wo", M(0,4,8,9));
}
END_TEST

START_TEST(test_single_char_matches_work)
{
	assert_scores(0, "asd", "s", M(1));
	assert_scores(0, "asd", "d", M(2));
	assert_scores(0x100000,"asd", "a", 0);
	assert_scores(0x100000, "asd", "ad", M(0,2));
	assert_scores(-1, "asd", "q", 0);
	assert_scores(-1, "asd", "ds", 0);
	assert_scores(0x400, "asd", "sd", M(1,2));
	assert_scores(0x100000, "asdaaas", "a", M(0));
}
END_TEST

START_TEST(test_slash_handling)
{
	qry.right_match = 1;
	assert_scores(0x100201, "activerecord/lib/active_record", "ar", M(17,24));
	qry.right_match = 0;
	assert_scores(0x100000, "base.rb", "b", M(0));
	/* '/' works */
	assert_scores(0x200201, "activerecord/lib/octive_record/base.rb", "ar/b", M(0,24,30,31));
	qry.right_match = 1;
	assert_scores(0x200201, "activerecord/lib/octive_record/base.rb", "ar/b", M(0,24,30,31));
	qry.right_match = 0;
}
END_TEST

START_TEST(test_right_match)
{
	qry.right_match = 1;

	assert_scores(0, "sabababa", "a", M(7));
	assert_scores(0, "sabababa", "aa", M(5,7));

	assert_scores(0, "sabababa", "b", M(6));
	assert_scores(0, "sabababa", "bb", M(4,6));

	qry.right_match = 0;

	assert_scores(0, "sabababa", "a", M(1));
	assert_scores(0, "sabababa", "aa", M(1,3));

	assert_scores(0, "sabababa", "b", M(2));
	assert_scores(0, "sabababa", "bb", M(2,4));
}
END_TEST

static
void setup(void)
{
	qry.right_match = 0;
}

Suite *scorer_suite (void)
{
	Suite *s = suite_create ("Scorer");

#define T(name)							\
	do {							\
		TCase *tc = tcase_create(#name);		\
		tcase_add_test(tc, name);			\
		tcase_add_checked_fixture(tc, setup, 0);	\
		suite_add_tcase(s, tc);				\
	} while (0)

	T(test_char_after_delimiter_handling);
	T(test_capital_pattern_chars_work);
	T(test_single_char_matches_work);
	T(test_slash_handling);
	T(test_right_match);
	T(test_empty_string);

	return s;
#undef T
}

int main(void)
{
	int number_failed;
	Suite *s = scorer_suite();
	SRunner *sr = srunner_create(s);

	srunner_run_all(sr, CK_NORMAL);

	number_failed = srunner_ntests_failed(sr);
	srunner_free (sr);

	return (number_failed == 0) ? EXIT_SUCCESS : EXIT_FAILURE;
}
