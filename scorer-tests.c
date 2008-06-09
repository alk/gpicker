#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <check.h>

#include "scorer.h"

static
void	_assert_scores(char *file, int line, int expected_score, char *string, char *pattern, int *expected_match)
{
	int pat_len = strlen(pattern);
	int real_score;
	int i;
	unsigned match[pat_len];
	real_score = score_simple_string(string, pattern, match);
	if (real_score != expected_score) {
		_fail_unless(0, file, line, 0, "scoring %s against pattern %s should score %d, not %d",
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
	fail_unless(1, __FILE__, __LINE__, "cannot happen", 0);
}

#define assert_scores(score, string, pattern, match) _assert_scores(__FILE__, __LINE__, (score), (string), (pattern), (match))
#define M(...) ((unsigned[]){__VA_ARGS__})

START_TEST(test_capital_pattern_chars_work)
{
	assert_scores(1002, "flexi_record", "FR", M(0,6));
}
END_TEST

START_TEST(test_char_after_delimiter_handling)
{
	/* char right after delimiter must match start of word */
	assert_scores(1001501, "some_owo-word", "s-wo", M(0,4,9,10));
//	assert_scores(1001501, "some_owoWord", "s-wo", M(0,4,9,10));
}
END_TEST

START_TEST(test_single_char_matches_work)
{
	assert_scores(0, "asd", "s", M(1));
	assert_scores(0, "asd", "d", M(2));
	assert_scores(501,"asd", "a", 0);
	assert_scores(501, "asd", "ad", M(0,2));
	assert_scores(-1, "asd", "q", 0);
	assert_scores(-1, "asd", "ds", 0);
	assert_scores(1000, "asd", "sd", M(1,2));
	assert_scores(501, "asdaaas", "a", M(0));
}
END_TEST

START_TEST(test_slash_handling)
{
	/* 'ar' in pattern should not match chars with '/' between them */
//	assert_scores(1000502, "activerecord/lib/active_record/base.rb", "ar/b", M(17,24,30,31));
	/* '/' works */
	assert_scores(1000502, "activerecord/lib/octive_record/base.rb", "ar/b", M(0,24,30,31));
}
END_TEST

Suite *scorer_suite (void)
{
	Suite *s = suite_create ("Scorer");

#define T(name) do { TCase *tc = tcase_create(#name); tcase_add_test(tc, name); suite_add_tcase(s, tc);} while (0)

	T(test_char_after_delimiter_handling);
	T(test_capital_pattern_chars_work);
	T(test_single_char_matches_work);
	T(test_slash_handling);

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
