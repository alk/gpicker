#ifndef SCORER_H
#define SCORER_H

int score_string(char *string, char *pattern, const unsigned string_length, unsigned* match);
int score_simple_string(char *string, char *pattern, unsigned *match);

#endif
