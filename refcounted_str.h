#ifndef REFCOUNTED_STR_H
#define REFCOUNTED_STR_H

// NOTE: this is not for performance critical code

struct refcounted_str {
	int refcnt;
	char *str;
};

struct refcounted_str *refcounted_str_dup(char *str);
void refcounted_str_get(struct refcounted_str **ptr, struct refcounted_str *src);
void refcounted_str_put(struct refcounted_str **ptr);

#endif
