#ifndef COMPILER_H
#define COMPILER_H

#ifdef __GNUC__
#define __EXPECT(a,b) __builtin_expect(a,b)
#define __BUILTIN_CONSTANT(a) __builtin_constant_p(a)
#else
#define __EXPECT(a,b) (a)
#define __BUILTIN_CONSTANT(a) 0
#endif

#endif
