#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char *version()
{
    char *s = (char *) malloc(5);
    strcpy(s, "v0.1");
    return s;
}

int println()
{
    printf("\n");
    return 0;
}

int print_int16(int n)
{
    return printf("%d", n);
}

int print_int32(int n)
{
    return printf("%d", n);
}

int print_string(char *s)
{
    return printf("%s", s);
}

