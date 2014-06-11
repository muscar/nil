#include <stdio.h>

extern double kl_main();

double putchard(double d)
{
    putchar((char) d);
    return 0;
}

int main(int argc, const char *argv[])
{
    printf("kaleidoscope driver v0.1\n");
    kl_main();
    return 0;
}
