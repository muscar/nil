#include <stdio.h>

int println()
{
    printf("\n");
    return 0;
}

int print_int(int n)
{
    return printf("%d", n);
}

// struct point
// {
//     int x, y;
// };

// struct point mk_point(int x, int y)
// {
//     struct point result = { .x = x, .y = y };
//     return result;
// }

// extern int fst(struct point p);
// extern int snd(struct point p);

// int c_fst(struct point p)
// {
//     return p.x;
// }

// int c_snd(struct point p)
// {
//     return p.y;
// }

// int c_run()
// {
//     struct point p = { .x = 10, .y = 20 };
//     print_int(c_fst(p));
//     println();
//     print_int(c_snd(p));
//     return 0;
// }

// int run()
// {
//     struct point p;
//     p.x = 10;
//     p.y = 20;
//     print_int(fst(p));
//     println();
//     print_int(snd(p));
//     return 0;
// }