#if 0
(splint -exportlocal $0 && echo "-- Splint OK --") || echo "-- Splint NG --"
g++ -g $0 -o a.out && ./a.out; exit;
#endif

#include <stdio.h>

#define DBG_PRINT(_ARGS_...) {fprintf(stderr, ## _ARGS_); fflush(stderr);}

int main(int argc, char *argv[])
{
    return 0;
}
