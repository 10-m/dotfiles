#if 0
g++ -g $0 -o a.out && ./a.out; exit;
#endif

#include <iostream>
using namespace std;

#define DBG_PRINT(_ARGS_...) {fprintf(stderr, ## _ARGS_); fflush(stderr);}

int main(int argc, char *argv[])
{
    return 0;
}
