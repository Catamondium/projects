#include <stdio.h>
#include <stdlib.h>

int fibonacci(int n)
{
    if (n == 0)
    {
        return 0;
    }
    if (n == 1)
    {
        return 1;
    }
    else
    {
        int first = 0;
        int second = 1;
        int result;
        for (int i = 1; i < n; i++)
        {
            result = first + second;
            first = second;
            second = result;
        }
        return result;
    }
}

int main(int argc, char *argv[])
{
    if (argc == 1)
    {
        printf("Error, integer argument required.\n");
    }
    else
    {
        int n = atoi(argv[1]);
        printf("%d\n", fibonacci(n));
    }
}
