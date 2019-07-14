#include "bTree.h"

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <time.h>
#define ITEMS 15
#define RANGE 50

int main()
{
    srand(time(NULL));
    bTree tree = newTree();

    for (int i = 0; i < ITEMS; ++i)
        insert(&tree, rand() % RANGE);

    assert(tree.size == ITEMS);

    printf("Tree print\n");
    tprint(tree);

    printf("Array sort\n");
    int sorted[tree.size];
    sort(tree, sorted);
    for (int i = 0; i < tree.size; ++i)
        printf("tree[%02d]:\t%d\n", i, sorted[i]);

    destroy(&tree);
}
