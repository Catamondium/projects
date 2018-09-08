#include "intList.h"

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

const char *BOUNDARY = "\n----------\n";
int main() {
	intList list = EMPTYLIST;

	for(int i = 0; i < 10; i++) {
		insert(&list, i, i);
	}

	assert(length(list)==10);
	printf("Append 10:\t%d items\n", length(list));

	insert(&list, 5, 9999);

	assert(get(list, 5) == 9999);
	assert(length(list) == 11);
	printf("Insert[5]:\t%d items\n", length(list));


	printf("\nGetting%s", BOUNDARY);
	for(int i = 0; i < length(list); i++) {
		printf("[%02d]:\t%d\n", i, get(list, i));
	}

	printf("\nRemove[5]:\t%d\n", removeAt(&list, 5));
	assert(get(list, 5)==5);
	assert(length(list)==10);
	printf("\nRemoveLast:\t%d\n", removeAt(&list, length(list)-1));

	printf("Popping%s", BOUNDARY);
	for(int i = 0; length(list) != 0; i++) {
		printf("[%02d]:\t%d\n", i, pop(&list));
	}

	printf("\nemptyList:\t%d items\n", length(list));

	for(int i = 0; i < 10; i++) {
		insert(&list, i, i);
	}

	printf("\nGenerated list, length 10\n");

	destroy(&list);
	printf("Destroy:\t%d items remaining\n", length(list));
}
