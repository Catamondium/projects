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

	printf("Insert@5:\t%d items\n", length(list));


	printf("\nGetting%s", BOUNDARY);
	for(int i = 0; i < length(list); i++) {
		printf("%d:\t%d\n", i, get(list, i));
	}

	printf("\nPopping%s", BOUNDARY); // only 5 popped?
	for(int i = 0; length(list) != 0; i++) { // changing alongside us?
		printf("%d:\t%d\n", i, pop(&list));
	}

	printf("\nemptyList:\t%d items\n", length(list));

	for(int i = 0; i < 10; i++) {
		insert(&list, i, i);
	}

	destroy(&list);
	printf("\nDelete:\t%d items remaining\n", length(list));
}
