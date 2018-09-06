#include "intList.h"

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

const char *BOUNDARY = "\n----------\n";
int main() {
	intList list = EMPTYLIST;

	for(int i = 0; i < 10; i++) {
		add(&list, i);
	}

	printf("Append 10:\t%d items\n", length(list));

	printf("\nGetting%s", BOUNDARY);
	for(int i = 0; i < 10; i++) {
		printf("%d:\t%d\n", i, get(list, i));
	}

	printf("\nPopping%s", BOUNDARY);
	for(int i = 0; i < 10; i++) { // Good place to test length
		int y = pop(&list);
		printf("%d:\t%d\n", i, y);
	}

	printf("\nemptyList:\t%d items\n", length(list));

	for(int i = 0; i < 10; i++) {
		add(&list, i);
	}

	destroy(&list);
	printf("\nDelete:\t%d items remaining\n", length(list));
}
