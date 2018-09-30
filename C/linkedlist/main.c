#include "intList.h"

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <stdbool.h>

int maptest(int x) {
	return (x > 5)?true:false;
}

int reducetest(int acc, int x) {
	return (acc || x)?true:false;
}

const char *BOUNDARY = "\n----------\n";
int main() {
	intList list = EMPTYLIST;

	for(int i = 0; i < 10; i++) {
		insert(&list, i, i);
	}

	assert(length(list)==10);
	printf("Append 10:\t%d items\n", length(list));

	printf("Sum:\t%d\n", sum(list));
	printf("Product:\t%d\n", product(list));

	insert(&list, 5, 9999);

	assert(get(list, 5) == 9999);
	assert(length(list) == 11);
	printf("Insert[5]:\t%d items\n", length(list));


	printf("\nGetting%s", BOUNDARY);
	for(int i = 0; i < length(list); i++) {
		printf("[%02d]:\t%d\n", i, get(list, i));
	}

	printf("\nSetting%s", BOUNDARY);
	for(int i = 0; i < length(list) +1; i++) {
		int ret = set(list, i, i*20);
		printf("[%d]:\t%d\t%s\n", i, i*20, (ret)?"fail":"success");
	}

	printf("\nRemove[5]:\t%d\n", removeAt(&list, 5));
	assert(get(list, 5)==6*20);
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

	printf("\nGenerated list, length 11, pushed 255\n");
	push(&list, 255);

	int testArr[length(list)];
	toArray(list, testArr);
	printf("Array construction%s", BOUNDARY);
	for(int i = 0; i < length(list); i++) {
		printf("Arr[%02d]:\t%d\n", i, testArr[i]);
	}

	printf("\nMap test%s", BOUNDARY);
	map(list, maptest);
	for(int i = 0; i < length(list); i++) {
		int val = get(list, i);
		printf("[%02d]:\t%d\t%s\n", i, val, (val)?"True":"False");
	}
	
	int red = reduce(list, reducetest, false);
	printf("RedOR:\t%d\t%s", red, (red)?"True":"False");

	destroy(&list);
	printf("\nDestroy:\t%d items remaining\n", length(list));
}
