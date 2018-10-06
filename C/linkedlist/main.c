#include "intList.h"

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <stdbool.h>

void printList(intList list) {
	for(int i = 0; i < length(list); i++) {
		printf("[%02d]:\t%d\n", i, get(list, i));
	}
}

int mapfunc(int x) {
	return (x > 5)?true:false;
}

int reduceOR(int acc, int x) {
	return (acc || x)?true:false;
}

int reduceAND(int acc, int x) {
	return (acc && x)?true:false;
}

const char *BOUNDARY = "\n----------\n";
int main() {
	intList list = EMPTYLIST;

	for(int i = 0; i < 10; i++) {
		insert(&list, -1, i);
	}

	assert(length(list)==10);
	printf("Append 10%s", BOUNDARY);
	printList(list);

	printf("\nSum:\t%d\n", sum(list));
	printf("Product:\t%d\n", product(list));

	insert(&list, 5, 9999);

	assert(get(list, 5) == 9999);
	assert(length(list) == 11);
	printf("Insert[5]:\t%d items\n", length(list));


	printf("\ngetting%s", BOUNDARY);
	printList(list);

	printf("\nsetting%s", BOUNDARY);
	for(int i = 0; i < length(list); i++) {
		set(list, i, i*20);
		printf("[%d]:\t%d\n", i, i*20);
	}

	printf("\nremove[5]:\t%d\n", removeAt(&list, 5));
	assert(get(list, 5)==6*20);
	assert(length(list)==10);
	printf("removelast:\t%d\n", removeAt(&list, length(list)-1));

	printf("\npopping%s", BOUNDARY);
	for(int i = 0; length(list) != 0; i++) {
		printf("[%02d]:\t%d\n", i, pop(&list));
	}

	printf("\nemptylist:\t%d items\n", length(list));

	for(int i = 0; i < 10; i++) {
		insert(&list, i, i);
	}

	printf("generated list, length 11, pushed 255\n");
	push(&list, 255);

	int testarr[length(list)];
	toArray(list, testarr);
	printf("\narray construction%s", BOUNDARY);
	for(int i = 0; i < length(list); i++) {
		printf("arr[%02d]:\t%d\n", i, testarr[i]);
	}

	intList fromArr = fromArray(testarr, length(list));
	printf("\nfromArray construction%s", BOUNDARY);
	printList(fromArr);

	printf("\ncopy construction%s", BOUNDARY);
	intList clone = copy(fromArr);
	printList(clone);

	printf("\nMapping:\t(x > 5)%s", BOUNDARY);
	map(list, mapfunc);
	for(int i = 0; i < length(list); i++) {
		int val = get(list, i);
		printf("[%02d]:\t%d\t%s\n", i, val, (val)?"true":"false");
	}
	
	int red = reduce(list, reduceOR, false);
	assert(red == true);
	printf("\nRedOR:\t%d\t%s\n", red, (red)?"true":"false");
	red = reduce(list, reduceAND, false);
	assert(red == false);
	printf("RedAND:\t%d\t%s\n", red, (red)?"true":"false");


	destroy(&list);
	printf("\nDestroy:\t%d items remaining\n", length(list));
}
