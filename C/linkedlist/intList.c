#include "intList.h"
#include <stdlib.h>
#include <stdio.h>

int length(node *head) {
	node *current = head;
	int ret = 0;
	while(current != NULL) {
		ret++;
		current = current->next;
	}
	return ret;
}

int returnSafe(int ret) {
	return ret;
}

int insert(node **headAddr, const unsigned int index, const int x) {
	node *newElem = (node*)malloc(sizeof(node));
	if(newElem == NULL) {
		fprintf(stderr, "Unable to allocate new element.");
		exit(-1);
	}
	newElem->data = x;

	node **current = headAddr;
	unsigned int i = 0;
	while (*current != NULL && i < index) {
		current=&(*current)->next;
		i++;
	}

	newElem->next = *current;
	*current = newElem;

	return 0;
}

int get(node *head, int index) {
	if(head == EMPTYLIST) {
		fprintf(stderr, "Index OutOfBounds\n");
		exit(-1);
	}
	node * current = head;
	for(int i = 0; i < index; i++) {
		if(current->next == NULL) {
			fprintf(stderr, "Index OutOfBounds\n");
			exit(-1);
		}
		
		current = current->next;
	}
	return current->data;
}

int pop(node **headAddr) {
	int ret;

	if(*headAddr != EMPTYLIST) {
		ret = returnSafe((*headAddr)->data);
		node *tmp = (*headAddr)->next;
		free(*headAddr);
		*headAddr = tmp;
	}
	return ret;
}

int destroy(node **headAddr) {
	node *current = *headAddr;
	node *next;
	while(current != NULL) {
		next = current->next;
		free(current);
		current = next;
	}
	*headAddr = NULL;
	return 0;
}
