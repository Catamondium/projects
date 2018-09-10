#include "intList.h"
#include <stdlib.h>
#include <stdio.h>

int returnSafe(const int ret) {
	return ret;
}

int length(node *head) {
	node *current = head;
	int ret = 0;
	while(current != NULL) {
		ret++;
		current = current->next;
	}
	return ret;
}

int sum(node *head) {
	int ret = 0;
	node *current = head;
	while(current != NULL) {
		ret += current->data;
		current = current->next;
	}
	return ret;
}

int product(node *head) {
	int ret = 1;
	node *current = head;
	while(current != NULL && ret != 0) {
		ret *= head->data;
		current = current->next;
	}
	return ret;
}

int insert(node **headAddr, const unsigned int index, const int x) {
	node **current = headAddr;
	unsigned int i = 0;
	while (*current != NULL && i < index) {
		current=&(*current)->next;
		i++;
	}

	if(i != index) {
		fprintf(stderr, "IndexOutofBounds\n");
		exit(-1);
	}

	node *newElem = (node*)malloc(sizeof(node));
	if(newElem == NULL) {
		fprintf(stderr, "Unable to allocate new element\n");
		exit(-1);
	}

	newElem->data = x;
	newElem->next = *current;
	*current = newElem;

	return 0;
}

int get(node *head, const int index) {
	if(head == EMPTYLIST) {
		fprintf(stderr, "Index OutOfBounds\n");
		exit(-1);
	}
	node * current = head;
	for(int i = 0; i < index; i++) {
		if(current->next == NULL) {
			fprintf(stderr, "IndexOutOfBounds\n");
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
	} else {
		fprintf(stderr, "Popped from empty list");
		exit(-1);
	}
	
	return ret;
}


int removeAt(node **headAddr, const int index) {
	if(index == 0)
		return pop(headAddr);

	int ret;

	node **current = headAddr;
	int i = 0;
	while(*current != NULL && i < index-1) {
		current=&(*current)->next;
		i++;
	}

	if(i != index-1) {
		fprintf(stderr, "IndexOutOfBounds\n");
		exit(-1);
	}

	node *tmp = (*current)->next;
	ret = returnSafe(tmp->data);

	(*current)->next = (*current)->next->next;
	free(tmp);

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
