#include "intList.h"
#include <stdlib.h>
#include <stdio.h>

int length(Node *head) {
	Node *current = head;
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

int add(Node **headAddr, int x) {
	Node *newElem = (Node*)malloc(sizeof(Node));

	if(newElem == NULL) {
		fprintf(stderr, "Unable to allocate new Node\n");
		exit(-1);
	}

	newElem->data = x;
	newElem->next = NULL;

	
	if(*headAddr == EMPTYLIST)
		*headAddr = newElem;

	else { // traverse to last
		// Clearer 'for' approach
		for(Node *current = *headAddr;; current = current->next) {
			if(current->next == NULL) {
			current->next = newElem;
			break;
			}
		}
		// Arguably cleaner 'while' approach
		/*
		Node *current = *headAddr;
		while(1) {
			if(current->next == NULL) {
				current->next = newElem;
				printf("Appended to list %p\n", headAddr);
				break;
			}
			current = current->next;
		}
		*/
	}
	return 0;
}

int get(Node *head, int index) {
	if(head == EMPTYLIST) {
		fprintf(stderr, "Index OutOfBounds\n");
		exit(-1);
	}
	Node * current = head;
	for(int i = 0; i < index; i++) {
		if(current->next == NULL) {
			fprintf(stderr, "Index OutOfBounds\n");
			exit(-1);
		}
		
		current = current->next;
	}
	return current->data;
}

int pop(Node **headAddr) {
	int ret;

	if(*headAddr != EMPTYLIST) {
		ret = returnSafe((*headAddr)->data);
		Node *tmp = (*headAddr)->next;
		free(*headAddr);
		*headAddr = tmp;
	}
	return ret;
}

int destroy(Node **headAddr) {
	Node *current = *headAddr;
	Node *next;
	while(current != NULL) {
		next = current->next;
		free(current);
		current = next;
	}
	*headAddr = NULL;
	return 0;
}
