#include "intList.h"
#include <stdlib.h>
#include <stdio.h>

int returnSafe(const int ret) {
	// exists to convert removed data to new, automatic memory
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

int get(node *head, const unsigned int index) {
	if(head == EMPTYLIST) {
		fprintf(stderr, "IndexOutOfBounds\n");
		exit(-1);
	}
	node * current = head;
	for(unsigned int i = 0; i < index; i++) {
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

int push(node **headAddr, const int x) {
	return insert(headAddr, 0, x);
}

int removeAt(node **headAddr, const unsigned int index) {
	if(index == 0) return pop(headAddr);

	int ret;

	node **linker = headAddr;
	unsigned int i = 0;
	while(*linker != NULL && i < index-1) {
		linker=&(*linker)->next;
		i++;
	}

	if(i != index-1) {
		fprintf(stderr, "IndexOutOfBounds\n");
		exit(-1);
	}

	node *tmp = (*linker)->next;
	ret = returnSafe(tmp->data);

	(*linker)->next = tmp->next;
	free(tmp);

	return ret;
}

int set(node *head, const unsigned int index, const int x) {
	unsigned int i = 0;
	node *current = head;
	while(current != NULL && i < index) {
		current = current->next;
		i++;
	}

	if(current == NULL)
		return 1;

	current->data = x;
	return 0;
}

int reduce(node *head, R_agent f, int init) {
	int ret = init;
	node* current = head;
	while(current != NULL) {
		ret = (*f)(init, current->data);
		current = current->next;
	}
	return ret;
}

void map(node *head, M_agent f) {
	node *current = head;
	while(current != NULL) {
		current->data = (*f)(current->data);
		current = current->next;
	}
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

int toArray(node *head, int arr[]) {
	node *current = head;
	for(int i = 0; i < length(head); i++) {
		if(current == NULL)
			return 1;
		arr[i] = current->data;
		current = current->next;
	}
	return 0;
}
