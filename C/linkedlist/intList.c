#include "intList.h"
#include <stdlib.h>
#include <stdio.h>

int returnSafe(const int ret) {
	// exists to convert removed data to new, automatic memory
	return ret;
}

int add(int a, int b) {
	return a + b;
}

int mul(int a, int b) {
	return a * b;
}

int stdIndex(node *head, signed int index) {
	// convert to from-head index form
	
	unsigned int len = length(head);
	if(index >= 0)
		return index;

	else if(len == 0)
		return 0;

	else
		return len-1; // append for all -ve indices
}

node * travP1(node *head, signed int index) {
	// traverse by element
	index = stdIndex(head, index);

	int i = 0;
	node *current = head;
	while(i < index && current != NULL) {
		current = current->next;
		i++;
	}

	if(current == NULL) {
		fprintf(stderr, "IndexOutofBounds\n");
		exit(-1);
	}

	return current;
}

node ** travP2(node **headAddr, signed int index) {
	// traverse by &(*current)->next
	index = stdIndex(*headAddr, index);

	int i = 0;
	node **current = headAddr;
	while (i < index && *current != NULL) {
		current=&(*current)->next;
		i++;
	}
	
	if(i != index) {
		fprintf(stderr, "IndexOutofBounds\n");
		exit(-1);
	}

	return current;
}

node * makenode(const int x) {
	node *newElem = (node*)malloc(sizeof(node));
	if(newElem == NULL) {
		fprintf(stderr, "Unable to allocate new element\n");
		exit(-1);
	}

	newElem->data = x;
	newElem->next = NULL;

	return newElem;
}

// miscellaneous operations
unsigned int length(node *head) {
	node *current = head;
	int ret = 0;
	while(current != NULL) {
		ret++;
		current = current->next;
	}
	return ret;
}

int sum(node *head) {
	return reduce(head, add, 0);
}

int product(node *head) {
	return reduce(head, mul, 1);
}

// stack operations
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

// indexed operations
int insert(node **headAddr, const signed int index, const int x) {
	node **current = travP2(headAddr, index);

	node *newElem = makenode(x);
	newElem->next = *current;
	*current = newElem;

	return 0;
}

int get(node *head, const signed int index) {
	return travP1(head, index)->data;
}

void set(node *head, const signed int index, const int x) {
	travP1(head, index)->data = x;
}

int removeAt(node **headAddr, const signed int index) {
	if(index == 0) return pop(headAddr);

	int ret;

	node **linker = travP2(headAddr, index-1);

	node *tmp = (*linker)->next;
	ret = returnSafe(tmp->data);

	(*linker)->next = tmp->next;
	free(tmp);

	return ret;
}

// structure-wide operations
int reduce(node *head, R_agent f, const int init) {
	int ret = init;
	node *current = head;
	while(current != NULL) {
		ret = (*f)(ret, current->data);
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

// constructors/destructors
int toArray(node *head, int arr[]) {
	node *current = head;
	for(unsigned int i = 0; i < length(head); i++) {
		if(current == NULL)
			return 1;
		arr[i] = current->data;
		current = current->next;
	}
	return 0;
}

node * fromArray(int arr[], const unsigned int arrsize) {
	node *head = NULL;
	
	node **linker = NULL;
	for(unsigned int i = 0; i < arrsize; i++) {
		node *newElem = makenode(arr[i]);

		if(i == 0) { // bind heads
			head = newElem;
			linker = &head;
		} else { // bind to tailend

		linker = &(*linker)->next;
		*linker = newElem;
		}
	}
	return head;
}

node * copy(node *h_origin) {
	int init = 1;

	node *h_new = NULL;
	
	node **linker = NULL;
	node *current = h_origin;
	while(current != NULL) {
		node *newElem = makenode(current->data);

		if(init) { // bind h_news
			h_new = newElem;
			linker = &h_new;

			init = 0;
		} else { // bind to tailend

		linker = &(*linker)->next;
		*linker = newElem;
		}

		current = current->next;
	}
	return h_new;
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
