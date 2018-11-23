#include "bTree.h"
#include <stdio.h>
#include <stdlib.h>

// NODE section

node * newNode(int i) {
	node *newElem = (node*)malloc(sizeof(node));

	if(newElem == NULL) {
		fprintf(stderr, "Failed to allocate new element");
		exit(-1);
	}

	newElem->data = i;
	newElem->lesser = NULL;
	newElem->greater = NULL;

	return newElem;
}

void add(node **n, int i) {
	if(*n == NULL)
		*n = newNode(i);

	else if((*n)->data < i) {
		if((*n)->lesser == NULL)
			(*n)->lesser = newNode(i);
		else
			add(&((*n)->lesser), i);

	} else {
		if((*n)->greater == NULL)
			(*n)->greater = newNode(i);
		else
			add(&((*n)->greater), i);
	}
}

void rm(node *n) {
	if(n != NULL) {
		if(n->lesser != NULL)
			rm(n->lesser);
		if(n->greater != NULL)
			rm(n->greater);

		free(n);
	}
}

void visit(node *n, int *arr, int *index) {
	if(n->lesser != NULL)
		visit(n->lesser, arr, index);

	arr[*index] = n->data;
	(*index)++;

	if(n->greater != NULL)
		visit(n->greater, arr, index);
}

void pvisit(node *n) {
	if(n->lesser != NULL)
		pvisit(n->lesser);

	printf("%d\n", n->data);

	if(n->greater != NULL)
		pvisit(n->greater);
}

// TREE section

bTree newTree() {
	bTree ret;
	ret.root = NULL;
	ret.size = 0;
	return ret;
}

void insert(bTree *tree, int i) {
	add(&(tree->root), i);
	tree->size = tree->size + 1;
}

void destroy(bTree *tree) {
	rm(tree->root);
	tree->size = 0;
}

void sort(bTree tree, int *arr) {
	int i = 0;
	if(tree.root != NULL)
		visit(tree.root, arr, &i);
}

void tprint(bTree tree) {
	if(tree.root != NULL)
		pvisit(tree.root);
}

