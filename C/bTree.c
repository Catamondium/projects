#include <stdio.h>
#include <stdlib.h>

typedef struct node {
	int data;
	struct node *lesser;
	struct node *greater;
} node;

node * makenode(int i) {
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
		*n = makenode(i);
	else if((*n)->data < i) {
		if((*n)->lesser == NULL)
			(*n)->lesser = makenode(i);
		else
			add(&((*n)->lesser), i);
	} else {
		if((*n)->greater == NULL)
			(*n)->greater = makenode(i);
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

void pvisit(node *n) {
	if(n->lesser != NULL)
		pvisit(n->lesser);

	printf("%d\n", n->data);

	if(n->greater != NULL)
		pvisit(n->greater);
}

//TODO
//void visit(node *n) {
//	if(n->lesser != NULL)
//		visit(n->lesser);
//
//	printf("%d\n", n->data);
//
//	if(n->greater != NULL)
//		visit(n->greater);
//}

typedef struct bTree {
	node *root;
	int size;
} bTree;

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

void tprint(bTree tree) {
	if(tree.root != NULL)
		pvisit(tree.root);
}

// TODO
//void sort(bTree tree) {
//	if(tree.root != NULL)
//		visit(tree.root);
//}

int main(int argc, char **argv) {
	bTree tree = newTree();

	for(int i = 0; i < 50; i++) {
		insert(&tree, rand() % atoi(argv[1]));
	}

	tprint(tree);
	destroy(&tree);
}
