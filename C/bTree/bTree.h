#pragma once

typedef struct node {
    int data;
    struct node *lesser;
    struct node *greater;
} node;

typedef struct bTree {
    node *root;
    int size;
} bTree;


bTree newTree();
void insert(bTree * tree, int i);
void destroy(bTree * tree);
void tprint(bTree tree);
void sort(bTree tree, int *arr);
