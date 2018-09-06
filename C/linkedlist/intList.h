#pragma once
#define EMPTYLIST NULL
/* TODO
 * int push(node **headAddr);
 * int set(node *head);
 * int insert(node **headAddr, int index, int x);
 */

struct node {
	int data;
	struct node *next;
};

typedef struct node node;
typedef struct node *intList;

int length(node *head);
int returnSafe(int ret);
int insert(node **headAddr, const unsigned int index, const int x);
int get(node *head, int index);
int pop(node **headAddr);
int destroy(node **headAddr);
