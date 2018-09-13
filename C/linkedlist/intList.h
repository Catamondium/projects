#pragma once
#define EMPTYLIST NULL
/* TODO
 * int push(node **headAddr, const int x);
 * int set(node *head, const int x);
 *
 * fromArray constructor?
 */

struct node {
	int data;
	struct node *next;
};

typedef struct node node;
typedef struct node* intList;

int length(node *head);
int sum(node *head);
int product(node *head);

int insert(node **headAddr, const unsigned int index, const int x);
int get(node *head, const int index);
int set(node *head, const int index, const int x);

int pop(node **headAddr);
int removeAt(node **headAddr, const int index);
int destroy(node **headAddr);
