#pragma once
#define EMPTYLIST NULL
/* TODO
 * int push(node **headAddr, const it x);
 * int set(node *head, const int x);
 * int remove(node **headAddr, const int index);
 */

struct node {
	int data;
	struct node *next;
};

typedef struct node node;
typedef struct node *intList;

int length(node *head);
int returnSafe(const int ret);
int insert(node **headAddr, const unsigned int index, const int x);
int get(node *head, const int index);
int pop(node **headAddr);
int destroy(node **headAddr);
