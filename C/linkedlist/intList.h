#pragma once
#define EMPTYLIST NULL
/* TODO
 * int push(Node **headAddr);
 * int set(Node *head);
 * int insert(Node **headAddr, int index, int x);
 */

struct Node {
	int data;
	struct Node *next;
};

typedef struct Node Node;
typedef struct Node *intList;

int length(Node *head);
int returnSafe(int ret);
int add(Node **headAddr, int x);
int get(Node *head, int index);
int pop(Node **headAddr);
int destroy(Node **headAddr);
