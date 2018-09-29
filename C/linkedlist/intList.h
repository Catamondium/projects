#pragma once
#define EMPTYLIST NULL
/* TODO
 * fromArray constructor?
 *
 * structure-wide arithmetic?
 * * int map(list, int *func(int));
 * * int reduce(list, int *func(int, int), int inital);
 */

struct node {
	int data;
	struct node *next;
};

typedef struct node node;
typedef struct node* intList;

// miscellaneous & arithmetic functions
int length(intList list);
int sum(intList list);
int product(intList list);

// indexed operations
int insert(intList *list, const unsigned int index, const int x);
int get(intList list, const unsigned int index);
int set(intList list, const unsigned int index, const int x);
int removeAt(intList *list, const unsigned int index);

// stack operations
int pop(intList *list);
int push(intList *list, const int x);

// constructors/destructors
int toArray(intList list, int arr[]); // assume correct premade array
int destroy(intList *list);
