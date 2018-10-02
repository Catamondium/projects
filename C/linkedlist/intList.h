#pragma once
#define EMPTYLIST NULL
/* TODO
 * fromArray constructor?
 */

struct node {
	int data;
	struct node *next;
};

typedef struct node node;
typedef struct node* intList;

// reducing function signature
typedef int (*R_agent)(int/*accumulator*/, int/*current*/);

// mapping function signature
typedef int (*M_agent)(int/*current*/);

// miscellaneous & arithmetic functions
int length(intList list);
int sum(intList list);
int product(intList list);

// indexed operations
int insert(intList *list, const signed int index, const int x);
int removeAt(intList *list, const signed int index);
int get(intList list, const signed int index);
void set(intList list, const signed int index, const int x);

// stack operations
int pop(intList *list);
int push(intList *list, const int x);

// structure-wide operations
void map(intList list, M_agent f);
int reduce(intList list, R_agent f, const int init);

// constructors/destructors
int toArray(intList list, int arr[]); // assume correct premade array
int destroy(intList *list);
