#include <iostream>
#include <vector>
#include "parsing.hpp"
#include "note.hpp"

/* TODO
 * Decide on DueTime implementation
 * * Implement looped parsing for the field
 * IO
 * * implement argv IO
 * * implement interactive IO
 * Notification system
 */

int main(int argc, char **argv) {
	parsing::parse("notes");
}
