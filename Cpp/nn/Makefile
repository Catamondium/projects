GCC=g++-8
CFLAGS=-std=c++17 -Wall -Wextra -pedantic -g 
LINK=-lstdc++fs

parser=lib/parsing.cpp lib/parsing.hpp
note=lib/note.cpp lib/note.hpp

a.out : main.cpp $(parser) $(note) ~/.notes
	$(GCC) $(CFLAGS) lib/parsing.cpp main.cpp lib/note.cpp $(LINK)
