GCC=g++-8
CFLAGS=-std=c++17 -Wall -Wextra -pedantic 
LINK=-lstdc++fs

parser=lib/parsing.cpp lib/parsing.h
note=lib/note.cpp lib/note.h

a.out : main.cpp $(parser) $(note) ~/.notes
	$(GCC) $(CFLAGS) lib/parsing.cpp main.cpp lib/note.cpp $(LINK)
