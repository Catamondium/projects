GCC=g++-8
CFLAGS=-std=c++17 -Wall -Wextra -pedantic -g
LINK=-lstdc++fs

parser=lib/parsing.cpp lib/parsing.hpp
note=lib/note.cpp lib/note.hpp
util=lib/util.cpp lib/util.hpp

a.out : main.cpp $(parser) $(note) $(util) ~/.notes
	$(GCC) $(CFLAGS) main.cpp lib/util.cpp lib/parsing.cpp lib/note.cpp $(LINK)
