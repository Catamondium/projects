~/.notes : ./notes # datafile initialisation
	ln -s ./notes ~/.notes # latch onto test datafile

a.out : main.cpp \ # primary/debugging operation
	parsing.cpp parsing.hpp \ # unmarshalling
	note.hpp \ # central type
	~/.notes
	g++ \
		-std=c++17 -Wall -Wextra -pedantic \
		parsing.cpp main.cpp
	./a.out
