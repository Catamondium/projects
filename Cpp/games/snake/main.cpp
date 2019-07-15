#include <array>
#include <curses.h>
#include <cstdlib>
#include <time.h>

#include "snake.hpp"
#include "../cursesgame.hpp"
#include "pallete.hpp"

struct SnakeGame : public CursesGame<2>
{
    Snake player;
    Vec fruit;

    SnakeGame()
    {
        pallete = {
            Color{FOOD, COLOR_GREEN, COLOR_BLACK},
            Color{SNAKE_HEAD, COLOR_RED, COLOR_BLACK},
        };
    }

    void init() override
    {
        setFrameRate(10);
        srand(time(NULL));
        player = Snake{Vec{width / 2, height / 2}}; // avoids empty body;
        fruit = Vec{spawn(width, height)};
    }

    void loop() override;
    void keyPressed(int) override;
};

void SnakeGame::loop()
{

    if (player.update(fruit, width, height))
    {
        player = Snake{Vec{width / 2, height / 2}};
    }

    player.draw(width, height);
    attron(COLOR_PAIR(FOOD));
    mvaddch(fruit.y, fruit.x, 'X');
    attroff(COLOR_PAIR(FOOD));
}

void SnakeGame::keyPressed(int ch)
{
    switch (ch)
    {
    case 'w': // UP
    case KEY_UP:
        player.dir(0, -1);
        break;

    case 's': // RIGHT
    case KEY_DOWN:
        player.dir(0, 1);
        break;

    case 'a': // LEFT
    case KEY_LEFT:
        player.dir(-1, 0);
        break;

    case 'd': // DOWN
    case KEY_RIGHT:
        player.dir(1, 0);
        break;

    case 'q': // QUIT
        noLoop();
        break;
    default:
        break;
    }
}

int main()
{
    SnakeGame g;
    g.run();
}
