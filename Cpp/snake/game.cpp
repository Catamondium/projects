#include <curses.h>
#include <cstdlib>
#include <time.h>
#include <thread>
#include <chrono>
#include "snake.hpp"

constexpr std::chrono::milliseconds framerate(double fps)
{
    return std::chrono::milliseconds((int)(1000 / fps));
}

struct game
{
    int width;
    int height;
    snake player;
    vec fruit;

    game()
    {
        srand(time(NULL));

        initscr();
        cbreak();
        noecho();
        nonl();
        intrflush(stdscr, TRUE);
        keypad(stdscr, TRUE);
        nodelay(stdscr, TRUE);
        curs_set(0);

        player = snake{vec(width / 2, height / 2)}; // avoids empty body;
    }
    void loop();
    bool keypressed();

    ~game()
    {
        endwin();
    }
};

void game::loop()
{
    getmaxyx(stdscr, height, width);

    fruit = vec{spawn(width, height)};
    while (true)
    {
        getmaxyx(stdscr, height, width);
        erase();

        if (player.update(fruit, width, height))
        {
            player = snake{vec(width / 2, height / 2)};
        }

        player.draw(width, height);
        mvaddch(fruit.y, fruit.x, 'X');
        if (keypressed())
            break;

        std::this_thread::sleep_for(framerate(10));
    }
}

bool game::keypressed() // bool == should_quit
{
    switch (getch())
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
    case 27:
        return true;
    default:
        break;
    }

    return false;
}

int main()
{
    game g;
    g.loop();
}
