#include <curses.h>
#include <cstdlib>
#include <time.h>
#include <thread>
#include <chrono>
#include "snake.hpp"

/* BUGS:
 * * delay between keypress & action
 * * * adjust framerate?
 * * * halfdelay / timeout?
 */

using namespace std::chrono_literals;

constexpr std::chrono::milliseconds framerate(double fps)
{
    return std::chrono::milliseconds((unsigned long long)std::floor((1 / fps) * 1000));
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

        getmaxyx(stdscr, height, width);
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
    player = snake{spawn(width, height), false};
    fruit = vec{spawn(width, height)};
    while (true)
    {
        getmaxyx(stdscr, height, width);
        erase();

        if (player.update(fruit, width, height))
        {
            player = snake{spawn(width, height), false};
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
