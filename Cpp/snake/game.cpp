#include <curses.h>
#include <cstdlib>
#include <time.h>
#include <thread>
#include <chrono>
#include "snake.hpp"

#define DEBUG
/* TODO:
 * BASIC:
 * * eat fruit
 * * death conditions
 * BUGS:
 * * delay between keypress & action
 * * * adjust framerate?
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

#ifdef DEBUG
    int lastkey;
#endif
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
#ifdef DEBUG
    long frame = 0;
#endif

    player = spawn(width, height);
    fruit = spawn(width, height);
    while (true)
    {
        getmaxyx(stdscr, height, width);
        erase();

        player.update(fruit, width, height);
        player.draw(width, height);
        mvaddch(fruit.y, fruit.x, 'X');

#ifdef DEBUG
        mvaddstr(1, 0, ("Framemod: " + std::to_string(frame)).c_str());
        mvaddch(2, 0, lastkey);
        frame = (frame + 1) % 2;
#endif

        if (keypressed())
            break;

#ifdef DEBUG
        std::this_thread::sleep_for(framerate(5));
#else
        std::this_thread::sleep_for(framerate(10));
#endif
    }
}

bool game::keypressed() // bool == should_quit
{
#ifdef DEBUG
    int ch = getch();
    if (ch != ERR)
        lastkey = ch;
    switch (ch)
#else
    switch (getch())
#endif
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
