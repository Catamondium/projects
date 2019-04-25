#include <curses.h>
#include <cstdlib>
#include <time.h>
#include <thread>
#include <chrono>
#include "snake.h"

#define DEBUG

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
    int lastkey; //XXX
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

        getmaxyx(stdscr, width, height);
    }
    void loop();
    void keypressed();

    ~game()
    {
        endwin();
        player.~snake();
        fruit.~vec();
    }
};

void game::loop()
{
    long frame = 0; //XXX
    player = {width, height};
    fruit = {(int)std::floor(rand() % width),
             (int)std::floor(rand() % height)};
    while (true)
    {
        getmaxyx(stdscr, width, height);
        erase();

        player.draw(width, height);
        mvaddch(fruit.y, fruit.x, 'X');

        mvprintw(0, 20, std::to_string(frame).c_str()); //XXX
#ifdef DEBUG
        mvaddch(0, 50, lastkey);
        frame = (frame + 1) % 2;
#endif

        keypressed(); // getch() calls refresh()

#ifdef DEBUG
        std::this_thread::sleep_for(framerate(2));
#else
        //std::this_thread::sleep_for(66ms); // 15 fps
#endif
    }
}

void game::keypressed()
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
    case 'w':
    case KEY_UP:
        player.dir(0, -1);
        break;
    case 's':
    case KEY_DOWN:
        player.dir(0, 1);
        break;
    case 'a':
    case KEY_LEFT:
        player.dir(-1, 0);
        break;
    case 'd':
    case KEY_RIGHT:
        player.dir(1, 0);
        break;
    default:
        break;
    }
}

int main()
{
    game g;
    g.loop();
}