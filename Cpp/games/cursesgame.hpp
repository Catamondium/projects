#pragma once
#include <curses.h>
#include <chrono>
#include <thread>
#include "game.hpp"

constexpr std::chrono::milliseconds frameRate(double fps)
{
    return std::chrono::milliseconds((int)(1000 / fps));
}

using Mouse = MEVENT;

struct CursesGame : public Game
// 'Realtime' NCurses game
{

    void setFrameRate(double fps)
    {
        framerate = frameRate(fps);
    }

    void run() final
    {
        init();
        while (true)
        {
            getmaxyx(stdscr, height, width);
            erase();
            loop();
            input();

            if (stoploop)
                break;

            std::this_thread::sleep_for(framerate);
        }
    }

    void input() final
    {
        int ch = getch();
        if (ch == 27)
        { // ESC
            noLoop();
            return;
        }

        if (ch == KEY_MOUSE)
        {
            Mouse rawmouse;
            if (getmouse(&rawmouse) == OK)
                mouseEvent(ch, rawmouse);
        }
        else
        {
            keyPressed(ch);
        }
    }

    CursesGame()
    {
        initscr();
        cbreak();
        noecho();
        nonl();
        intrflush(stdscr, TRUE);
        keypad(stdscr, TRUE);
        nodelay(stdscr, TRUE);
        curs_set(0);
        mousemask(ALL_MOUSE_EVENTS, NULL);
        getmaxyx(stdscr, height, width);
    }

    ~CursesGame()
    {
        endwin();
    }

    virtual void keyPressed(int){};
    virtual void mouseEvent(int, Mouse){};

protected:
    int height;
    int width;

private:
    std::chrono::milliseconds framerate = frameRate(60);
};
