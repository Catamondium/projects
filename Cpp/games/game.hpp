#include <curses.h>
#include <chrono>
#include <thread>

struct Game
// Basic step-oriented game
{
    void noLoop()
    {
        stoploop = true;
    }

    void resume()
    {
        stoploop = false;
    }

    virtual void run()
    {
        init();
        while (true)
        {
            loop();

            if (stoploop)
                break;
        }
    };

    virtual void init() = 0;
    virtual void loop() = 0;

protected:
    bool stoploop = false;
};

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
            int ch = getch();
            if (stoploop || ch == 27) // ESC
                break;

            if (ch == KEY_MOUSE)
            {
                MEVENT rawmouse;
                if (getmouse(&rawmouse) == OK)
                    mouseEvent(ch, rawmouse);
            }
            else
            {
                keyPressed(ch);
            }

            std::this_thread::sleep_for(framerate);
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
