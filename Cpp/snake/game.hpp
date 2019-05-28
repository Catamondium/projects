#include <curses.h>
#include <chrono>
#include <thread>

constexpr std::chrono::milliseconds frameRate(double fps)
{
    return std::chrono::milliseconds((int)(1000 / fps));
}

struct Game
{
    void noLoop()
    {
        stoploop = true;
    }

    void setFrameRate(double fps)
    {
        framerate = frameRate(fps);
    }

    void run()
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
            keyPressed(ch);
            std::this_thread::sleep_for(framerate);
        }
    }

    Game()
    {
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

    ~Game()
    {
        endwin();
    }

    int height;
    int width;
    virtual void loop() = 0;
    virtual void init() = 0;
    virtual void keyPressed(int) = 0;

private:
    std::chrono::milliseconds framerate = frameRate(60);
    bool stoploop = false;
};