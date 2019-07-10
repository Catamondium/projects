#pragma once

struct Game
// Basic step-oriented game
{
    inline void noLoop()
    {
        stoploop = true;
    }

    inline void resume()
    {
        stoploop = false;
    }

    virtual void run()
    {
        init();
        while (true)
        {
            loop();
            input();

            if (stoploop)
                break;
        }
    };

    virtual void init() = 0;
    virtual void loop() = 0;
    virtual void input() = 0;

protected:
    bool stoploop = false;
};