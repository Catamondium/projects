#include <array>
#include <iostream>

#include "../game.hpp"
#include "hanoi.hpp"

struct CmdHanoi final : public Game
{
    void init() override
    {
        for (int i = 4; i >= 0; --i)
        {
            towers[0].push(i);
        }

        for (int i = 1; i < towers.size(); ++i)
        {
            towers[i] = Tower();
        }

        std::cout << "Move? FROM TO" << std::endl;
    }

    void loop() override
    {
        using std::end;

        for (int i = 0; i < towers.size(); ++i)
        {
            std::cout << '[' << i << "]: ";
            std::cout << printTower(towers[i]) << std::endl;
        }

        if (towers[towers.size() - 1].cont() == std::deque<int>{4, 3, 2, 1, 0})
        {
            std::cout << "WIN in " << mov << " moves." << std::endl;
            noLoop();
        }
    }

    void input() override
    {
        int from, to;
        std::cout << mov << " Move? ";
        std::cin >> from;
        std::cin >> to;

        std::string err = transfer(towers, from, to);
        if (err == "")
        {
            ++mov;
        }
        else
        {
            std::cout << err << std::endl;
        }
    }

private:
    std::array<Tower, TOWERS> towers;
    int mov = 0;
};

int main(int argc, char **argv)
{
    CmdHanoi hanoi;
    hanoi.run();
}