#include <optional>
#include <chrono>
#include <sstream>
#include <iomanip>

#include "note.hpp"

using systime = std::chrono::system_clock;
std::optional<std::string> Note::printEvent()
{
    if (!event)
        return {};
    std::time_t tt = systime::to_time_t(event.value());

    struct std::tm *tm = std::gmtime(&tt);
    std::stringstream ret;
    ret << std::put_time(tm, "%d/%m/%Y %R");
    return ret.str();
}

std::string Note::unmarshal()
{
    std::string ret;

    ret += "Heading:\t" + heading + '\n';
    if (event)
        ret += "Event:\t" + printEvent().value() + '\n';
    ret += body + "\n##";
    return ret;
}

bool operator==(const Note &lhs, const Note &rhs) noexcept
{
    return (lhs.heading == rhs.heading) &&
           (lhs.body == rhs.body) &&
           (lhs.event == rhs.event);
}
