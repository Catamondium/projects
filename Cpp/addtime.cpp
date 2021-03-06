#include <unistd.h> // getopt
#include <math.h>   // floor

#include <string>   // stoi
#include <iostream> // showpos/noshowpos
#include <iomanip>  // setw, setfill

struct Time
{
    int hrs;
    int mins;
    Time(int h, int m) : hrs(h), mins(m){};
    Time(std::string in)
    {
        int delimpos = in.find(':');
        std::string str_hrs = in.substr(0, delimpos);
        std::string str_mins = in.substr(delimpos + 1, in.length());
        this->hrs = stoi(str_hrs);
        this->mins = stoi(str_mins);
    };
    int abs() const;
};

int Time::abs() const
{
    return (hrs * 60) + mins;
}

Time operator+(const Time lhs, const int rhs)
{
    int tot = lhs.abs() + rhs;
    return Time(floor(tot / 60), tot % 60);
}

void usage(const std::string prog)
{
    std::cout << "Usage: " << prog << " [-hql] <HH:MM> <mins | HH:MM>\n"
                                      "note: if mins_elapse is negative, precede it with '--'\n"
                                      "options:\n\t-q quietly output end time\n"
                                      "\t-h print this message and exit\n"
                                      "\t-l print written language"
              << std::endl;
    std::exit(1);
}

std::ostream &operator<<(std::ostream &stream, Time a)
{
    char buf[1000];
    sprintf(buf, "%02d:%02d", a.hrs, a.mins);
    stream << buf; // c-style format solution

    //stream << std::noshowpos << std::setfill('0') << std::setw(2) << a.hrs << ':'
    //	<< std::setw(2) << a.mins; // C++ format solution

    return stream;
}

int main(int argc, char **argv)
{
    bool quiet = false;

    int c;
    while ((c = getopt(argc, argv, "qhl")) != -1)
    {
        switch (c)
        {
        case 'q':
            quiet = true;
            break;
        case 'l':
            std::cout << "Cpp: " << __cplusplus << std::endl;
            return 0;
        default:
            usage(argv[0]);
            break;
        }
    }

    if (argc < 3)
        usage(argv[0]);

    Time start = Time(argv[optind++]);
    int elapse = (std::string(argv[optind]).find(':') != std::string::npos) ? Time(argv[optind]).abs() : atoi(argv[optind]);

    Time end = start + elapse;
    if (!quiet)
    {
        std::cout << "Start:\t" << start << "\t"
                  << std::showpos << elapse << "\nEnd:\t" << end << std::endl;
    }
    else
        std::cout << end << std::endl;
}
