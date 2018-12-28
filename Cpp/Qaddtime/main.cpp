#include "qaddtime.h"
#include <QApplication>

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);
    Qaddtime w;
    w.show();

    return a.exec();
}
