#include "qaddtime.h"
#include "ui_qaddtime.h"

Qaddtime::Qaddtime(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::Qaddtime)
{
    ui->setupUi(this);
}

Qaddtime::~Qaddtime()
{
    delete ui;
}

void Qaddtime::on_elapse_editingFinished()
{
    QTime start = ui->timeEdit->time();
    int elapse = ui->elapse->value();

    int offset = (start.hour()*60) + start.minute();
    int tot = elapse + offset;

    QString hrs = QString::number(qFloor(tot / 60));
    QString mins = QString::number(tot % 60);

    QString result = QString("%1:%2")
            .arg(hrs, 2, '0').arg(mins, 2, '0');

    ui->result->setText(result);
}

void Qaddtime::on_timeEdit_editingFinished()
{
    Qaddtime::on_elapse_editingFinished();
}
