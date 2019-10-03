#!/usr/bin/env python3
import PySimpleGUI as sg
from parser import parse, GB_FORMAT, tclsmap, Holiday
from sys import argv
from datetime import datetime as dt
from itertools import starmap
from csv import writer


""" Refactoring TODO
    Table wrapper
    indexed string spinner
"""


class IntSpinner:
    def __init__(self, spinner, upper):
        self.spinner = spinner
        self.upper = upper
        self.update()

    def __iadd__(self, val):
        self.upper += val
        self.update()
        return self

    def __isub__(self, val):
        self.upper -= val
        self.update()
        return self

    def update(self):
        self.spinner.update(values=list(range(self.upper)))

    def get(self):
        return int(self.spinner.get())


def gb_conv(value):
    return dt.strftime(value, GB_FORMAT)


def tabulate(dvals, **kwargs):
    """
    Tabulate window values dates
    """
    table_v = list(map(list, dvals))
    return sg.Table(table_v, headings=['START', 'END'], **kwargs)


def toggle(win, keys, disabled=True):
    for k in keys:
        win[k].update(disabled=bool(disabled))


if __name__ == "__main__":
    fields = ('Start', 'End')
    sg.change_look_and_feel('GreenTan')
    if len(argv) > 1:
        descriptor = argv[1]
    else:
        descriptor = sg.PopupGetFile('Open calendar descriptor', file_types=(("ALL FILES", "*"),))

    if descriptor is None:
        exit(0)

    with open(descriptor) as f:
        data = list(tclsmap(Holiday, gb_conv, parse(f)))

    try:
        init = data[0] and 0
    except KeyError:
        init = None
    ctl = [
            [
                sg.In(key='-D_TXT-', size=(10, 1), justification='center', enable_events=1),
                sg.CalendarButton('Cal', target='-D_TXT-', format=GB_FORMAT, key='-D_RAW-')
            ],
            [
                sg.Spin(fields, initial_value='Start', key='-FIELD-'),
                sg.Spin(list(range(len(data))), initial_value=init, key='-ROW-', enable_events=1)
            ],
            [
                sg.Button('Add'), sg.Button('Remove'),
                sg.Button('Apply')]
        ]

    layout = [
                [
                    tabulate(data, key='-DATA-', justification='center'),
                    sg.Frame('Controls', ctl, element_justification='center')
                ],
                [
                    sg.Cancel(),
                    sg.Submit()
                ]
        ]
    w = sg.Window('Main', layout).finalize()
    tdata = w['-DATA-']
    row = IntSpinner(w['-ROW-'], len(tdata.get()))
    rfield = w['-FIELD-']
    text = w['-D_TXT-']

    ndata = None
    while True:
        toggle(w, ['Remove', 'Add', 'Apply'], '' == text.get())
        toggle(w, ['Remove', 'Apply'], not tdata.get())

        event, values = w.read()

        if event in (None, 'Cancel'):
            break
        elif event == 'Submit':
            ndata = tdata.get()
            break
        elif event == 'Add':
            tdata.update(values=tdata.get() + [[text.get(), '']])
            row += 1
        elif event == 'Apply':
            row = row.get()
            field = fields.index(rfield.get())
            vals = tdata.get()
            nval = text.get()
            vals[row][field] = nval
            tdata.update(vals)
        elif event == 'Remove':
            nrow = row.get()
            vals = tdata.get()
            del vals[nrow]
            tdata.update(vals)
            row -= 1
        elif event in ('-ROW-',) + fields:
            nrow = int(row.get())
            field = fields.index(rfield.get())
            if nrow == -1:
                continue
            val = tdata.get()[nrow][field]
            text.update(val)

        print(f"Event: {event}")
        print(f"Vals: {values}")
    w.close()

    if ndata is not None:
        with open(descriptor, 'w+') as f:
            cwriter = writer(f, delimiter=' ')
            cwriter.writerows(ndata)
