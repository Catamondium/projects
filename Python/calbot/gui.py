#!/usr/bin/env python3
import PySimpleGUI as sg
from parser import parse, GB_FORMAT, tclsmap, Holiday
from sys import argv
from datetime import datetime as dt
from itertools import starmap


"""
Design idea: TODO
    Box sizes = (10, 1) for dates

    #Enumerate holidays in Table
    #Spins & Radios selects element to edit
    #Buttons for Append, Remove, Edit & Submit

    Load selected bound into In, Cal pair? In requires validation


    Events:
        Enable for Spins & Radios
"""


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
                    sg.Quit()
                ]
        ]
    w = sg.Window('Main', layout).finalize()
    hols = w['-DATA-']
    while True:
        toggle(w, ['Remove', 'Add', 'Apply'], '' == w['-D_TXT-'].get())
        toggle(w, ['Remove', 'Apply'], not w['-DATA-'].get())

        event, values = w.read()

        if event in (None, 'Quit'):
            break
        elif event == 'Append':
            hols.update(values=hols.get() + [['A', 'B']])
        elif event in ('-ROW-',) + fields:
            row = int(w['-ROW-'].get() or -1)
            field = fields.index(w['-FIELD-'].get())
            if row == -1:
                continue
            val = w['-DATA-'].get()[row][field]
            w['-D_TXT-'].update(val)
            print(f"Event: {event}")
            print(f"Vals: {values}")
    w.close()
    # Do we keep going?
    # Supporting comments should do for usability