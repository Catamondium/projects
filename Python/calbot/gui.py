#!/usr/bin/env python3
import PySimpleGUI as sg
from parser import parse, GB_FORMAT, tclsmap, Holiday
from sys import argv
from datetime import datetime as dt


def gb_conv(value):
    return dt.strftime(value, GB_FORMAT)


def tabulate(dvals):
    """
    Tabulate window values dates
    """
    starts = sorted(datum for datum in dvals if 's' in datum)
    ends = sorted(datum for datum in dvals if 'e' in datum)
    table_v = [[dvals[s], dvals[e]] for s, e in zip(starts, ends)]
    return sg.Table(table_v, key='holidays', headings=['START', 'END'], justification='center')


def radio_group(title, choices, group_id, frame=True):
    """
    Generate framed horisontal Radio group
    keyed in {title}_{choice} form
    """
    radios = [sg.Radio(ch, group_id, key=f"{title}_{ch}") for ch in choices]
    return sg.Frame(title, [radios]) if frame else radios


def gen(*hols):
    width = len(str(len(hols)))
    return [[sg.In(h.start, key=f"hol_{i:0{width}}s", size=(10, 1)), sg.CalendarButton('cal', target=(i, 0), format=GB_FORMAT),
            sg.In(h.end, key=f"hol_{i:0{width}}e", size=(10, 1)), sg.CalendarButton('cal', target=(i, 2), format=GB_FORMAT)] for i,h in enumerate(hols)]

if __name__ == "__main__":
    if len(argv) > 1:
        descriptor = argv[1]
    else:
        descriptor = sg.PopupGetFile('Open calendar descriptor')
    with open(descriptor) as f:
        data = tclsmap(Holiday, gb_conv, parse(f))

    layout = [*gen(*data), [sg.Quit()]]
    w = sg.Window('Cal manager', layout)
    event, values = w.read()
    w.close()

    layout = [[*radio_group('Bound', ['Start', 'End'], 0, False), sg.Button('Append'), sg.Spin(list(range(len(values) // 2)))], [tabulate(values)], [sg.Quit()]]
    w = sg.Window('Values', layout).finalize()
    hols = w['holidays']
    """
    Design idea:
        Enumerate holidays in Table
        Spinboxes selects element to edit
        Button to append
    """
    while True:
        event, values = w.read()
        if event in (None, 'Quit'):
            break
        else:
            hols.update(values=hols.get() + [['A', 'B']]) # Table is updateable
            print(values)
    w.close()
