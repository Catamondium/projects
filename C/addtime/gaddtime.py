#!/usr/bin/python3

import sys
import re
import subprocess
import gi
gi.require_version("Gtk", "3.0")
from gi.repository import Gtk

# gtk preable
builder = Gtk.Builder()
builder.add_from_file("gui.glade")
primary = builder.get_object("MainWindow")

start = builder.get_object("start")
elapse = builder.get_object("elapse")
result = builder.get_object("result")
btn = builder.get_object("run");

handlers = {} 
def register(handler):
    def decorator(func):
        handlers[handler] = func
        return func
    return decorator

def validEntry(text):
    return bool(re.match("^[0-9:\+\-]+$", text))

def enableButton(truth):
    btn.set_sensitive(truth)
    if truth:
        relief = Gtk.ReliefStyle.NORMAL 
    else:
        relief = Gtk.ReliefStyle.HALF
    btn.set_relief(relief)

#### Window level callbacks
@register("destroy")
def destroy(*args):
    Gtk.main_quit()
    sys.exit();

@register("keypress")
def keypress(window, event):
    if event.keyval == 65307: # esc
        destroy()

#### Widget level callbacks
@register("validate")
def validate(*args):
    s_text = start.get_text()
    e_text = elapse.get_text()

    enableButton(validEntry(s_text) and validEntry(e_text))

@register("mvFocus")
def mvFocus(*args):
    elapse_entry = builder.get_object("elapse_entry");
    elapse_entry.grab_focus()

@register("clickBtn")
def clickBtn(*args):
    validate()
    btn.clicked()

@register("eval")
def eval(*args):
    s_text = start.get_text()
    e_text = elapse.get_text()

    raw = subprocess.check_output(
            ["addtime", "-q", "--", s_text, e_text])\
                    .decode('utf-8')
    raw = raw[0:-1]
    result.set_text(raw)

# Further postable
builder.connect_signals(handlers)
primary.show_all()

Gtk.main()
