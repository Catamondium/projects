#!/usr/bin/python3

import sys
import subprocess
import gi
gi.require_version("Gtk", "3.0")
from gi.repository import Gtk

valids = "0123456789+-:"

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

@register("mvFocus")
def mvFocus(*args):
    elapse_entry = builder.get_object("elapse_entry");
    elapse_entry.grab_focus()

@register("clickBtn")
def clickBtn(*args):
    btn.clicked()

@register("destroy")
def destroy(*args):
    Gtk.main_quit()
    sys.exit();

@register("validate")
def validate(*args):
    s_text = start.get_text()
    e_text = elapse.get_text()

    if not any([x == y for x in s_text for y in valids]):
        disableButton()
        return

    if not any([x == y for x in e_text for y in valids]):
        disableButton()
        return
    enableButton()

@register("eval")
def eval(*args):
    s_text = start.get_text()
    e_text = elapse.get_text()
    output = subprocess.run(["addtime", "-q", "--", s_text, e_text], stdout=subprocess.PIPE)
    
    raw = output.stdout.decode('utf-8')
    raw = raw[0:-1]
    result.set_text(raw)

def disableButton():
    btn.set_sensitive(False)
    btn.set_relief(Gtk.ReliefStyle.HALF)

def enableButton():
    btn.set_sensitive(True)
    btn.set_relief(Gtk.ReliefStyle.NORMAL)

# Further postable
builder.connect_signals(handlers)
primary.show_all()

Gtk.main()
