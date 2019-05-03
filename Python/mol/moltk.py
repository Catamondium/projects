#!/usr/bin/env python3
import tkinter as tk
from functools import partial
from mol import mass, sanitize
from re import findall


def setentry(text, e):
    e.delete(0, tk.END)
    e.insert(0, text)


class App(tk.Frame):
    DEFAULT = ""

    def __init__(self, master=None):
        super().__init__(master)
        self.pack()

        self.input = tk.Entry()
        self.input.bind("<Return>", partial(self.onInput, self))
        self.input.pack()

        self.var = tk.StringVar()
        self.var.set(self.DEFAULT)
        self.input.focus()
        self.input.icursor(tk.END)
        self.input["textvariable"] = self.var

        self.output = tk.Label()
        self.output.pack()

        self.out = tk.StringVar()
        self.out.set(f"{mass(self.DEFAULT):.2f}")
        self.output["textvariable"] = self.out

    def onInput(self, w, event):
        text = sanitize(self.var.get())
        self.var.set(text)
        out = ""
        try:
            val = mass(text)
            self.output.config(bg=self.cget("bg"))
        except:
            out = "ERROR"
            self.output.config(bg="red")
        else:
            out = f"{val:.2f}"
        self.out.set(out)


if __name__ == "__main__":
    app = App()
    app.master.title("Moltk")
    app.mainloop()
