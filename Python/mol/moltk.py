#!/usr/bin/env python3
import tkinter as tk
from functools import partial
from mol import mass, sanitize, ElementError
from re import findall


class Application(tk.Frame):
    def __init__(self, master=None):
        super().__init__(master)
        self.pack()
        DEFAULT = "C6H6"

        self.input = tk.Entry()
        self.input.bind("<Return>", partial(self.on_input, self))
        self.input.pack()

        self.var = tk.StringVar()
        self.var.set(DEFAULT)
        self.input["textvariable"] = self.var
        self.input.focus()
        self.input.icursor(tk.END)

        self.output = tk.Label()
        self.output.pack()

        self.out = tk.StringVar()
        self.out.set(f"{mass(DEFAULT):.2f} g/mol")
        self.output["textvariable"] = self.out

    def on_input(self, w, event):
        text = sanitize(self.var.get())
        self.var.set(text)
        try:
            val = mass(text)
        except ElementError as e:
            self.out.set(f"\"{e.elem}\" doesn't exist")
            self.output.config(bg="red")
        else:
            self.out.set(f"{val:.2f} g/mol")
            self.output.config(bg=self.cget("bg"))


if __name__ == "__main__":
    app = Application()
    app.master.title("Moltk")
    app.mainloop()
