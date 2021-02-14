from nodes import * # forward
from random import choice

class Circuit:
    """
    A managed circuit backed by Node graph
    """
    def __init__(self):
        self.inputs = dict()
        self.outputs = dict()
        self.data: list[Node] = [] # all symbols

    @staticmethod
    def fromRAW(*syms: Node):
        c = Circuit()
        c.data = syms
        c.reflect()
        return c

    def reflect(self):
        """
        Canonicalise internal graph
        """
        self.rolecall()
        self._genout()
        self._genIds()
        self._map()

    def rolecall(self):
        n = choice(self.data)
        self.data = n.rolecall()

    def _genout(self):
        """
        Generate missing OUTs
        """
        cop = set(self.data)
        for i in self.data:
            if issubclass(type(i), Logic) and len(i.outputs) == 0:
                nout = Out()
                cop.add(nout)
                nout.input(i)
        self.data.update(cop)
                    
    
    def _genIds(self):
        """
        INTERNAL ID assigner
        """
        #TODO deterministic ordering
        for i,s in enumerate(self.data):
            s._id = i

    def _map(self):
        """
        Label and register unnamed I/O
        """
        char_i = ord('x')
        char_o = ord('x')
        for s in self.data:
            if issubclass(type(s), IO):
                iname = s.name or chr(char_i)
                oname = s.name or chr(char_o)

                if isinstance(s, In):
                    s.setName('i' + iname)
                    self.inputs['i' + iname] = s
                    char_i += 1
                else:
                    s.setName('o' + oname)
                    self.outputs['o' + oname] = s
                    char_o += 1

                if char_i > ord('z'):
                    char_i = ord('a')
                if char_o > ord('z'):
                    char_o = ord('a')
    
    def __call__(self, **kwargs):
        return self.eval(**kwargs)

    def eval(self, **kwargs):
        """
        Evaluate contained circuit
        """
        for k,v in kwargs.items():
            if k in self.inputs:
                self.inputs[k].value = v # OUTs reset themselves anyway
        out = dict()
        for o in self.outputs.values():
            if isinstance(o, Out):
                o.eval()
                out[o.name] = o.value
        return out

    def __repr__(self):
        return f"Circuit({len(self.data)}, I/O = {len(self.inputs)}/{len(self.outputs)})"

    def debug(self) -> str:
        cat = []
        for s in self.data:
            if isinstance(s, Out): # image via depth-first search
                cat.append(s.debug())
        return "Circuit({})".format("\n\t".join(cat))