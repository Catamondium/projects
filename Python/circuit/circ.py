from nodes import * # forward

class Circuit:
    """
    A managed circuit backed by Node graph
    """
    def __init__(self):
        self.io = dict()
        self.data: list[Node] = [] # all symbols
    
    @staticmethod
    def fromRAW(*syms: Node):
        c = Circuit()
        c.data = syms
        c._genIds()
        c._map()
        return c
    
    def _genIds(self):
        """
        INTERNAL ID assigner
        """
        #TODO deterministic ordering
        for i,s in enumerate(self.data):
            s._id = i
    
    def _map(self):
        char = ord('x')
        for s in self.data:
            if s.logic in (Logic.IN, Logic.OUT):
                self.io[s.name or chr(char)] = s
                char += 1
                char %= ord('z')+1
    
    def __repr__(self):
        ins = len(list(filter(lambda x: x.logic == Logic.IN, self.data)))
        outs = len(list(filter(lambda x: x.logic == Logic.OUT, self.data)))
        return f"Circuit({len(self.data)}, I/O={ins}/{outs})"

    def debug(self) -> str:
        cat = []
        for s in self.data:
            if s.logic == Logic.OUT:
                cat.append(s.debug())
        return "Circuit({})".format("\n\t".join(cat))