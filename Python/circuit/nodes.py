# internal str
from enum import Enum, auto
from functools import reduce

##### WARNING
# Evaluation model assumes DFS ordering

class InputConflict(Exception):
    def __init__(self, node, incount, in_limit):
        self.message = f"Too many inputs to {node}, {incount} > {in_limit}"
        self.node = node
    def __str__(self):
        return self.message

class Node:
    """
    circ graph Node class
    deriving components form RAW primatives for Circuit
    """
    # Input inputs
    in_limit = None
    def __init__(self, *inputs):

        self.inputs = set()
        self.outputs = set()

        self.input(*inputs)

        self._id = None # nodeID
        self.value = None # cached fn

    def input(self, *ins):
        if all(map(lambda x: issubclass(type(x), Node), ins)):
            self.inputs.update(ins)
            for i in ins:
                i.accept(self)
            return self

    def accept(self, output):
        self.outputs.add(output)
        return self

    def rolecall(self):
        """
        Graph discovery call
        """
        seen = set()
        plen = 0
        # pre populate
        seen.add(self)
        seen.update(self.inputs)
        seen.update(self.inputs)
        while plen != len(seen): # continue until no new nodes
            plen = len(seen)
            s = set(seen) # duplicate
            for sym in s:
                # fresh populations
                seen.update(sym.inputs)
                seen.update(sym.outputs)
            seen.update(s) # copy over
        return seen

    def eval(self):
        """
        Evaluate node fn
        IN/OUTs forward self.value
        """
        if(self.in_limit is not None and (len(self.inputs) > self.in_limit)):
            raise InputConflict(self, len(self.inputs), self.in_limit)
        return self._eval()
    
    def _eval(self):
        """
        INTERNAL evaluation
        """
        return self.value

    def __call__(self):
        return self.eval()
    
    def __repr__(self):
        return f"{type(self).__name__}:{self._id}"

class Logic(Node): # marker type
    pass

class Or(Logic):
    def __init__(self, *inputs):
        super().__init__(*inputs)

    def _eval(self):
        def fn(a, b):
            return a or b.eval()
        self.value = reduce(fn, self.inputs, False)
        return self.value

    def debug(self):
        return "OR({})".format(", ".join(map(lambda x: x.debug(), self.inputs)))

class And(Logic):
    def __init__(self, *inputs):
        super().__init__(*inputs)

    def _eval(self):
        def fn(a, b):
            return a and b.eval()
        self.value = reduce(fn, self.inputs, True)
        return self.value

    def debug(self):
        return "AND({})".format(", ".join(map(lambda x: x.debug(), self.inputs)))

class Not(Logic):
    in_limit = 1 # can only validly take 1 INode
    def __init__(self, *inputs):
        super().__init__(*inputs)

    def _eval(self):
        return not list(self.inputs)[0].eval()
    
    def  debug(self):
        return f"NOT({list(self.inputs)[0].debug()})"[:-2]

class IO(Node):
    def __init__(self, name=None):
        self.name = name
        super().__init__()
    
    def setName(self, name):
        self.name = name

    def __repr__(self):
       n = f"'{self.name}'" if self.name is not None else ''
       return f"{type(self).__name__}{n}:{self._id}"

class In(IO):
    in_limit = 0 # takes no INodes
    def __init__(self, name=None):
        super().__init__(name)

    def setVal(self, nval):
        self.value = nval
    
    def debug(self):
        return f"{self.name}"

    def _eval(self):
        return self.value

class Out(IO):
    in_limit = 1
    def __init__(self, name=None):
        self.name = name
        super().__init__()
    
    def debug(self):
        return f"{self.name} = {list(self.inputs)[0].debug()}"
    
    def _eval(self): # Depth-first order
        self.value = list(self.inputs)[0].eval()
        return self.value