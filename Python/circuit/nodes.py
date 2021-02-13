# internal repr
from enum import Enum, auto
from functools import reduce

##### WARNING
# Evaluation model assumes DFS ordering

class Logic(Enum):
    IN = auto()
    OUT = auto()
    OR = auto()
    AND = auto()
    NOT = auto()


class Node:
    # Input inputs
    inputs = None
    def __init__(self, sym: Logic):
        self.children = [] # inputs
        self._id = None # nodeID
        self.value = None # cached fn
    
    def input(self, *ins):
        self.children.append(ins)
        return self
    
    def eval(self):
        """
        Evaluate node fn
        IN/OUTs forward self.value
        """
        assert(inputs is None or len(self.children) == inputs)
        return self._eval()
    
    def _eval(self):
        """
        INTERNAL evaluation
        """
        return self.value
    
    def __call__(self):
        return self.eval()

class Or(Node):
    def __init__(self):
        super().__init__(Logic.OR)
    
    def _eval(self):
        def fn(a, b):
            return a.value or b.value
        self.value = reduce(fn, self.children, False)
        return self.value
    
    def __repr__(self):
        return "OR:{}{}".format(self._id, "\n, ".join(map(repr, self.children)))

class And(Node):
    def __init__(self):
        super().__init__(Logic.AND)
    
    def _eval(self):
        def fn(a, b):
            return a.value and b.value
        self.value = reduce(fn, self.children, True)
        return self.value
    
    def __repr__(self):
        return "AND:{}{}".format(self._id, "\n, ".join(map(repr, self.children)))

class Not(Node):
    inputs = 1 # can only validly take 1 INode
    def __init__(self):
        super().__init__(Logic.NOT)
    
    def _eval(self):
        return not self.children[0].value
    
    def __repr__(self):
        return f"NOT:{self._id}{repr(self.children[0])}"[:-2]

class In(Node):
    inputs = 0 # takes no INodes
    def __init__(self):
        super().__init__(Logic.IN)
    
    def setVal(self, nval):
        self.value = nval
    
    def __repr__(self):
        return f"IN:{self._id}"

class Out(Node):
    def __init__(self):
        super().__init__(Logic.OUT)
    
    def __repr__(self):
        return f"OUT:{self._id}"
    # impl provided by Node