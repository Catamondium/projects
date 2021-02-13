# internal str
from enum import Enum, auto
from functools import reduce

##### WARNING
# Evaluation model assumes DFS ordering

class Logic(Enum):
    IN = auto()

    OR = auto()
    AND = auto()
    NOT = auto()

    OUT = auto()


class Node:
    """
    circ graph Node class
    deriving components form RAW primatives for Circuit
    """
    # Input inputs
    inputs = None
    def __init__(self, logic: Logic):
        self.logic = logic
        self.children = list() # inputs
        self._id = None # nodeID
        self.value = None # cached fn
    
    def input(self, *ins):
        self.children += ins
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
    
    def __repr__(self):
        return f"{self.logic.name}:{self._id}"

class Or(Node):
    def __init__(self):
        super().__init__(Logic.OR)
    
    def _eval(self):
        def fn(a, b):
            return a.value or b.value
        self.value = reduce(fn, self.children, False)
        return self.value
    
    def debug(self):
        return "OR:{}({})".format(self._id, ", ".join(map(lambda x: x.debug(), self.children)))

class And(Node):
    def __init__(self):
        super().__init__(Logic.AND)
    
    def _eval(self):
        def fn(a, b):
            return a.value and b.value
        self.value = reduce(fn, self.children, True)
        return self.value
    
    def debug(self):
        return "AND:{}({})".format(self._id, ", ".join(map(lambda x: x.debug(), self.children)))

class Not(Node):
    inputs = 1 # can only validly take 1 INode
    def __init__(self):
        super().__init__(Logic.NOT)
    
    def _eval(self):
        return not self.children[0].value
    
    def  debug(self):
        return f"NOT:{self._id}({self.children[0].debug()})"[:-2]

class In(Node):
    inputs = 0 # takes no INodes
    def __init__(self):
        self.name = None
        super().__init__(Logic.IN)

    def setVal(self, nval):
        self.value = nval
    
    def setName(self, name):
        self.name = name
    
    def debug(self):
        return repr(self)

    def __repr__(self):
       n = f"'{self.name}'" if self.name is not None else ''
       return f"I{n}:{self._id}"

class Out(Node):
    inputs = 1
    def __init__(self):
        self.name = None
        super().__init__(Logic.OUT)
    
    def setName(self, name):
        self.name = name
    
    def debug(self):
        return f"{self} = {self.children[0].debug()}"
    
    def __repr__(self):
        n = f"'{self.name}'" if self.name is not None else ''
        return f"O{n}:{self._id}"