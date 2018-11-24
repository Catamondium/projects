class Node {
  float value;
  Node lesser, greater;

  Node(float i) {
    value = i;
  }

  void addNode(Node a) {
    if (a.value < value) {
      if (lesser == null) {
        lesser = a;
      } else {
        lesser.addNode(a);
      }
    } else if (greater == null) {
      greater = a;
    } else {
      greater.addNode(a);
    }
  }

  void visit() {
    if (lesser != null) {
      lesser.visit();
    }
    println(value);
    if (greater != null) {
      greater.visit();
    }
  }

  void search(float val) {
    if (val == value) {
      println("Found: " + value);
    } else if ((val < value) && (lesser != null)) {
      lesser.search(val);
    } else if ((val >= value) && (greater != null)) {
      greater.search(val);
    }
  }
}

class BinTree {
  Node root;

  void addValue(float v) {
    Node n = new Node(v);
    if (root == null) {
      root = n;
    } else {
      root.addNode(n);
    }
  }

  void traverse() {
    root.visit();
  }

  void search(float val) {
    root.search(val);
  }
}
