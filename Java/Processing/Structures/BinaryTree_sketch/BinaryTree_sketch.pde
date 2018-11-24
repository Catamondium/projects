 BinTree tree = new BinTree();

void setup() {
  size(700, 600);
  for (int i = 0; i < 100; i++) {
    tree.addValue(floor(random(100)));
  }
  tree.traverse();
  tree.search(20);
}

void draw() {
  background(0);
}
