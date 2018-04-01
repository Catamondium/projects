class Hive {
  ArrayList<Drone> drones;
  Rectangle boundary;

  Hive(Rectangle bound) {
    drones = new ArrayList<Drone>();
    boundary = bound;
  }

  void run() {
    QuadTree tree = new QuadTree(boundary, 5);
    for (Drone a : drones) {
      tree.insert(a.pos);
    }
    for (Drone a : drones) {
      a.run(tree, drones);
    }
    tree.debug();
  }

  void addDrone(Drone a) {
    drones.add(a);
  }
}
