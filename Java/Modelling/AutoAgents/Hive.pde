class Hive {
  ArrayList<Drone> drones;
  Rectangle boundary;

  Hive(Rectangle bound) {
    drones = new ArrayList<Drone>();
    boundary = bound;
  }

  void run() {
    QuadTree tree = new QuadTree(boundary, 1);
    for (Drone a : drones) {
      tree.insert(a.pos);
    }
    for (Drone a : drones) {
      a.run(tree, drones);
    }
    tree.debug(0);
  }

  void addDrone(Drone a) {
    drones.add(a);
  }
}