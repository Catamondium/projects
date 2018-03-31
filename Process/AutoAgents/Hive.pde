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
      a.run(tree, drones);
    }
    //tree.show();
  }

  void addDrone(Drone a) {
    drones.add(a);
  }
}
