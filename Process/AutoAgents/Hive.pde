class Hive {
  ArrayList<Drone> drones;

  Hive() {
    drones = new ArrayList<Drone>();
  }

  void run() {
    for (Drone a : drones) {
      a.run(drones);
    }
  }

  void addDrone(Drone a) {
    drones.add(a);
  }
}
