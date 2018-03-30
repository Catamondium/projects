class Drone {
  PVector pos, vel, acc;
  float r, maxvel, maxforce;
  int hue;

  Drone(float x_, float y_) {
    pos = new PVector(x_, y_);
    vel = new PVector(random(-2, 2), random(-2, 2));
    acc = new PVector(0, 0);
    r = random(4, 8);
    maxvel = 5;
    maxforce = 0.2;
    hue = color(floor(random(0, 360)), 360, 360);
  }

  void run(ArrayList<Drone> drones) {
    hive(drones);
    update();
    edges();
    show();
  }

  void update() {
    vel.add(acc);
    vel.limit(maxvel);
    pos.add(vel);
    acc.mult(0);
    edges();
  }

  void show() {
    float theta = vel.heading() + HALF_PI;
    fill(hue);
    stroke(0);
    pushMatrix();
    translate(pos.x, pos.y);
    rotate(theta);
    triangle(-r, r*2, r, r*2, 0, -r*2);
    popMatrix();
  }

  void edges() {
    if (pos.x < -r) {
      pos.x = width + r;
    }
    if (pos.y < -r) {
      pos.y = height + r;
    }
    if (pos.x > width + r) {
      pos.x = -r;
    }
    if (pos.y > height + r) {
      pos.y = -r;
    }
  }

  void applyForce(PVector force) {
    acc.add(force);
  }

  void hive(ArrayList<Drone> drones) {
    PVector target = new PVector(mouseX, mouseY);
    PVector seekF = seek(target);
    PVector arriveF = arrive(target);
    PVector sepF = separate(drones, r*2);
    PVector cohF = cohesion(drones, r*4);
    PVector alnF = align(drones, r*4);

    seekF.mult(1);
    arriveF.mult(1);
    sepF.mult(2);
    cohF.mult(1);
    alnF.mult(1.5);

    //applyForce(seekF); // Disabled to show flocking behaviors
    //applyForce(arriveF);
    applyForce(sepF);
    applyForce(cohF);
    applyForce(alnF);
  }

  // Individual behaviors

  // Go target location
  PVector seek(PVector target) {
    PVector desired = PVector.sub(target, pos);
    desired.setMag(maxvel);

    PVector steer = PVector.sub(desired, vel);
    steer.limit(maxforce);

    return steer;
  }

  // Slow to stop at target location
  PVector arrive(PVector target) {
    PVector desired = PVector.sub(target, pos);
    float d = desired.mag();

    if ( d < 100) {
      float m = map(d, 0, 100, 0, maxvel);
      desired.setMag(m);
    } else {
      desired.setMag(maxvel);
    }

    PVector steer = PVector.sub(desired, vel);
    steer.limit(maxforce);
    return steer;
  }

  // Group behaviors

  // Keep apart
  PVector separate(ArrayList<Drone> drones, float dist_) {
    float desiredSep = dist_;
    PVector steer = new PVector(0, 0);
    int count = 0;
    for (Drone other : drones) {
      float d = PVector.dist(pos, other.pos);
      if ((d > 0) && (d < desiredSep)) {
        PVector diff = PVector.sub(pos, other.pos);
        diff.normalize();
        diff.div(d);
        steer.add(diff);
        count++;
      }
    }
    if (count > 0) {
      steer.div(count);
      steer.setMag(maxvel);

      steer.sub(vel);
      steer.limit(maxforce);
    }
    return steer;
  }

  // Keep together
  PVector cohesion(ArrayList<Drone> drones, float dist_) {
    float desiredDist = dist_;
    PVector steer = new PVector(0, 0);
    int count = 0;
    for (Drone other : drones) {
      float d = PVector.dist(pos, other.pos);
      if ((d > 0) && (d < desiredDist)) {
        steer.add(other.pos);
        count++;
      }
    }
    if (count > 0) {
      steer.div(count);
      return seek(steer);
    }
    return steer;
  }

  // Maintain similar direction to other drones
  PVector align(ArrayList<Drone> drones, float dist_) {
    float aligndist = dist_;
    PVector steer = new PVector();
    int count = 0;
    for (Drone other : drones) {
      float d = PVector.dist(pos, other.pos);
      if ((d > 0) && (d < aligndist)) {
        steer.add(other.vel);
        count++;
      }
    }
    if (count > 0) {
      steer.div(count);
      steer.setMag(maxvel);
      steer.sub(vel);
      steer.limit(maxforce);
    }
    return steer;
  }
}
