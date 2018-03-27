class Drone {
  PVector pos, vel, acc;
  float r, maxvel, maxforce;
  int hue;

  Drone(int x_, int y_) {
    pos = new PVector(x_, y_);
    vel = new PVector(0, -2);
    acc = new PVector(0, 0);
    r = floor(random(5, 8));
    maxvel = random(4, 6);
    maxforce = random(0.1, 0.3);
    hue = color(floor(random(0, 360)), 360, 360);
  }

  void update() {
    applyBehaviors(drones);
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
    if (pos.y < -this.r) {
      pos.y = height + this.r;
    }
    if (pos.x > width + this.r) {
      pos.x = -r;
    }
    if (pos.y > height + this.r) {
      pos.y = -r;
    }
  }

  void applyForce(PVector force) {
    acc.add(force);
  }

  void applyBehaviors(ArrayList<Drone> drones) {
    PVector seekF = seek(new PVector(mouseX, mouseY));
    PVector arriveF = arrive(new PVector(mouseX, mouseY));
    PVector sepF = separate(drones, r*2);
    PVector cohF = cohesion(drones, r*4);
    PVector alnF = align(drones, r*4);

    seekF.mult(1);
    arriveF.mult(1);
    sepF.mult(4);
    cohF.mult(2);
    alnF.mult(2);

    applyForce(seekF);
    applyForce(arriveF);
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
    PVector sum = new PVector();
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

  // Maintain similar direction
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
