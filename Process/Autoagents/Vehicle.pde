class Vehicle {
  PVector pos, vel, acc;
  float r, maxvel, maxforce;

  Vehicle(int x_, int y_) {
    pos = new PVector(x_, y_);
    vel = new PVector(0, -2);
    acc = new PVector(0, 0);
    r = 5;
    maxvel = 4;
    maxforce = 0.1;
  }

  void update() {
    vel.add(acc);
    vel.limit(maxvel);
    pos.add(vel);
    acc.mult(0);
  }

  void show() {
    float theta = vel.heading() + HALF_PI;
    fill(255);
    stroke(0);
    pushMatrix();
    translate(pos.x, pos.y);
    rotate(theta);
    triangle(-r, r*2, r, r*2, 0, -r*2);
    popMatrix();
  }

  void applyForce(PVector force) {
    acc.add(force);
  }

  // Individual behaviors
  void seek(PVector target) {
    PVector desired = PVector.sub(target, pos);
    desired.setMag(maxvel);

    PVector steer = PVector.sub(desired, vel);
    steer.limit(maxforce);

    applyForce(steer);
  }

  void arrive(PVector target) {
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
    applyForce(steer);
  }

  // Group behaviors

  void separate(ArrayList<Vehicle> vehicles) {
    float desiredsep = r * 2;
    PVector sum = new PVector();
    int count = 0;
    for (Vehicle other : vehicles) {
      float d = PVector.dist(pos, other.pos);
      if ((d > 0) && (d < desiredsep)) {
        PVector diff = PVector.sub(pos, other.pos);
        diff.normalize();
        diff.div(d);
        sum.add(diff);
        count++;
      }
    }
    if (count > 0) {
      sum.div(count);
      sum.setMag(maxvel);
      
      PVector steer = PVector.sub(sum, vel);
      steer.limit(maxforce);
      applyForce(steer);
    }
  }
}
