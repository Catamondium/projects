class Tessalator {
  float w, h;
  PVector pos;
  Container cont;
  Tessalator[] tessalators = new Tessalator[4];
  boolean tessalated = false;


  Tessalator(PVector pos_, float w_, float h_) {
    pos = pos_;
    w = w_;
    h = h_;
  }

  void update() {
    if (!tessalated) {
      cont = new Container(pos, w, h, floor(random(1, 3)), floor(random(1, 3)));
      cont.update();
    } else {
      for (Tessalator t : tessalators) {
        t.update();
      }
    }
  }

  void show() {
    if (!tessalated) {
      cont.show();
    } else {
      for (Tessalator t : tessalators) {
        t.show();
      }
    }
  }

  void tessalate() {
    if (!tessalated) {
      float w_new = w / 2;
      float h_new = h / 2;

      float offset_x = w / 4;
      float offset_y = h / 4;

      PVector[] pos_new = new PVector[4];
      pos_new[0] = new PVector(pos.x - offset_x, pos.y - offset_y);
      pos_new[1] = new PVector(pos.x + offset_x, pos.y - offset_y);
      pos_new[2] = new PVector(pos.x - offset_x, pos.y + offset_y);
      pos_new[3] = new PVector(pos.x + offset_x, pos.y + offset_y);

      for (int i = 0; i < tessalators.length; i++) {
        tessalators[i] = new Tessalator(pos_new[i], w_new, h_new);
      }
      tessalated = true;
    } else {
      for (Tessalator t : tessalators) {
        t.tessalate();
      }
    }
  }
}
