int SAFETY = 0;
int CAR = 1;
int LOG = 2;
class Lane extends Rectangle {
 Obsticle[] objects;
 int type;
 
 Lane(float y_, color col_) {
  super(0, y_, width, grid.y, col_);
  type = SAFETY;
  objects = new Obsticle[0];
 }
 
 Lane(float y_, int num, int spc, float spd, float xoff, color col_, int type_) {
   super(width / 2, y_, width, grid.y, col_);
   type = type_;
   objects = new Obsticle[num];
   
   for(int i = 0; i < objects.length; i++) {
    objects[i] = new Obsticle((xoff + (spc * i) + grid.x), y_, 2 * grid.x, spd, #FF0000);
   }
 }
 
 void update() {
  for(Obsticle a : objects) {
   a.update(); 
  }
 }
 
 void display() {
   show();
  for(Obsticle a : objects) {
   a.show(); 
  }
 }
}


/*
TODO:
 Create Lane object:
   typed constructor:
     Obstical spacing + offset
     number of Obsticals
   Obstical[]
   pos.y, 
   Background colour
   Type:
     Log ==> sticky intersection, deadly !intersection
     Car ==> deadly intersection
   
   f(x):
     update()
     display()
     
 Integrate Lane object:
   Lanes[0 ==> 10].pos.y = width ==> 0
   (for : each) update() & display()
   Frog responsibilities:
     constrain lane to Lane[].length
     get Lane.type if !safety
     check intersection with Lane[lane].Obsticals[]:
       stick to logs, die off logs
       die from cars
 **/
