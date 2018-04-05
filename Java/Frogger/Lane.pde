int SAFETY = 0;
int CAR = 1;
int LOG = 2;
class Lane extends Rectangle {
 Obsticle[] obsticles;
 int type;
 float speed;
 
 Lane(float y_, color col_) {
  super(5, y_, width, grid.y, col_, false);
  type = SAFETY;
  obsticles = new Obsticle[0];
 }
 
 Lane(float y_, int num, int spc, float spd, float xoff, color col_, int type_) {
   super(5, y_, width, height / lanes.length, col_, false);
   type = type_;
   obsticles = new Obsticle[num];
   speed = spd;
   
   for(int i = 0; i < obsticles.length; i++) {
     color colO = #FFFFFF;
     if(type_ == CAR) {
      colO =  #FF0000;
     }
     if(type_ == LOG) {
      colO = #654321; 
     }
    obsticles[i] = new Obsticle(grid.x * (i + spc), y_, 2 * grid.x, spd, colO);
   }
 }
 
 void update() {
  for(Obsticle a : obsticles) {
   a.update(); 
  }
 }
 
 void display() {
   show();
  for(Obsticle a : obsticles) {
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
