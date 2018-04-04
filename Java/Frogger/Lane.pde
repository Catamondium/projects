/*
TODO:
  PRE: type macros
 Create Lane object:
   pos.x = 0 & w = width & h = grid.y *constants*
   typed constructor:
     obsticle speed, forward to obsticle !! Keep for frog
     obsticle spacing + offset
     number of obsticles
   Obsticle[]
   pos.y, 
   Background colour
   Type:
     Log ==> sticky intersection, deadly !intersection
     Car ==> deadly intersection
   Safety + safety constructor
   
   f(x):
     update()
     display()
     
 Integrate Lane object:
   Lanes[0 ==> 10].pos.y = width ==> 0
   (for : each) update() & display()
   Frog responsibilities:
     get Lane.type if !safety
     check intersection with Lane[lane].obsticles[]:
       stick to logs, die off logs
       die from cars
 **/
