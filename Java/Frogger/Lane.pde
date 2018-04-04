/*
TODO:
  PRE: type macros
 Create Lane object:
   pos.x = 0 & w = width & h = grid.y *constants*
   typed constructor:
     Obstical speed, forward to Obstical !! Keep for frog
     Obstical spacing + offset
     number of Obsticals
   Obstical[]
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
     constrain lane to Lane[].length
     get Lane.type if !safety
     check intersection with Lane[lane].Obsticals[]:
       stick to logs, die off logs
       die from cars
 **/
