-- /*
--  * Simple program to demonstrate generating coordinates
--  * using the Lorenz Attractor
--  */

-- #include <stdio.h>

-- /*  Lorenz Parameters  */
-- double s  = 10;
-- double b  = 2.6666;
-- double r  = 28;

-- /*
--  *  Main
--  */
-- int main(int argc, char *argv[])
-- {
--    int i;
--    /*  Coordinates  */
--    double x = 1;
--    double y = 1;
--    double z = 1;
--    /*  Time step  */
--    double dt = 0.001;

--    printf("%5d %8.3f %8.3f %8.3f\n",0,x,y,z);
--    /*
--     *  Integrate 50,000 steps (50 time units with dt = 0.001)
--     *  Explicit Euler integration
--     */
--    for (i=0;i<50000;i++)
--    {
--       double dx = s*(y-x);
--       double dy = x*(r-z)-y;
--       double dz = x*y - b*z;
--       x += dt*dx;
--       y += dt*dy;
--       z += dt*dz;
--       printf("%5d %8.3f %8.3f %8.3f\n",i+1,x,y,z);
--    }
--    return 0;
-- }

--data Lorenz = Lorenz {step::Integer, x::GLfloat, y::GLfloat, z::GLfloat} deriving (Read, Show, Eq)

s  = 10
b  = 2.6666
r  = 28
dt = 0.001
lorenz  :: Float -> [(Integer, Float, Float, Float)]
lorenz  dt = go 1 1 1 0 [(0, 1, 1, 1)]
        where 
          go :: Float -> Float -> Float -> Integer -> [(Integer, Float, Float, Float)] -> [(Integer, Float, Float, Float)]
          go _ _ _ 10    xs = reverse xs
          go x y z step xs = let dx = s*(y-x)
                                 dy = x*(r-z)-y
                                 dz = x*y - b*z
                                 x' = x+dt*dx
                                 y' = y+dt*dy
                                 z' = z+dt*dz
                             in go x' y' z' (step+1) ((step+1, x', y', z'):xs)

          --go x y z step xs = do 
          --  let s  = 10
          --      b  = 2.6666
          --      r  = 28
          --      x' = (x+(dt*(s*(y-x))))
          --      y' = (y+(dt*(x*(r-z))-y))
          --      z' = (z+(dt*(x*y-b*z)))
          --  go x' y' z' (step-1) (step, x', y', z'):xs
          --go (y:ys) = y `k` go ys
