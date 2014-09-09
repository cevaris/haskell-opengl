import Graphics.UI.GLUT

data Lorenz = Lorenz {step::Integer, x::Float, y::Float, z::Float} deriving (Read, Show, Eq)

s  = 10
b  = 2.6666
r  = 28
dt = 0.001

lorenz  :: Float -> [Lorenz]
lorenz  dt = go 0 1 1 1 [Lorenz 0 1 1 1]
        where 
          go :: Integer -> Float -> Float -> Float -> [Lorenz] -> [Lorenz]
          go 10   _ _ _ xs = reverse xs
          go step x y z xs = let dx = s*(y-x)
                                 dy = x*(r-z)-y
                                 dz = x*y-b*z
                                 x' = x+dt*dx
                                 y' = y+dt*dy
                                 z' = z+dt*dz
                             in go (step+1) x' y' z' (Lorenz (step+1) x' y' z':xs)
