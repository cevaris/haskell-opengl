import Graphics.UI.GLUT

data Lorenz = Lorenz {step::Integer, x::Float, y::Float, z::Float} deriving (Read, Show, Eq)

s  = 10
b  = 2.6666
r  = 28
dt = 0.001

lorenz  :: Float -> [Lorenz]
lorenz  dt = go 1 1 1 0 [Lorenz 0 1 1 1]
        where 
          go :: Float -> Float -> Float -> Integer -> [Lorenz] -> [Lorenz]
          go _ _ _ 10    xs = reverse xs
          go x y z step xs = let dx = s*(y-x)
                                 dy = x*(r-z)-y
                                 dz = x*y-b*z
                                 x' = x+dt*dx
                                 y' = y+dt*dy
                                 z' = z+dt*dz
                             in go x' y' z' (step+1) (Lorenz (step+1) x' y' z':xs)
