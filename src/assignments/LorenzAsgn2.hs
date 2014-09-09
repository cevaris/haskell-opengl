import Graphics.UI.GLUT

data Lorenz = Lorenz {step::Integer, x::Float, y::Float, z::Float} deriving (Read, Show, Eq)

s  = 10
b  = 2.6666
r  = 28
dt = 0.001

lorenz  :: Float -> [Lorenz]
lorenz  dt = go (Lorenz 0 1 1 1) [(Lorenz 0 1 1 1)]
        where 
          go :: Lorenz -> [Lorenz] -> [Lorenz]
          go (Lorenz 10 _ _ _) xs = reverse xs
          go (Lorenz i x y z)  xs = let l = (Lorenz (i+1) (x+dt*(s*(y-x))) (y+dt*(x*(r-z)-y)) (z+dt*(x*y-b*z)))
                                    in go l (l:xs)
