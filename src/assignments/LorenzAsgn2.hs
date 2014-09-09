import Graphics.UI.GLUT


s  = 10
b  = 2.6666
r  = 28
dt = 0.001

data Lorenz = Lorenz {step::Integer, x::Float, y::Float, z::Float} deriving (Read, Show, Eq)
--data LorenzParams = LorenzParams {s:: Integer, b:: Float, r:: Integer, dt:: Float, limit:: Integer} deriving (Read, Show, Eq)

lzBase   = Lorenz 0 1 1 1
--lzParams = 10 2.6666 28 0.001 5


lorenz  :: Float -> [Lorenz]
lorenz  dt = go lzBase [lzBase]
        where 
          go :: Lorenz -> [Lorenz] -> [Lorenz]
          go (Lorenz 10 _ _ _) xs = reverse xs
          go (Lorenz i x y z)  xs = let l = Lorenz (i+1) (x+dt*(s*(y-x))) (y+dt*(x*(r-z)-y)) (z+dt*(x*y-b*z))
                                    in go l (l:xs)
