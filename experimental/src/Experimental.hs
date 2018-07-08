module Experimental ( check, mgu ) where

-- Simple check
check :: Bool
check = True

-- Math task about 250 let mgu
-- 250*let + mgu = 2005*god
mgu = filter ff [(toN (l,e,t), toN (m,g,u), toN (g,o,d))| 
                 l <- fd,
                 e <- alld, e /= l,
                 t <- alld, t /= e, t /= l,
                 m <- fd, m /= t, m /= e, m /= l,
                 g <- [1],
                 u <- alld, u /= m, u /= t, u /= e, u /= l,
                 o <- alld, o /= u, o /= m, o /= t, o /= e, o /= l,
                 d <- alld, d /= o, d /= u, d /= m, d /= t, d /= e, d /= l]
      where 
        toN (x,y,z) = x*100+y*10+z                      
        ff(x,y,z) = 250*x + y == 2005*z
        alld = [0,2,3,4,5,6,7,8,9]
        fd=[2..9]
