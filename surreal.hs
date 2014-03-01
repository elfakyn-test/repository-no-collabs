-- Implements the surreal numbers
-- Don't you dare do 3 * 3

module Data.Surreal ( Surreal, srZero ) where

data Surreal = Surreal [Surreal] [Surreal]
    deriving (Show, Read)

srZero = Surreal [] []

instance Eq Surreal where
    x == y = (x <= y) && (y <= x)

instance Ord Surreal where
    (<=) = srleq

instance Num Surreal where
    (+) = srPlus
    (-) = srMinus
    (*) = srMult
    negate      = srNegate
    abs         = srAbs
    signum      = srSignum
    fromInteger = srFromInteger


srleq :: Surreal -> Surreal -> Bool
srleq (Surreal [] [])   (Surreal [] []) = True
srleq x@(Surreal xl _) y@(Surreal _ yr) =
    all (not . flip srleq x) yr &&
    (all (not . srleq y) xl)

srPlus :: Surreal -> Surreal -> Surreal
srPlus x@(Surreal xl xr) y@(Surreal yl yr) =
    Surreal (lPlus xl y ++ (rPlus x yl))
            (lPlus xr y ++ (rPlus x yr))
        where
            lPlus xs y = [ x `srPlus` y | x <- xs ]
            rPlus x ys = [ x `srPlus` y | y <- ys ]

srNegate :: Surreal -> Surreal
srNegate (Surreal xl xr) = Surreal (map srNegate xr) (map srNegate xl)

srAbs :: Surreal -> Surreal
srAbs x
    | x < srZero = srNegate x
    | otherwise  = x

srSignum :: Surreal -> Surreal
srSignum x
    | x == srZero = srZero
    | x < srZero  = srMinusOne
    | otherwise   = srPlusOne
    where
        srPlusOne  = Surreal [srZero] []
        srMinusOne = Surreal [] [srZero]

srMinus :: Surreal -> Surreal -> Surreal
srMinus x y = srPlus x (srNegate y)

srMult :: Surreal -> Surreal -> Surreal
srMult x@(Surreal xl xr) y@(Surreal yl yr) =
    Surreal ((f xl yl) ++ f xr yr)
            ((f xl yr) ++ f xr yl)
        where
            f xs ys = lMult xs y `tPlus` (rMult x ys) `tMinus` (tMult xs ys)
            lMult  xs y  = [ x `srMult`  y | x <- xs          ]
            rMult  x  ys = [ x `srMult`  y |          y <- ys ]
            tMult  xs ys = [ x `srMult`  y | x <- xs, y <- ys ]
            tPlus  xs ys = [ x `srPlus`  y | x <- xs, y <- ys ]
            tMinus xs ys = [ x `srMinus` y | x <- xs, y <- ys ]

srFromInteger :: Integer -> Surreal
srFromInteger x
    | x < 0     = srNegate $ srFromInteger (-x)
    | x == 0    = srZero
    | otherwise = Surreal [srFromInteger (x - 1)] []

