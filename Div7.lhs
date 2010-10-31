from left to right.

10 mod 7 = 3

0, 7 -> 0
1, 8 -> 3
2, 9 -> 6
3 -> 2
4 -> 5
5 -> 1
6 -> 4


r0 = (0|7) | (1|8) r4 | (2|9) r1 | 3 r5 | 4 r2 | 5 r6
r1 = (0|7) r1 | (1|8) r

State-machine

> import Control.Monad
> import Control.Applicative
> states = [0..6]

> m x a = (x*10 + a) `mod` 7

> -- table' = (fmap (fmap m) =<< states) =<< states

> result = (.)

 table :: [[Int]]
 table = helper <$> states

 helper :: Int -> [(Int,Int)]
 helper = ((=<<states) . ((.) pure . (,)))

 table'' = ((>>=) states . (.) return . m) <$> states

 table' = (return . (>>=) states . (.) return . m) =<< states

 data Regex a = Lit a | Cat (Regex a) (Regex a) | Alt (Regex a) (Regex a) | Kleene (Regex a)

 simplify :: [alpha] -> state -> [state] -> (state -> alpha -> state) -> 


> main = do return ()

:t (result fmap m)
(result fmap m) :: (Functor f) => a1 -> f a -> f (a1, a)

                :: (Functor f) => f a1 -> f a -> f (f (a1, a))

 x :: (Monad f) => f (f a -> f b) -> f a -> f b
 x op a = join . ap op . return


(>>=) :: (Monad m) => m a -> (a -> m b) -> m b