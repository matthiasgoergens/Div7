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
> import Data.List
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

> data Regex a = Lit a | Cat [Regex a] | Alt [Regex a] | Kleene (Regex a) | Empty

> type Transition z a = z -> a -> z
> type STable z a = [(z, a, z)]
> type Table z a = [(z, Regex a, z)]

> msTable :: [state] -> [alpha] -> Transition state alpha -> STable state alpha
> msTable states alpha m = m' <$> states <*> alpha
>     where m' z a = (z, a, m z a)
> mTable = map (\(zi,a,zj) -> (zi,Lit a, zj))

> simplify :: (Ord alpha, Eq alpha, Show alpha
>             ,Ord state, Eq state, Show state) =>
>             state -> state -> Table state alpha -> Regex alpha
> simplify z0 zn table = case rest of
>                       [] -> 
>                       (a:_) -> simplify z0 zn (simplify1 a table)
>     where states = nub . join . map (\(z0,_,z1) -> [z0,z1]) $ table
>           rest = filter (/=zn) . filter (/=z0) $ states
> simplify1 :: (Ord alpha, Eq alpha, Show alpha
>             ,Ord state, Eq state, Show state) =>
>             state -> Table state alpha -> Table state alpha
> simplify1 rm allTable = normals ++ news
>     where classify (z0,a,z1) = (z0 == rm, z1 == rm)
>           fc a = filter ((==a) . classify) $ allTable
>           loops = Kleene . Alt . map (\(_,a,_) -> a) .
>                   fc $ (True, True)
>           normals = fc (False, False)
>           in_rm = fc (True, False)
>           out_rm = fc (False, True)
>           news = connect <$> in_rm <*> out_rm
>           connect (z0,a,_) (_,b,z1) = (z0,Cat [a, loops, b],z1)


> -- in_rm X loops X out_rm
>           
 
let (rmStart, table) = partition (\(z,_,_)-> z == rm) allTable in
                         let (rmEnd, table) = partition (\(_,_,z)-> z == rm) table in
                         join . map op $ table
     where op e@(z0,a,z1) = case (z0==rm,z1==rm) of
                            (True,  _) -> []
                            (False, False) -> [e]
                            (False, True) -> Cat <$> 



> main = do return ()

:t (result fmap m)
(result fmap m) :: (Functor f) => a1 -> f a -> f (a1, a)

                :: (Functor f) => f a1 -> f a -> f (f (a1, a))

 x :: (Monad f) => f (f a -> f b) -> f a -> f b
 x op a = join . ap op . return


(>>=) :: (Monad m) => m a -> (a -> m b) -> m b