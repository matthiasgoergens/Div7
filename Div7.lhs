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
> import Debug.Trace
> import qualified Data.Set as S
> import qualified Data.Map as M
> base = 10
> divi = 7
> states = [0..divi-1] -- [0..6]
> abc = [0..base-1]-- [0..9]
> m x a = (x*base + a) `mod` divi

> -- dt = flip const
> dt s a = trace (s ++ show a) a

main = do return ()

> main = do -- print . length $ t
>           -- print $ t
>--           mapM print $ rs
> --          print $ r1
> --          print "Simplification:"
>           putStrLn . toString $ r

> t = mTable . msTable states abc $ m
> rs = t : (map (\i -> foldr ($) t (map simplify1 [1..i])) $ tail states)
> r1 = head rs
> r = let r = simplify 0 0 t
>     in trace (show r) r

> result = (.)

> data Regex a = Lit a | Cat [Regex a] | Alt [Regex a] | Kleene (Regex a) | Empty deriving (Eq, Ord, Show)
> toString (Lit a) = paren.show $ a
> toString (Cat as) = paren . join . map toString $ as
> toString (Alt as) = paren . intercalate ("\\|") . map toString $ as
> toString (Kleene a) = paren . (++"*") . paren . toString $ a

> normalForm :: Regex a -> Regex a
> normalForm = undefined

> trafo (Kleene (Kleene a)) = Kleene (trafo a)
> trafo (Kleene a) = Kleene (trafo a)
> trafo (Alt as) = jointAlts . map op . nubs $ as
>     where op (Alt b) = Alt b
>           op b = Alt [b]
>           jointAlts = Alt . join . map unAlt
>           unAlt (Alt x) = x
>           unAlt _ = undefined
> 
> trafo (Cat as) = joinCats . map op $ as
>     where op (Cat b) = Cat b
>           op (b) = Cat [b]
>           joinCats = Cat . join . map unCat
>           unCat (Cat x) = x
>           unCat _ = undefined
> trafo x = x

> paren s = "\\("++s++"\\)"

> type Transition z a = z -> a -> z
> type STable z a = [(z, a, z)]
> type Table z a = [(z, Regex a, z)]
> type Dict z a = M.Map (z,z) (Regex a)

> msTable :: [state] -> [alpha] -> Transition state alpha -> STable state alpha
> msTable states alpha m = m' <$> states <*> alpha
>     where m' z a = (z, a, m z a)
> mTable :: STable z a -> Table z a
> mTable = map (\(zi,a,zj) -> (zi,Lit a, zj))
> mDict :: (Ord state, Ord alpha) => Table state alpha -> Dict state alpha
> mDict t = M.fromListWith combine . map (\(z0,a,z1)->((z0,z1),a)) $ t
>     where combine a b = trafo . Alt $ [a, b]
> oDict d = map (\((z0,z1),a) -> (z0,a,z1)) . M.toList $ d
> nubsT = oDict . mDict

> nubs = S.toList . S.fromList

> simplify :: (Ord alpha, Eq alpha, Show alpha
>             ,Ord state, Eq state, Show state) =>
>             state -> state -> Table state alpha -> Regex alpha
> simplify z0 zn table = (if z0 == zn then Kleene else id) . Alt . sort . map (\(_,a,_) -> a) . filter (\(z0',_,z1) -> z0'==z0 && z1==zn) $ simplest
>     where states = nubs . join . map (\(z0,_,z1) -> [z0,z1]) $ table
>           rest = dt "Rest: " . filter (/=zn) . filter (/=z0) $ states
>           simplest = foldr simplify1 table rest
> simplify1 :: (Ord alpha, Eq alpha, Show alpha
>             ,Ord state, Eq state, Show state) =>
>             state -> Table state alpha -> Table state alpha
> simplify1 rm allTable = nubsT (normals ++ news)
>     where classify (z0,a,z1) = (z0 == rm, z1 == rm)
>           fc a = filter ((==a) . classify) $ allTable
>           loops = trace ("loops " ++ show (length allTable) ++ ": ") . Kleene . Alt . sort . map (\(_,a,_) -> a) .
>                   fc $ (True, True)
>           normals = fc (False, False)
>           in_rm = fc (False, True)
>           out_rm = fc (True, False)
>           news = connect <$> in_rm <*> out_rm
>           connect (z0,a,_) (_,b,z1) = (z0,trafo $ Cat [a, loops, b],z1)


> -- in_rm X loops X out_rm
>           
 
let (rmStart, table) = partition (\(z,_,_)-> z == rm) allTable in
                         let (rmEnd, table) = partition (\(_,_,z)-> z == rm) table in
                         join . map op $ table
     where op e@(z0,a,z1) = case (z0==rm,z1==rm) of
                            (True,  _) -> []
                            (False, False) -> [e]
                            (False, True) -> Cat <$> 




:t (result fmap m)
(result fmap m) :: (Functor f) => a1 -> f a -> f (a1, a)

                :: (Functor f) => f a1 -> f a -> f (f (a1, a))

 x :: (Monad f) => f (f a -> f b) -> f a -> f b
 x op a = join . ap op . return


(>>=) :: (Monad m) => m a -> (a -> m b) -> m b