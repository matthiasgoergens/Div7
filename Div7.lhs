> import Control.Monad
> import Control.Applicative
> import Data.List
> import Debug.Trace
> import qualified Data.Set as S
> import qualified Data.Map as M
> base = 10
> divi = 7
> states = [0..divi-1] -- [0..6]
> alphabet = [0..base-1]-- [0..9]
> transitionFunc x a = (x*base + a) `mod` divi

> main = do putStrLn . toString $ r

> t = makeTable . makeSimpleTable states alphabet $ transitionFunc
> r = simplify 0 0 t

> data Regex a = Lit a | Cat [Regex a] | Alt [Regex a] | Kleene (Regex a) | Empty
>     deriving (Eq, Ord, Show)

> toString (Lit a) = putParens.show $ a
> toString (Cat as) = putParens . join . map toString $ as
> toString (Alt as) = putParens . intercalate ("\\|") . map toString $ as
> toString (Kleene a) = putParens . (++"*") . putParens . toString $ a

canonize tries to remove some redundancy from the regular expressions.

> canonize (Kleene (Kleene a)) = Kleene (canonize a)
> canonize (Kleene a) = Kleene (canonize a)
> canonize (Alt as) = jointAlts . map op . nubs $ as
>     where op (Alt b) = Alt b
>           op b = Alt [b]
>           jointAlts = Alt . join . map unAlt
>           unAlt (Alt x) = x
>           unAlt _ = undefined
> 
> canonize (Cat as) = joinCats . map op $ as
>     where op (Cat b) = Cat b
>           op (b) = Cat [b]
>           joinCats = Cat . join . map unCat
>           unCat (Cat x) = x
>           unCat _ = undefined
> canonize x = x

> putParens s = "\\("++s++"\\)"

> type Transition z a = z -> a -> z
> type STable z a = [(z, a, z)]
> type Table z a = [(z, Regex a, z)]
> type Dict z a = M.Map (z,z) (Regex a)

> makeSimpleTable :: [state] -> [alpha] -> Transition state alpha -> STable state alpha
> makeSimpleTable states alpha fun = fun' <$> states <*> alpha
>     where fun' z a = (z, a, fun z a)
> makeTable :: STable z a -> Table z a
> makeTable = map (\(zi,a,zj) -> (zi,Lit a, zj))
> mDict :: (Ord state, Ord alpha) => Table state alpha -> Dict state alpha
> mDict t = M.fromListWith combine . map (\(z0,a,z1)->((z0,z1),a)) $ t
>     where combine a b = canonize . Alt $ [a, b]
> oDict d = map (\((z0,z1),a) -> (z0,a,z1)) . M.toList $ d

Combine duplicate transitions in O (n * log n):

> nubsT = oDict . mDict

Remove duplicates in a list in O(n * log n):

> nubs = S.toList . S.fromList

> simplify :: (Ord alpha, Eq alpha, Show alpha
>             ,Ord state, Eq state, Show state) =>
>             state -> state -> Table state alpha -> Regex alpha
> simplify z0 zn table = (if z0 == zn then Kleene else id) . Alt . sort . map (\(_,a,_) -> a) . filter (\(z0',_,z1) -> z0'==z0 && z1==zn) $ simplest
>     where states = nubs . join . map (\(z0,_,z1) -> [z0,z1]) $ table
>           rest = filter (/=zn) . filter (/=z0) $ states
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
>           connect (z0,a,_) (_,b,z1) = (z0,canonize $ Cat [a, loops, b],z1)


