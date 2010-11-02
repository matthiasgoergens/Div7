> import Control.Monad
> import Control.Applicative
> import Data.List
> import qualified Data.Set as S
> import qualified Data.Map as M
> import Data.Function

We are interested in decimal numbers:

> base = 10

and their divisibility by 7:

> divi = 7

Our strings will have 10 different digits and our automaton needs 7 states.

> digits = [0..base-1]
> states = [0..divi-1]

Going from one digit to the next, the state will transition as follows:

> transitionFunc :: Transition Int Int
> transitionFunc state digit = (state*base + digit) `mod` divi

Here `z' denotes a state and `a' denotes a digit.

> type Transition z a = z -> a -> z
> type STable z a = [(z, a, z)]
> type Table z a = [(z, Regex a, z)]
> type Dict z a = M.Map (z,z) (Regex a)

> makeSimpleTable :: [state] -> [digit] -> Transition state digit -> STable state digit
> makeSimpleTable states digit fun = fun' <$> states <*> digit
>     where fun' z a = (z, a, fun z a)

> makeTable :: STable z a -> Table z a
> makeTable = map (\(zi,a,zj) -> (zi,Lit a, zj))

We only need maps (or dicts, to use the Python term) for combining duplicate transitions in our tables:

> makeDict :: (Ord state, Ord digit) => Table state digit -> Dict state digit
> makeDict t = M.fromListWith combine . map (\(z0,a,z1)->((z0,z1),a)) $ t
>     where combine a b = Alt $ [a, b]

> dict2table :: (Ord state, Ord digit) => Dict state digit -> Table state digit
> dict2table d = map (\((z0,z1),a) -> (z0,a,z1)) . M.toList $ d

Combine duplicate transitions in O (n * log n):

> nubsT = dict2table . makeDict



> t = makeTable . makeSimpleTable states digits $ transitionFunc
> regex = simplify 0 0 t

> main = do print regex

> data Regex a = Lit a  | Kleene (Regex a) | Cat [Regex a] | Alt [Regex a] | Empty 
>     deriving (Eq, Ord)

> instance Show a => Show (Regex a) where
>     show = toString (const False)

> toString thisNeedsParens regex
>              = (if thisNeedsParens regex then putParens else id)
>                $ case regex
>                  of Empty -> ""
>                     Lit a -> show a
>                     Cat as -> join . map down $ as
>                     Alt as -> intercalate ("\\|") . map down $ as
>                     Kleene a -> (++"*") . down $ a
>     where down = toString (thatNeedsParens regex)
>           putParens s = "\\("++s++"\\)"

> -- thatNeedsParens _ _ = True
> thatNeedsParens = (<) `on` stub

> stub :: Regex a -> Regex ()
> stub (Lit _) = Lit ()
> stub (Cat _) = Cat []
> stub (Kleene _) = Kleene Empty
> stub (Alt _) = Alt []



Remove duplicates in a list in O(n * log n):

> nubs = S.toList . S.fromList

> simplify :: (Ord digit, Eq digit, Show digit
>             ,Ord state, Eq state, Show state) =>
>             state -> state -> Table state digit -> Regex digit
> simplify z0 zn table = (if z0 == zn then Kleene else id) . Alt . sort . map (\(_,a,_) -> a) . filter (\(z0',_,z1) -> z0'==z0 && z1==zn) $ simplest
>     where states = nubs . join . map (\(z0,_,z1) -> [z0,z1]) $ table
>           rest = filter (/=zn) . filter (/=z0) $ states
>           simplest = foldr simplify1 table rest
> simplify1 :: (Ord digit, Eq digit, Show digit
>             ,Ord state, Eq state, Show state) =>
>             state -> Table state digit -> Table state digit
> simplify1 rm allTable = nubsT (normals ++ news)
>     where classify (z0,a,z1) = (z0 == rm, z1 == rm)
>           fc a = filter ((==a) . classify) $ allTable
>           loops = Kleene . Alt . sort . map (\(_,a,_) -> a) .
>                   fc $ (True, True)
>           normals = fc (False, False)
>           in_rm = fc (False, True)
>           out_rm = fc (True, False)
>           news = connect <$> in_rm <*> out_rm
>           connect (z0,a,_) (_,b,z1) = (z0,Cat [a, loops, b],z1)


