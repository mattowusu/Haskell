import Prelude hiding (Num)
import qualified Prelude (Num)

type Variable = String
type State = Variable -> Boolean
type Predicate = Variable -> Bool

data Quantified = U Variable Quantified | E Variable Quantified | B Boolean

-- variable, terminal, impies, and, not, or
data Boolean = Var Variable | Terminal Bool | Boolean :=> Boolean
 | Boolean :^ Boolean | Not Boolean



update :: State -> Boolean -> Variable -> State
update st a v x
   | x==v = a
   | otherwise = st x

-- evaluate a simple boolean expression with variable assignment
eval_boolean :: Boolean -> State -> Bool
eval_boolean (Var x) s = eval_boolean (s x) s
eval_boolean (Terminal tt) s = tt
eval_boolean (b1 :=> b2) s = (not (eval_boolean b1 s)) || (eval_boolean b2 s)
eval_boolean (b1 :^ b2) s =  (eval_boolean b1 s) && (eval_boolean b2 s)
eval_boolean (Not b1) s =  not (eval_boolean b1 s)


-- evaluate a boolean expression with quantifiers
eval_quantified :: Quantified -> State -> Bool
eval_quantified (U v q) s = (eval_quantified q (update s (Terminal True) v)) && (eval_quantified q (update s (Terminal False) v))
eval_quantified (E v q) s = (eval_quantified q (update s (Terminal True) v)) || (eval_quantified q (update s (Terminal False) v))
eval_quantified (B b) s = eval_boolean b s

-- test things
simple_bool = (Terminal True) :=> (Terminal True)
example_quantified = (U "x" (B (Var "x")))
s :: State
s "x" = Terminal True
s "y" = Terminal False


-- there exists a student (x) such that, if they fail the exam, all other students (y) will also fail
expression = (E "x" (U "y" (B ((Not (Var "x")) :=> (Not (Var "y"))))))
