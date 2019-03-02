import Prelude hiding (Num)
import qualified Prelude (Num)

type Variable = String
type State = Variable -> Boolean

data Quantified = Universal Variable Quantified | Existential Variable Quantified | Boolean

data Boolean = Var Variable | TruthVal Bool | Boolean :=> Boolean
 | Boolean :^ Boolean | Not Boolean

eval_boolean :: Boolean -> State -> Bool
eval_boolean (Var x) s = eval_boolean (s x) s
eval_boolean (TruthVal tt) s = tt
eval_boolean (b1 :=> b2) s = (not (eval_boolean b1 s)) || (eval_boolean b2 s)
eval_boolean (b1 :^ b2) s =  (eval_boolean b1 s) && (eval_boolean b2 s)
eval_boolean (Not b1) s =  not (eval_boolean b1 s)

-- eval_quantified :: Quantified -> Boolean
-- eval_quantified Universal
-- eval_quantified Universal
-- eval_quantified b = eval_boolean b


simple_bool = (TruthVal True) :=> (TruthVal True)

s :: State
s "x" = TruthVal True
s "y" = TruthVal False
