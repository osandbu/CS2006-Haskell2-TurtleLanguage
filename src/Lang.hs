{-# LANGUAGE FlexibleInstances #-}
-- | Defines the different types of constructs of the language and how they are executed and evaluated. Includes mathematical and boolean expressions, turtle commands and function-definitions. Converts 'Program's to 'Command's which are executed and displayed in 'Turtle'.
module Lang where

import Graphics.Gloss.Color
import Turtle

-- | Monadic 'instance' which makes a function call on an 'Either' 'String' 'a' to return the function called on the 'Right' value if that is the type, otherwise it will return the original 'Left' value.
instance Monad (Either String) where
    return v = Right v
    fail s   = Left s
    x >>= f = case x of
        Left s  -> Left s
        Right v -> f v

-- | Data constructors for mathematical 'Expr'essions.
data Expr = Val Float
          | Var String
          | Add Expr Expr
          | Mul Expr Expr
          | Sub Expr Expr
          | Div Expr Expr
  deriving Show

-- | Data constructors for turtle commands.
data TurtleCmd = 
    Fd Expr -- ^ Move turtle forward 'Expr' pixels.
    | Bk Expr -- ^ Move turtle backwards 'Expr' pixels.
    | Rt Expr -- ^ Turn turtle 'Expr' degrees to the right.
    | Lt Expr -- ^ Turn turtle 'Expr' degrees to the left.
    | PenUp   -- ^ Make the pen go up.
    | PenDown -- ^ Make the pen go down.
    | ChgColor Expr Expr Expr -- ^ Change the pen color to a given RBG color.
    | MoveTo Expr Expr -- ^ Move turtle to an absolute position on the grid.
    | JumpTo Expr Expr -- ^ Make turtle jump to an absolute position on the grid. Not drawing a line between the start and end points.
  deriving Show

-- | Data constructor for boolean expressions statements.
data Condition = LessT Expr Expr -- ^ Less than expression.
             | LessTE Expr Expr  -- ^ Less than or equals expression.
             | GreatT Expr Expr  -- ^ Greater than expression.
             | GreatTE Expr Expr -- ^ Greater than or expression.
             | Equals Expr Expr  -- ^ Equals expression.
  deriving Show

-- | 'Program' data constructor.
data Program = T TurtleCmd -- ^ A single 'TurtleCmd'
    | Call String [Expr]   -- ^ Call to a function given some arguments.
    | Seq [Program]        -- ^ 'Seq'uence of 'Programs'
    | Rep Int Program      -- ^ Repeat a program a number of times.
    | IfElse Condition Program Program -- ^ If-else-statement.
    | If Condition Program -- ^ If-statement.
    | Assign String Expr   -- ^ Assign a variable a value.
  deriving Show

-- | 'Function's take a list of 'String's 'arguments' and have a 'Program' 'definition'
data Function = Fn { arguments :: [String],
                     definition :: Program }
  deriving Show

-- | A list of tuples with 'Function' identifiers and 'Definitions'.
type Definitions = [(String, Function)]
-- | A list of tuples with local variable identifiers and float values.
type Locals = [(String, Float)]

-- | 'eval' evaluates an expression to a float.
eval :: Locals -- ^ Local variables to be used.
     -> Expr   -- ^ 'Expr' to be evaluated.
     -> Float  -- ^ resultant 'Float' value
eval locs expr = case expr of
                   Val f   -> f
                   Var v   -> case lookup v locs of
                                Just val -> val
                                Nothing  -> error ("No variable named " ++ v)
                   Add a b -> eval locs a + eval locs b
                   Sub a b -> eval locs a - eval locs b
                   Mul a b -> eval locs a * eval locs b
                   Div a b -> eval locs a / eval locs b

-- | 'evalCondition' Evalutes a boolean expression to a 'Bool'
evalCondition :: Locals -- ^ Local variables to be used.
    -> Condition        -- ^ 'Condition' to be evaluted.
    -> Bool             -- ^ resultalt 'Bool'
evalCondition locs c = case c of
                LessT a b  -> (eval locs a) < (eval locs b)
                LessTE a b -> (eval locs a) <= (eval locs b)
                GreatT a b  -> (eval locs a) > (eval locs b)
                GreatTE a b  -> (eval locs a) >= (eval locs b)
                Equals a b  -> (eval locs a) == (eval locs b)

-- | 'interp' interprets a program and returns either a 'Command' or a 'String' error message.
interp :: Definitions -- ^ List of 'Function's which may be called.
       -> Locals      -- ^ List of local variables.
       -> Program     -- ^ 'Program' to be interpreted.
       -> Either String Command -- ^ Either a 'Command' or a 'String' error message if something goes wrong.
interp defs locs prog = case prog of
        T cmd -> case cmd of
                   Fd e -> Right (fd (eval locs e))
                   Bk e -> Right (bk (eval locs e))
                   Rt e -> Right (rt (eval locs e))
                   Lt e -> Right (lt (eval locs e))
                   PenUp   -> Right penUp
                   PenDown -> Right penDown
                   ChgColor r g b -> 
                          do let r' = round (eval locs r)
                             let g' = round (eval locs g)
                             let b' = round (eval locs b)
                             Right (chgColor (makeColor8 r' g' b' 255))
                   MoveTo x y -> Right (moveTo (eval locs x) (eval locs y))
                   JumpTo x y -> Right (jumpTo (eval locs x) (eval locs y))
        Call f args -> case lookup f defs of
            Just (Fn pargs prog') -> 
                       interp defs (zip pargs (map (eval locs) args)) prog'
            Nothing -> Left ("No such function " ++ show f)
        --Seq progs -> foldr1 (interp defs locs +>) progs
        Seq []     -> Right nothing
        Seq [prog] -> interp defs locs prog
        Seq (x:xs) -> do x' <- interp defs locs x
                         y' <- interp defs locs (Seq xs)
                         Right (x' +> y')
        Rep 1 cmd -> interp defs locs cmd
        Rep n cmd -> if n > 1 then do x <- interp defs locs cmd
                                      y <- interp defs locs (Rep (n-1) cmd)
                                      Right (x +> y)
                              else Right nothing
        If c x -> if (evalCondition locs c) then interp defs locs x else Right nothing
        IfElse c x y -> if (evalCondition locs c) then interp defs locs x else interp defs locs y
