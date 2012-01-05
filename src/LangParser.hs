-- | 'LangParser' defines the functions which are used to parse programs in .
module LangParser where

import Graphics.Gloss.Color
import Parsing
import Lang

-- | The 'reduce' function converts a float, n, to 0.n.
reduce :: Float -> Float
reduce d | d < 1     = d
         | otherwise = reduce ( d / 10.0 )

-- | The 'float' function attempts to parse a float or integer.
float :: Parser Float
float = do space
           a <- int
           let v = fromIntegral a
           do char '.'
              b <- nat
              space
              let d = reduce (fromIntegral b)
              if v >= 0 then return (v + d) else return (v - d)
              ||| do space
                     return v

-- | The 'var' function attempts to parse a variable identifier.
var :: Parser Expr -- ^ Returns an 'Expr' of the 'Var' type.
var = do var <- identifier
         return (Var var)

-- | The 'val' function attempt to parse a 'Float'.
val :: Parser Expr -- ^ Returns an 'Expr' of the 'Var' type.
val = do f <- float
         return (Val f)

-- | The 'value' function attempts to parse a variable or value.
value :: Parser Expr -- ^ Returns an 'Expr' of the 'Var' or 'Val' type.
value = val ||| var

-- | The 'add' function attempts to parse a mathematical expression containing the plus operator.
add :: Parser Expr
add = do a <- value
         symbol "+"
         b <- value
         return (Add a b)

-- | The 'sub' function attempts to parse a mathematical expression containing the minus operator.
sub :: Parser Expr
sub = do a <- value
         symbol "-"
         b <- value
         return (Sub a b)

-- | The 'mul' function attempts to parse a mathematical expression containing the multiplication operator.
mul :: Parser Expr
mul = do a <- value
         symbol "*"
         b <- value
         return (Mul a b)

-- | The 'div' function attempts to parse a mathematical expression containing the division operator.
parseDiv:: Parser Expr
parseDiv = do a <- value
              symbol "/"
              b <- value
              return (Div a b)

-- | The 'expr' function attempts to parse a mathematical expression.
expr :: Parser Expr
expr = mul
   ||| parseDiv
   ||| add
   ||| sub
   ||| var 
   ||| val

-- | The 'parseFd' function attempts to parse a "Fd"-TurtleCmd
parseFd :: Parser TurtleCmd
parseFd = do symbol "Fd"
             e <- expr
             return (Fd e)

-- | The 'parseBk' function attempts to parse a "Bk"-TurtleCmd
parseBk :: Parser TurtleCmd
parseBk = do symbol "Bk"
             e <- expr
             return (Bk e)

-- | The 'parseRt' function attempts to parse a "Rt"-TurtleCmd
parseRt :: Parser TurtleCmd
parseRt = do symbol "Rt"
             e <- expr
             return (Rt e)

-- | The 'parseLt' function attempts to parse a "Lt"-TurtleCmd
parseLt :: Parser TurtleCmd
parseLt = do symbol "Lt"
             e <- expr
             return (Lt e)

-- | The 'parsePenUp' function attempts to parse a "PenUp"-TurtleCmd
parsePenUp :: Parser TurtleCmd
parsePenUp = do symbol "PenUp"
                return PenUp

-- | The 'parsePenDown' function attempts to parse a "PenDown"-TurtleCmd
parsePenDown :: Parser TurtleCmd
parsePenDown = do symbol "PenDown"
                  return PenDown

-- | The 'parseRGB' function attempts to parse a color expressed as a RGB triple.
parseRGB :: Parser TurtleCmd
parseRGB = do symbol "("
              r <- expr
              symbol ","
              g <- expr
              symbol ","
              b <- expr
              symbol ")"
              return (ChgColor r g b)

-- | The 'parseColor' function attempts to parse one of the predefined colors.
parseColor :: Parser Color
parseColor =  do symbol "WHITE"
                 return white
            ||| do symbol "RED"
                   return red
            ||| do symbol "GREEN"
                   return green
            ||| do symbol "BLUE"
                   return blue
            ||| do symbol "YELLOW"
                   return yellow
            ||| do symbol "CYAN"
                   return cyan
            ||| do symbol "MAGNETA"
                   return magenta
            ||| do symbol "ROSE"
                   return rose
            ||| do symbol "VIOLET"
                   return violet
            ||| do symbol "AZURE"
                   return azure
            ||| do symbol "AQUAMARINE"
                   return aquamarine
            ||| do symbol "CHARTREUSE"
                   return chartreuse
            ||| do symbol "ORANGE"
                   return orange

-- | The 'parseChgColor' function attempts to parse a ChgColor-TurtleCmd, which is "Color" followed by a RGB-triple or a predefined color.
parseChgColor :: Parser TurtleCmd
parseChgColor = do symbol "Color"
                   parseRGB ||| 
                       do color <- parseColor
                          let (r, g, b, _) = rgbaOfColor color
                          let (r', g', b') = (r*255, g*255, b*255)
                          let cmd = ChgColor (Val r') (Val g') (Val b')
                          return cmd

-- | The 'parseMoveTo' function attempts to parse a "MoveTo"-TurtleCmd
parseMoveTo :: Parser TurtleCmd
parseMoveTo = do symbol "MoveTo"
                 x <- expr
                 y <- expr
                 return (MoveTo x y)

-- | The 'parseJumpTo' function attempts to parse a "JumpTo"-TurtleCmd
parseJumpTo :: Parser TurtleCmd
parseJumpTo = do symbol "JumpTo"
                 x <- expr
                 y <- expr
                 return (JumpTo x y)

-- | The 'parseTurtleCmd' function attempts to parse a TurtleCmd
parseTurtleCmd :: Parser TurtleCmd
parseTurtleCmd = parseFd 
             ||| parseBk
             ||| parseRt
             ||| parseLt
             ||| parsePenUp
             ||| parsePenDown
             ||| parseChgColor
             ||| parseMoveTo
             ||| parseJumpTo

-- | The 'exprs' function attempts to parse a list of expressions, delimited by ','. Example: "a, b, 6, 2-5"
exprs :: Parser [Expr]
exprs = do x <- expr
           do symbol ","
              xs <- exprs
              return (x:xs)
              ||| return [x]
    ||| do return []
               
-- | The 'parseT' function attempts to parse a TurtleCmd-program
parseT :: Parser Program
parseT = do cmd <- parseTurtleCmd
            return (T cmd)

-- | The 'parseCall' function attempts to parse a function call with an identifier and zero or more arguments. Examples: "f()" or "func(a,b-2)"
parseCall :: Parser Program
parseCall = do func <- identifier
               symbol "("
               args <- exprs
               symbol ")"
               return (Call func args)

-- | The 'parseRepeat' function attempts to parse a repeat-statement. Example: "repeat 5 { Fd 1 Rt 1}"
parseRepeat :: Parser Program
parseRepeat = do symbol "repeat"
                 n <- integer
                 symbol "{"
                 cmd <- parseProgram
                 symbol "}"
                 return (Rep n cmd)

-- | The 'parseCondition' function attempts to parse a conditional-statement. Examples include: "a < 2", "b == x-5".
parseCondition :: Parser Condition
parseCondition = do a <- expr
                    symbol "<"
                    b <- expr
                    return (LessT a b)
             ||| do a <- expr
                    symbol "<="
                    b <- expr
                    return (LessTE a b)
             ||| do a <- expr
                    symbol ">"
                    b <- expr
                    return (GreatT a b)
             ||| do a <- expr
                    symbol ">="
                    b <- expr
                    return (GreatTE a b)
             ||| do a <- expr
                    symbol "=="
                    b <- expr
                    return (Equals a b)
             
-- | The 'parseFd' function attempts to parse an 'If'- or 'IfElse'-statement.
parseIf :: Parser Program
parseIf = do symbol "if"
             symbol "("
             c <- parseCondition
             symbol ")"
             symbol "{"
             x <- parseProgram
             symbol "}"
             do symbol "else"
                symbol "{"
                y <- parseProgram
                symbol "}"
                return (IfElse c x y) 
                ||| return (If c x)

-- | The 'parsePrograms' function attempts to parse a sequence of programs.
parsePrograms :: Parser [Program] -- ^ A list of programs.
parsePrograms = do x <- parseT ||| parseIf ||| parseCall
                        ||| parseRepeat
                   do xs <- parsePrograms
                      return (x:xs)
                      ||| return [x]
               ||| do return []

-- | The 'parseSeq' function attempts to parse a sequence of programs.
parseSeq :: Parser Program -- ^ A 'Seq' 'Program'.
parseSeq = do prgs <- parsePrograms
              return (Seq prgs)

-- | The 'parseProgram' function attempts to parse a program.
parseProgram :: Parser Program
parseProgram = parseSeq

-- | The 'parseArgs' function attempts to parse a number of arguments in a 'Function' definition.
parseArgs :: Parser [String]
parseArgs = do x <- identifier
               do symbol ","
                  xs <- parseArgs
                  return (x:xs)
                  ||| return [x]
           ||| do return []

-- | The 'function' function attempts to parse a 'Fn'
function :: Parser (String, Function)
function = do name <- identifier
              symbol "("
              args <- parseArgs
              symbol ")"
              symbol "{"
              def <- parseSeq
              symbol "}"
              return (name, (Fn args def))

-- | The 'script' function attempts to parse a script, i.e. a sequence of function 'Definitions'.
script :: Parser Definitions
script = do x <- function
            do xs <- script
               return (x:xs)
               ||| return [x]
        ||| do return []

-- | The 'parseScript' function attempts to parse a complete script.
parseScript :: String -- ^ 'String' to be parsed into function 'Definitions'
            -> Either String Definitions -- ^ Returns 'Either' a 'String' error message or function 'Definitions'
parseScript s = case (parse script s) of
                    [(x, [])]   -> Right x
                    [(_, rest)] -> Left ("There is more... " ++ rest)
                    []          -> Left ("Error parsing script.")

-- | The 'test' function attempts to use the given parsing function to 'parse' an input 'String'
test :: Parser a -- ^ The 'Parser' to be used.
     -> String   -- ^ The 'String' to be 'parse'd
     -> a        -- ^ The result of using the 'Parser' to 'parse' the 'String'
test func s = case (parse func s) of
                    [(x, [])]   -> x
                    [(_, rest)] -> error ("There is more... " ++ rest)
                    []          -> error ("Error parsing script.")
