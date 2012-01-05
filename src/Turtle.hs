-- | The 'Turtle' deals with the graphical operations and display.
module Turtle where

import Graphics.Gloss

-- | Turtle constructor.
data Turtle = Turtle { location :: Point,  -- ^ position
                       direction :: Float, -- ^ direction in degrees
                       pen_colour :: Color,-- ^ pen color
                       pen_down :: Bool }  -- ^ whether the pen is down.
    deriving Show -- for testing purposes

-- | Converts from degrees to radians.
rad :: Float -> Float
rad r = r*(pi/180)

-- | The start position of turtles.
start = Turtle (0,0) 0 white True

-- | A 'Command' takes a Turtle and returns a resultant 'Turtle' and 'Picture'
type Command = Turtle -> (Turtle, Picture)

-- | 'fd' moves a 'Turtle' a number of pixels forward.
fd :: Float -> Command
fd dist t = do let (x,y) = location t 
               let x' = x + dist * sin (rad (direction t))
               let y' = y + dist * cos (rad (direction t))
               moveTo x' y' t

-- | 'bk' moves a 'Turtle' a number of pixels backward.
bk :: Float -> Command
bk dist t = fd (-dist) t

-- | 'rt' turns a 'Turtle' a number of degrees to the right.   
rt :: Float -> Command
rt angle t = (t { direction = bound (angle + direction t) }, Blank)
   where bound ang | ang > 360 = bound (ang - 360)
                   | ang < 0 = bound (ang + 360)
                   | otherwise = ang

-- | 'lt' turns a 'Turtle' a number of degrees to the left.
lt :: Float -> Command
lt angle t = rt (-angle) t

-- | 'moveTo' moves a 'Turtle' to an absolute position on the grid.
moveTo :: Float -> Float -> Command
moveTo x y t = (t { location = (x, y) }, pic)
    where
        (x', y') = location t
        pic | pen_down t = (Color (pen_colour t) (Line [(x',y'), (x,y)]))
            | otherwise = Blank

-- | 'jumpTo' jumps a 'Turtle' to an absolute position on the grid.
jumpTo :: Float -> Float -> Command
jumpTo x y t = nothing (t { location = (x, y) })

-- | 'penUp' takes a 'Turtle's pen up.
penUp :: Command
penUp t = nothing t { pen_down = False }

-- | 'penUp' takes a 'Turtle's pen down.
penDown :: Command
penDown t = nothing t { pen_down = True }

-- | 'chgColor' changes a 'Turtle's pen color.
chgColor :: Color -> Command
chgColor c t = nothing t { pen_colour = c}

-- | 'nothing' does not alter the state of the turtle and returns a blank picture.
nothing :: Command
nothing t = (t, Blank)

-- | Combines two 'Command's.
(+>) :: Command -> Command -> Command
(+>) c1 c2 t = let (t', p1) = c1 t
                   (t'', p2) = c2 t' in
                   (t'', Pictures [p1, p2])

-- | 'runTurtle' executes a command on the default 'Turtle' and displays it in a window.
runTurtle :: Command -> IO ()
runTurtle cmd = displayInWindow "Turtle" (640,480) (50,50) black
                (snd (cmd start))

