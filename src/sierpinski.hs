import Graphics.Gloss
import System.IO.Unsafe
import System.Environment
import Data.Char

-- | Main entry point to the application.
-- | module Main where

-- | The main entry point.

main =  animate (InWindow "Sierpinski Triangles" (500, 650) (20,  20))
		black (picture_sierpinski getDegree)

getDegree :: Int
getDegree = (digitToInt (head (head (unsafePerformIO (getArgs)))))

side :: Float
side = 100.0

	
-- Sierpinski Triangles
picture_sierpinski :: Int -> Float -> Picture	
picture_sierpinski degree time
	= sierpinski degree time (aquamarine)

-- Base of triangles
base_tri :: Color -> Picture
base_tri color 
	= Color color
	$ Polygon [
	((-(side/2)),0),
        ((side/2),0),
        (0,(-(((sqrt 3.0)/2)*side)))]


		
sierpinski :: Int -> Float -> Color -> Picture
sierpinski 0 time color = base_tri color
sierpinski n time color
 = let inner
            = Scale 0.5 0.5
            $ sierpinski (n-1) time (mix color)
   in   Pictures
                [base_tri color
                , Translate (-(side/2)) (-((((sqrt 3.0)/2)*side)/2)) $ inner
                , Translate ((side/2)) (-((((sqrt 3.0)/2)*side)/2)) $ inner
                , Translate (0) (((((sqrt 3.0)/2)*side)/2)) $ inner] 

--
mix :: Color -> Color
mix c = mixColors 1 2 red c
