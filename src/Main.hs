import Graphics.Gloss
import System.Random

-- | Main entry point to the application.
-- | module Main where

-- | The main entry point.

main =  animate (InWindow "Wheel" (500, 650) (20,  20))
		white (picture 5)



-- | Tree Fractal.
--	Based on ANUPlot code by Clem Baker-Finch.
--	




-- The picture is a tree fractal, graded from brown to green
picture :: Int -> Float -> Picture	
picture degree time
	= wheel degree time (dim $ dim blue)


-- Base of circles
base :: Color -> Picture
base color 
	= Color color
	$ Circle 100


-- start the circle fractal.
wheel 	:: Int 		-- Fractal degree
	-> Float	-- time
	-> Color 	-- Color for the circle
	-> Picture

wheel 0 time color = base color
wheel n time color 
 = let	concentric 
		= Rotate (sin time*pi)
		$ Scale 0.5 0.5 
		$ wheel (n-1) (- time) (mix color)
   in	Pictures
		[ base color
		, Line [(0,0),(0,time*pi)]
		, Translate (randomRIO (1, 240)) (randomRIO (1, 240)) concentric]
		




-- Mix the color with something
mix :: Color -> Color
mix c = mixColors 1 10 black c