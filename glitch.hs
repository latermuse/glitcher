module Main where

-------------------------------------------------------------------------------------
-- 
--  Glitcher
--  
--  Author: Ron Watkins (latermuse)
--  Date:   May 31, 2013
--  Description: 
--    This program takes an input filename and an output filename passed as 
--    arguments, then uses additive and subtractive glitching to 
--    create a new image and output to the output filename.
--
--    example usage:
--      ./glitch "/home/ron/funny.jpg" "/home/ron/funny-glitched.jpg"
--      ./glitch "/home/music/siesta.mp3" "/home/music/siesta-glitched.mp3"
--
--    There currently is not any protection against inputting invalid arguments.
--
--    Use at your own risk.
--
--
--  License:
--    This program is free software: you can redistribute it and/or modify
--    it under the terms of the GNU General Public License as published by
--    the Free Software Foundation, either version 3 of the License, or
--    (at your option) any later version.
--    
--    This program is distributed in the hope that it will be useful,
--    but WITHOUT ANY WARRANTY; without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--    GNU General Public License for more details.
--    
--    You should have received a copy of the GNU General Public License
--    along with this program.  If not, see <http://www.gnu.org/licenses/>.
--
-------------------------------------------------------------------------------------


import qualified Data.ByteString.Lazy as B
import System.Environment
import Control.Applicative
import Control.Arrow
import Data.Int 
import Data.Monoid 
import System.Random


-------------------------------------------------------------------------------------
-- main reads arguments passed to it from the command line.
-- It then begins the program logic, completes it, and writes the file to the 
-- output filename.
-------------------------------------------------------------------------------------

main = do
  args                 <- getArgs                     -- Get filename passed to the program
  file                 <- B.readFile (inputFile args) -- read the first filename
  let fileLength       = B.length file                -- calculate the length of the file
  modified             <- modLoop 5 file              -- perform 5 glitches to the input
  
  -- Start debugging data
  putStrLn             $ "length of original file " ++ show fileLength          
  putStrLn             $ "length of modified file " ++ show (B.length modified)
  -- End debugging data

  B.writeFile (outputFile args) modified              -- write glitched data to the output file

 where
  outputFile           = head . tail 
  inputFile            = head 


-------------------------------------------------------------------------------------
-- modLoop loops the glitches over itself until i == 0
-------------------------------------------------------------------------------------

modLoop i x 
  | i == 0             = return x
  | otherwise          = do
                            modded <- mod' x 
                            modLoop (i - 1) modded
 where
  mod' x = modify x <$> getRandVars (B.length x)


-------------------------------------------------------------------------------------
-- 'modify' uses some basic logic to send the bytestring to get glitched by one of
-- four different methods:
--    additive
--    subtractive
--    reverse subtractive
--    reverse additive
-------------------------------------------------------------------------------------

modify file  (i,r,x,y) 
  | y > 5              = if even y then sub else subR
  | otherwise          = if even y then adi else adiR 
 where
  sub                  = subtractive i r file
  subR                 = B.reverse $ subtractive i r (B.reverse file)
  adi                  = additive i r x y file
  adiR                 = B.reverse $ additive i r x y (B.reverse file)
  
  
-------------------------------------------------------------------------------------
-- getRandVars uses randomRIO, gets four different random numbers and returns 
-- them for processing
-------------------------------------------------------------------------------------

getRandVars      ::    Int64         ->               -- The length of the bytestring
                       IO (Int64,Int64,Int64,Int64)   -- Return 4-tuple of random Int64s

getRandVars l    = do
  i                    <- randomRIO (1, (l - 1))      -- Starting point for cut/subtract
  r                    <- randomRIO ( l `div` 100, 
                                      l `div` 3  )    -- Amount of bits to cut/subtract
  x                    <- randomRIO (1, (l - 1))      -- Where to begin pasting
  y                    <- randomRIO (1, 10)           -- Amount of times to paste
  return (i,r,x,y)                                    -- Return


-------------------------------------------------------------------------------------
-- Additive will copy 'r' bits from a ByteString starting at 'i', then append it
-- 'x' amount of times at 'y'
-------------------------------------------------------------------------------------

additive         ::    Int64         ->               -- Where to begin copying
                       Int64         ->               -- How many bits to copy
                       Int64         ->               -- Where to begin pasting
                       Int64         ->               -- How many times to paste
                       B.ByteString  ->               -- The data to modify
                       B.ByteString                   -- The modified data

additive 
  i r x y s      =     uncurry (B.append)             -- Uncurry the tuple, appending snd to fst
                     . (id *** (B.append pasteData))  -- Id first, append pasteData to snd
                     $ B.splitAt i s                  -- Split s at i, creating a tuple

  where
  
    -- The data that will be copied
    copy         ::    B.ByteString
    copy         =     B.take r                       -- take 'r' amount of bits 
                     . snd                            -- use the second half of the tuple
                     $ B.splitAt i s                  -- split at 'i', creating a tuple

    -- Repeat copied data y amount of times; ready to be pasted
    pasteData    =     B.take (y * r)                 -- The amount of data to paste
                     $ cycle' copy                    -- Cycle to create an infinite ByteString


-------------------------------------------------------------------------------------
-- Subtractive will remove 'r' bits from a ByteString starting at 'i' 
-------------------------------------------------------------------------------------

subtractive      ::    Int64         ->               -- Where to begin subtracting
                       Int64         ->               -- How many bits to remove 
                       B.ByteString  ->               -- The data to modify
                       B.ByteString                   -- The modified data

subtractive i r  =     uncurry (B.append)             -- Uncurry the tuple (appending snd to fst)
                     . (id *** B.drop r)              -- Id first half of tuple, subtract r from second
                     . B.splitAt i                    -- Split at i, creating a tuple

-------------------------------------------------------------------------------------
-- Miscellaneous functions
-------------------------------------------------------------------------------------

-- Custom cycle function for bytestrings
cycle'           ::    B.ByteString -> 
                       B.ByteString
cycle' xs                
  | B.null xs    =     B.empty
  | otherwise    =     xs' 
 where 
  xs'            =     xs `B.append` xs'


