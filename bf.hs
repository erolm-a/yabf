{-|
    Copyright (c) 2020 Enrico Trombetta

    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all
    copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
    SOFTWARE.
-}

{-| This script parses the infamous Brainfuck language.
 -
 - Features:
 -  - It parses the text from stdin until it reads an EOF.
 -  - Uses Text and ByteString rather than vanilla String
 -  - Does not rely on parsec. The language has a very simple grammar, a stack suffices.
 -  - Of course, it uses monadic states.
 -}

import           Control.Monad
import           Control.Monad.Loops
import           Control.Monad.ST
import           Control.Monad.State
import qualified Data.ByteString               as BS
                                                ( ByteString
                                                , filter
                                                , elem
                                                , getContents
                                                , length
                                                , index
                                                )
import           Data.ByteString.Internal      as BS
                                                ( c2w
                                                , w2c
                                                )
import           Data.ByteString.UTF8          as BSU
import           Data.Array
import           Data.Array.ST
import           Data.Array.IO
import           Data.Word


-- |- A BF instruction.
data BFInstruction = BFLeft | BFRight | BFIncr | BFDecr | BFWrite | BFRead | BFLoop Int | BFEndLoop Int
                    deriving (Eq, Show)

-- | An evaluated program is an array of BFInstruction , once the program has been fully parsed.
type EvaluatedProgram = Array Int BFInstruction

-- A convenience stack while parsing. See `annotate` for details
type ParsingStack = [Int]

push :: Int -> StateT ParsingStack (ST s) ()
push n = state $ \xs -> ((), n : xs)

pop :: StateT ParsingStack (ST s) (Int)
pop = state $ \(x : xs) -> (x, xs)

annotate
  :: BS.ByteString -> StateT ParsingStack (ST s) (STArray s Int BFInstruction)
annotate code = do
  let codeSize = BS.length code
  array <- lift $ newArray (0, codeSize - 1) BFLeft

  forM_
    [0 .. codeSize - 1]
    (\idx -> do
      let currentChar = BS.w2c $ code `BS.index` idx
      lift $ writeArray array idx (parse currentChar)

      when (currentChar == '[') $ push idx

      when (currentChar == ']') $ do
        latest <- pop
        lift $ writeArray array idx (BFEndLoop latest)
    )

  forM_
    [codeSize - 1, codeSize - 2 .. 0]
    (\idx -> do
      let currentChar = BS.w2c $ code `BS.index` idx
      when (currentChar == ']') $ push idx
      when (currentChar == '[') $ do
        latest <- pop
        lift $ writeArray array idx (BFLoop $ latest + 1)
    )

  return array
 where
  parse ch = case ch of
    '+' -> BFIncr
    '-' -> BFDecr
    '>' -> BFLeft
    '<' -> BFRight
    '.' -> BFWrite
    ',' -> BFRead
    '[' -> BFLoop 0
    ']' -> BFEndLoop 0
    _   -> error $ "Unexpected character " ++ (ch : []) ++ "\n"

type ProgramCounter = Int

-- |The parsing stack denotes where to jump back
type Memory = Array Int Word8
type StatefulMemory = IOArray Int Word8

buildMemory :: Int -> IO (IOArray Int Word8)
buildMemory nbytes = newArray (0, nbytes - 1) (fromIntegral 0 :: Word8)

-- |BF mandates memory should be at least 30K bytes large. 
memory = buildMemory 30000

type TapeHead = Int

type BFState = (ProgramCounter, TapeHead)

-- |Evaluate an instruction, statelessly
eval :: EvaluatedProgram -> StateT BFState (IO) ()
eval evaluatedCode = do
  memory <- lift $ memory
  let codeSize = Prelude.length evaluatedCode

  -- extract the state, pick only the ip, check if it's time to halt
  whileM_ ((< codeSize) `fmap` fst `fmap` get) $ do
    (ip, head) <- get
    let instruction = evaluatedCode ! ip
    newState <- lift $ exec (ip, head) instruction memory
    put newState


-- Wrappers for reading and writing a single char
getWord8 :: IO (Word8)
getWord8 = BS.c2w `fmap` getChar

putWord8 :: Word8 -> IO ()
putWord8 word = putChar $ BS.w2c word

exec :: BFState -> BFInstruction -> StatefulMemory -> IO (BFState)
exec (instructionPointer, head) instruction memory = do
  currentCell <- readArray memory head
  case instruction of
    BFIncr -> do
      writeArray memory head (currentCell + 1)
      return (instructionPointer + 1, head)
    BFDecr -> do
      writeArray memory head (currentCell - 1)
      return (instructionPointer + 1, head)
    BFLeft  -> return (instructionPointer + 1, head + 1)
    BFRight -> return (instructionPointer + 1, head - 1)
    BFWrite -> do
      putWord8 currentCell
      return (instructionPointer + 1, head)
    BFRead -> do
      newCell <- getWord8
      writeArray memory head newCell
      return (instructionPointer + 1, head)
    BFLoop location -> if currentCell /= 0
      then return (instructionPointer + 1, head)
      else return (location, head)

    BFEndLoop location -> return (location, head)

strip :: ByteString -> ByteString
strip = BS.filter (\ch -> BS.elem ch (BSU.fromString "[].,<>+-"))

main = do
  source <- BS.getContents

  let stripped = strip source
      compiled = runSTArray $ evalStateT (annotate stripped) []
  runStateT (eval compiled) (0, 0)
