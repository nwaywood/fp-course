{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.FileIO where

import Course.Core
import Course.Applicative
import Course.Monad
import Course.Functor
import Course.List

{-

Useful Functions --

  getArgs :: IO (List Chars)
  putStrLn :: Chars -> IO ()
  readFile :: FilePath -> IO Chars
  lines :: Chars -> List Chars
  void :: IO a -> IO ()

Abstractions --
  Applicative, Monad:

    <$>, <*>, >>=, =<<, pure

Tuple Functions that could help --

  fst :: (a, b) -> a
  snd :: (a, b) -> b
  (,) :: a -> b -> (a, b)

Problem --
  Given a single argument of a file name, read that file,
  each line of that file contains the name of another file,
  read the referenced file and print out its name and contents.

Consideration --
  Try to avoid repetition. Factor out any common expressions.

Example --
Given file files.txt, containing:
  a.txt
  b.txt
  c.txt

And a.txt, containing:
  the contents of a

And b.txt, containing:
  the contents of b

And c.txt, containing:
  the contents of c

To test this module, load ghci in the root of the project directory, and do
    >> :main "share/files.txt"

Example output:

$ ghci
GHCi, version ...
Loading package...
Loading ...
[ 1 of 28] Compiling (etc...
...
Ok, modules loaded: Course, etc...
>> :main "share/files.txt"
============ share/a.txt
the contents of a

============ share/b.txt
the contents of b

============ share/c.txt
the contents of c

-}

-- Given the file name, and file contents, print them.
-- Use @putStrLn@.
printFile ::
  FilePath
  -> Chars
  -> IO ()
printFile path cs = putStrLn $ path ++ " " ++ cs

-- Given a list of (file name and file contents), print each.
-- Use @printFile@.
printFiles ::
  List (FilePath, Chars)
  -> IO ()
printFiles files =
  -- void $ sequence $ (\(name, contents) -> printFile name contents) <$> files
  void $ sequence $ (uncurry printFile) <$> files

-- Given a file name, return (file name and file contents).
-- Use @readFile@.
getFile ::
  FilePath
  -> IO (FilePath, Chars)
getFile filename =
    -- (\c -> (filename, c)) <$> readFile filename
    -- infix notation and eta reduce
    -- ((,) filename) <$> readFile filename
    lift2 (<$>) (,) readFile filename


-- Given a list of file names, return list of (file name and file contents).
-- Use @getFile@.
getFiles ::
  List FilePath
  -> IO (List (FilePath, Chars))
getFiles l = sequence $ getFile <$> l


-- Given a file name, read it and for each line in that file, read and print contents of each.
-- Use @getFiles@, @lines@, and @printFiles@.
run ::
  FilePath
  -> IO ()
run filename =
    readFile filename >>= getFiles . lines >>= printFiles
    -- do
        -- c <- readFile filename
        -- let blah = lines c :: List FilePath
        -- let blah2 = getFiles blah :: IO (List (FilePath, Chars))
        -- v <- blah2
        -- let blah3 = printFiles v
        -- blah3
    -- do
        -- c <- readFile filename
        -- v <- getFiles (lines c)
        -- printFiles v


-- /Tip:/ use @getArgs@ and @run@
main ::
  IO ()
main =
    -- STEP 1
    -- do
    --     a <- getArgs
    --     undefined
    -- STEP 2
    do
        a <- getArgs
        case a of
            Nil -> putStrLn "command line args please"
            r :. _ -> run r


----

-- Was there was some repetition in our solution?
-- ? `sequence . (<$>)`
-- ? `void . sequence . (<$>)`
-- Factor it out.
