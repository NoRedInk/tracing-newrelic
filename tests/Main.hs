module Main (main) where

import Test.Tasty.HUnit
import Test.Tasty as Tasty
import Control.Monad.IO.Class (liftIO)
import FreeAfter(FreeAfter(..), freeAfter)
import Data.IORef as IORef

main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "FreeAfter"
  [ testCase "Monad instance ensures cleanup operations happen at the end" $ do
      log' <- IORef.newIORef []
      freeAfter $ do
        liftIO $ writeLine log' "A"
        _ <- withCleanup log' "B"
        liftIO $ writeLine log' "C"
        _ <- withCleanup log' "D"
        liftIO $ writeLine log' "E"
        -- Log doesn't contain clean-up steps yet.
        liftIO $ assertLogEquals log'
          ["A"
          ,"C"
          ,"E"
          ]
      -- After `freeAfter` log contains the cleanup steps.
      assertLogEquals log'
        ["A"
        ,"C"
        ,"E"
        ,"Cleanup after: D"
        ,"Cleanup after: B"
        ]
  , testCase "Applicative instance ensures cleanup operations happen at the end" $ do
      log' <- IORef.newIORef []
      freeAfter $ do
        (\_ _ -> ())
          <$>
            (do
              liftIO $ writeLine log' "A"
              withCleanup log' "B"
            )
          <*>
            (do
              liftIO $ writeLine log' "1"
              withCleanup log' "2"
            )
        -- Log doesn't contain clean-up steps yet.
        liftIO $ assertLogEquals log'
          ["A"
          ,"1"
          ]
      -- After `freeAfter` log contains the cleanup steps.
      assertLogEquals log'
        ["A"
        ,"1"
        ,"Cleanup after: 2"
        ,"Cleanup after: B"
        ]
  ]

type Log = IORef.IORef [String]

assertLogEquals :: Log -> [String] -> IO ()
assertLogEquals log' expectedLines = do
  lines' <- IORef.readIORef log'
  assertEqual
    "log contains expected lines"
    expectedLines
    lines'

withCleanup :: Log -> String -> FreeAfter () String
withCleanup log' str = FreeAfter $ \f -> do
  f str
  writeLine log' ("Cleanup after: " ++ str)

writeLine :: Log -> String -> IO ()
writeLine log' line =
  IORef.atomicModifyIORef' log' (\lines' -> (lines' ++ [line] ,()))
