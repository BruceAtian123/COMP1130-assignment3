module Options where

import Data.Semigroup ((<>))
import Options.Applicative hiding (helper)

data Options = Options
  { timeout :: Int
  , playTo :: Int
  , human :: Bool
  }

options :: Parser Options
options =
  Options <$>
  option
    auto
    (long "timeout" <> short 't' <> showDefault <> value 2 <>
     help "How many seconds before times out.") <*>
  option
    auto
    (long "playTo" <> short 'p' <> showDefault <> value 3 <>
     help "How many points to play to.") <*>
  switch (long "human" <> short 'h' <> help "If there is a human player.")

helper :: Parser (a -> a)
helper =
  abortOption ShowHelpText $
  mconcat [long "help", help "Show this super helpful help text", hidden]

argumentHandler :: (Options -> IO ()) -> IO ()
argumentHandler = (execParser opts >>=)
  where
    opts =
      info
        (options <**> helper)
        (fullDesc <> progDesc "Run the Backgammon game with some options." <>
         header "Backgammon - an AI game for COMP1100/1130.")
