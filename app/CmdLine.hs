module CmdLine (Options(..), getOptions) where

import Options.Applicative

data Options = Options {
      optionsOutput :: FilePath
    }

getOptions :: IO Options
getOptions = execParser $ info (parseOptions <**> helper) $ mconcat [
      fullDesc
    , header "Utility for experimenting with scroll layout"
    ]

parseOptions :: Parser Options
parseOptions = Options
    <$> (strOption $ mconcat [
            short 'o'
          , help "Output filename"
          ])

