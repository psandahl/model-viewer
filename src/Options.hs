module Options
    ( Options (..)
    , options
    ) where

import           Data.Semigroup      ((<>))
import           Options.Applicative

data Options = Options
    { fullscreenMode :: !Bool
    , modelFile      :: !FilePath
    } deriving Show

options :: IO Options
options = execParser parserInfo

parserInfo :: ParserInfo Options
parserInfo =
    info (parser <**> helper)
        ( fullDesc
        <> progDesc "Display 3D models"
        <> header "model-viewer - a simple 3D model viewer"
        )

parser :: Parser Options
parser = Options
    <$> switch (
            long "fullscreen"
         <> short 'f'
         <> help "Run in fullscreen mode"
    )
    <*> strOption (
        long "model"
     <> short 'm'
     <> metavar "MODEL"
     <> help "Model specification file path"
    )
