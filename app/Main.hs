module Main (main) where

import Data.List (intercalate)
import Options.Applicative
import System.Exit (die)

import Layers (LayerSettings (..), buildLayers, validateSettings)
import LSystem (LSystem, systems, terDragon)
import Obj (meshFromLayers, renderObj)

data Options = Options
  { optSystem :: LSystem
  , optSettings :: LayerSettings
  , optOutput :: Maybe FilePath
  }

main :: IO ()
main = do
  options <- execParser (info (optionsParser <**> helper) description)
  settings <- either die pure (validateSettings (optSettings options))
  let obj = renderObj (meshFromLayers (buildLayers (optSystem options) settings)) ++ "\n"
  maybe (putStr obj) (`writeFile` obj) (optOutput options)
  where
    description = fullDesc <> progDesc "Generate a surface showing an L-system fractal developing, as an OBJ mesh"

optionsParser :: Parser Options
optionsParser =
  Options
    <$> option
      systemReader
      ( long "system"
          <> short 's'
          <> metavar "NAME"
          <> value (snd defaultSystem)
          <> help ("One of " ++ systemNames ++ " (default: " ++ fst defaultSystem ++ ")")
      )
    <*> settingsParser
    <*> optional
      ( strOption
          ( long "output"
              <> short 'o'
              <> metavar "FILE"
              <> help "File to write the mesh to (default: stdout)"
          )
      )

settingsParser :: Parser LayerSettings
settingsParser =
  LayerSettings
    <$> intOption "start" 1 "First iteration to include"
    <*> intOption "end" 5 "Last iteration to include"
    <*> intOption "smoothness" 2 "How much to round off each corner"

intOption :: String -> Int -> String -> Parser Int
intOption name defaultValue description =
  option auto (long name <> metavar "N" <> value defaultValue <> showDefault <> help description)

systemReader :: ReadM LSystem
systemReader = eitherReader $ \name ->
  maybe (Left ("unknown system, expected one of " ++ systemNames)) Right (lookup name systems)

systemNames :: String
systemNames = intercalate ", " (map fst systems)

defaultSystem :: (String, LSystem)
defaultSystem = ("terdragon", terDragon)
