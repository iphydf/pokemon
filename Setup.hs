import           System.Directory     (doesFileExist)

import           Data.ProtoLens.Setup
import           Distribution.Simple

main = do
  needProto <- not <$> doesFileExist "dist/build/autogen/Proto/Pokemon.hs"
  if needProto
    then defaultMainGeneratingProtos "protos/src"
    else defaultMain
