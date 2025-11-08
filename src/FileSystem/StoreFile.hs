module FileSystem.StoreFile (saveFile) where

import Buffer
import Data.Foldable
import qualified Data.Sequence as S
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

makeText :: S.Seq Buffer -> T.Text
makeText bs =
  let ls = toList bs
      ls' = map (T.pack . getString) ls
   in T.unlines ls'

saveFile :: FilePath -> S.Seq Buffer -> IO ()
saveFile fp bs = T.IO.writeFile fp (makeText bs)
