module FileSystem.LoadFile(loadFile) where

import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import qualified Data.Sequence as S
import           Buffer

loadFile :: FilePath -> IO (Maybe (S.Seq Buffer))
loadFile fp = do
  content <- T.IO.readFile fp -- point of potential failure (also improvement)
  return $ Just $ extractBuffer content

extractBuffer :: T.Text -> S.Seq Buffer
extractBuffer t = 
  let ls = T.lines t
  in S.fromList $ map (\l -> Buffer (textToSeq l) S.Empty 0) ls

textToSeq :: T.Text -> S.Seq Char
textToSeq = S.fromList . T.unpack
