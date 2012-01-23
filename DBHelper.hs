module DBHelper
(
  connectDB,
  getSql,
  allTags,
  curTags,
  fullHashes,
  updateTags,
  addTags,
  newDBHelper,
  findIncTag,
  rmHashs,
  rmTags
) where

import Database.SQLite
import Data.Functor
import Data.List
import System.Directory

import Constants
import FileHelper

connectDB :: (String -> SQLiteHandle -> IO String) -> IO String
connectDB f = finddb >>= (\curdb -> if curdb == "" then return "No databases are accessable.\n" else
              (openConnection $ curdb++dbdir++dbname) >>= (\h -> f curdb h >>= (\r -> closeConnection h >> return r)))

getSql :: SQLiteResult a => SQLiteHandle -> String -> ([[Row a]] -> [String]) -> IO [String]
getSql h q f = either (\_ -> [""]) f <$> execStatement h q

pullSelect = map (snd . head) . head

allTags :: SQLiteHandle -> IO [String]
allTags h = getSql h "SELECT name FROM sqlite_master WHERE type='table' AND name<>'files'" pullSelect

curTags :: SQLiteHandle -> String -> IO [String]
curTags h hash = getSql h ("SELECT tags FROM files WHERE hash='"++hash++"'") (words.snd.head.head.head)

fullHash :: SQLiteHandle -> String -> IO [String]
fullHash h part = getSql h ("SELECT hash FROM files WHERE hash LIKE '"++part++"%'") pullSelect

fullHashes :: SQLiteHandle -> [String] -> IO [String]
fullHashes h [] = return []
fullHashes h (part:parts) = do
    fullhash <- fullHash h part
    rest <- fullHashes h parts
    if length fullhash > 1 then do
        putStrLn $ part++" is ambiguous between\n"++(unlines fullhash)
        return []
        else if head fullhash /= "" then return $ (head fullhash):rest else do
            putStrLn $ part++" does not exist."
            return rest

updateTags _ _ [] = return ()
updateTags h tags (hash:hashs) = do
    curtags <- curTags h hash
    execStatement_ h $ "UPDATE files SET tags='"++(unwords $ union curtags tags)++"' WHERE hash='"++hash++"'"
    updateTags h tags hashs

addTags _ [] _ = return ()
addTags h (t:ts) hashes = do
    alltags <- allTags h
    if notElem t alltags then execStatement_ h $ "CREATE TABLE "++t++" ( hash text )" else return $ Just ""
    mapM (\a -> execStatement_ h $ "INSERT INTO "++t++" VALUES ('"++a++"')") hashes
    addTags h ts hashes

rmTags _ _ [] = return ()
rmTags h tags (hash:hashs) = do
  curtags <- curTags h hash
  execStatement_ h $ "UPDATE files SET tags='"++(unwords $ filter (not . flip elem tags) curtags)++"' WHERE hash='"++hash++"'"
  rmTags h tags hashs

rmHashs _ [] _ = return ()
rmHashs h (t:ts) hashs = do
  mapM (\a -> execStatement_ h $ "DELETE FROM "++t++" WHERE hash='"++a++"'") hashs
  rmHashs h ts hashs

newDBHelper dir alreadyexists= do
      if alreadyexists then return "Database already exists here.\n" else do
        createDirectory $ dir++dbdir
        createDirectory $ dir++dbdir++filedir
        h <- openConnection $ dir++dbdir++dbname
        execStatement_ h "CREATE TABLE files ( hash text, ext text, tags longtext, location text)"
        closeConnection h
        return ""

findIncTag _ [] = ""
findIncTag old (x:xs) = concat [" INNER JOIN ", x, " ON ", x, ".hash=", old, ".hash", (findIncTag x xs)]


