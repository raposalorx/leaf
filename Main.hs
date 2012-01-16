{-# LANGUAGE DeriveDataTypeable #-}
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Digest.Pure.SHA
import System.Console.CmdArgs
import Data.Functor
import Control.Monad
import Database.SQLite
import System.Directory

import Constants
import FileHelper
import DBHelper

data Opts = Add {hashes :: [String], tags :: [String]}
            | Rm {hashes :: [String], tags :: [String]}
            | New {tags :: [String], files :: [FilePath]}
            | Del {noconfirm :: Bool, hashes :: [String]}
            | Find {hashonly :: Bool, nottag :: [String], tags :: [String]}
            | List {hash :: String}
            | Link {from :: String, files :: [FilePath]}
            | Run {files :: [String], nulldel :: Bool, newlinedel :: Bool, replace :: String, delimiter :: String, command :: [String]}
            | Runtags {tags :: [String], nulldel :: Bool, newlinedel :: Bool, replace :: String, delimiter :: String, command :: [String]}
            | Listdb {dir :: String}
            | Newdb {dir :: String}
            | Deldb {noconfirm :: Bool, dir :: String}
            | Mergedb {noconfirm :: Bool, sourcedir :: String, destdir :: String}
            | Rmredundancies {noconfirm :: Bool}
            deriving (Show, Data, Typeable)

add = Add {hashes = def &= help "File to add tags too. You may specify more than one." &= typ "HASHS", tags = def &= args &= typ "TAGS"} &= help "Add tags."
rm = Rm {hashes = def &= help "File to remove tags from. You may specify more than one." &= typ "HASHS", tags = def &= args &= typ "TAGS"} &= help "Remove tags."
new = New {tags = def &= help "Tag to add to the file you're adding. You may specify more than one." &= typ "TAGS", files = def &= args &= typ "FILES"} &= help "Add a file to the tagging database."
del = Del {noconfirm = def &= help "Don't ask for confirmation.", hashes = def &= args &= typ "HASHES"} &= help "Delete a file from the tagging database."
findtags = Find {hashonly = def &= help "Only return the hash.", nottag = def &= help "Exclude files with tag. You may specify more than one." &= typ "TAGS", tags = def &= help "Include files with tag. You may specify more than one." &= typ "TAGS"} &= help "Find tags."
list = List {hash = def &= args} &= help "List tags."

listdb = Listdb {dir = def &= args &= typ "DIR"} &= help "List all databases within reach of DIR. If no directory is specified, use the current directory."
newdb = Newdb {dir = def &= args &= typ "DIR"} &= help "Create a new database at DIR. If no directory is specified, use the current directory."
deldb = Deldb {noconfirm = def &= help "Don't ask for confirmation.", dir = def &= args &= typ "DIR"} &= help "Delete the database at DIR. If no directory is specified, use the current directory."

mode = cmdArgsMode $ modes [add,rm,new,del,findtags,list, listdb,newdb,deldb] &= help "Maintain tags for files" &= program "tagit" &= summary "Tagit v0.5"

mux :: Opts -> IO String

mux (Add hashes tags) = connectDB $ \curdb h -> do
    fullhashes <- fullHashes h hashes
    addTags h tags fullhashes
    updateTags h tags fullhashes
    closeConnection h
    return "Tags added.\n"
    

mux (Rm hashes tags) = return $ "rm " ++ (show hashes) ++ " " ++ (show tags)

mux (New tags files) = connectDB $ \curdb h -> do
        files_global <- mapM canonicalizePath files
        existing <- filterM (doesFileExist) files_global
        fileError (filter (\x -> not $ elem x existing) files_global)
        contents <- mapM C8.readFile existing
        let hashes = map (showDigest) $ map (sha512) contents
        let filehashes = zip existing hashes
        mapM (\(a,b) -> renameFile a (curdb++dbdir++filedir++b)) $ filehashes
        mapM (\(a,b) -> execStatement_ h $ "INSERT INTO files VALUES ('"++b++"','"++(extension a)++"','"++(unwords tags)++"','"++a++"')") $ filehashes
        addTags h tags hashes
        updateTags h tags hashes
        return "Files added.\n"
        

mux (Del noconf hashes) = return $ "del " ++ (show noconf) ++ " " ++ (show hashes)

mux (Find hashonly nottags tags) = connectDB $ \curdb h -> do
    found <- getSql h ("SELECT files.hash, files.ext, files.tags FROM files" ++ findIncTag "files" tags) $
                    (map 
                      ((if hashonly then head else ((curdb++dbdir)++) . concat).init.map snd)).head.map
                        (filter
                          ((all
                            (not . flip elem nottags)
                          ).words.last.map snd))
    return $ unlines found
    

mux (List []) = connectDB $ \curdb h -> do
    found <- getSql h "SELECT name FROM sqlite_master WHERE type='table' AND name<>'files'" $ head.map (map (head.map snd))
    return $ (head found) ++ (concat $ map (", "++) $ tail found) ++ "\n"
    

mux (List hash) = connectDB $ \curdb h -> do
    found <- getSql h ("SELECT tags FROM files WHERE hash LIKE '" ++ hash ++ "%'") $ words.head.head.map (map (head.map snd))
    return $ (head found) ++ (concat $ map (", "++) $ tail found) ++ "\n"
    

mux (Listdb []) = do
    dir <- getCurrentDirectory
    dbs <- listalldb dir
    if (length dbs) > 0 then
        return $ init $ foldl (\a b -> b ++ (',':a)) "" dbs
        else return "No databases are accessable."

mux (Listdb dir) = do
    exists <- doesDirectoryExist dir
    if exists then do
        candir <- canonicalizePath dir
        dbs <- listalldb candir
        if (length dbs) > 0 then
            return $ init $ foldl (\a b -> b ++ (',':a)) "" dbs
            else return "No databases are accessable."
        else return "Directory Doesn't exist."

mux (Newdb []) = do
    dir <- getCurrentDirectory
    alreadyexists <- doesDirectoryExist $ dir++dbdir
    newDBHelper dir alreadyexists

mux (Newdb dir) = do
    exists <- doesDirectoryExist dir
    alreadyexists <- doesDirectoryExist $ dir++dbdir
    if exists then newDBHelper dir alreadyexists
        else return "Directory Doesn't exist."

mux (Deldb noconfirm dir) = return $ "deldb " ++ (show noconfirm) ++ " " ++ dir

main = do
    a <- cmdArgsRun mode
    out <- mux a
    putStr out

