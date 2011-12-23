{-# LANGUAGE DeriveDataTypeable #-}
import Database.SQLite
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Digest.Pure.SHA
import System.Console.CmdArgs
import System.Directory
import Data.Functor
import Data.List
import Control.Monad

dbdir = "/.leaf/"
dbname = "leaf"
filedir = "/files/"

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

mode = cmdArgsMode $ modes [add,rm,new,del,findtags,list, listdb,newdb,deldb] &= help "Maintain tags for files" &= program "tagit" &= summary "Tagit v0.0"

-- sha512 $ Data.ByteString.Lazy.Char8.pack "blah"

finddb :: IO String
finddb = do
    d <- getCurrentDirectory
    finddbhelper d
    where
    finddbhelper dir = do
        isdb <- doesDirectoryExist $ dir ++ dbdir
        parent <- canonicalizePath $ dir ++ "/.."
        if parent/=dir then if isdb then return dir else finddbhelper parent else return $ if isdb then dir else ""

listdb_ :: String -> IO [String]
listdb_ dir = do
    isdb <- doesDirectoryExist $ dir ++ dbdir
    parent <- canonicalizePath $ dir ++ "/.."
    if parent/=dir then if isdb then do 
        parentdbs <- listdb_ parent
        return $ dir:parentdbs
        else listdb_ parent
        else return $ if isdb then [dir] else []

extension :: String -> String
extension s = if head s == '.' || notElem '.' s then "" else '.': (reverse $ takeWhile (/='.') $ reverse s)

allTags :: SQLiteHandle -> IO [String]
allTags h = either (\_ -> [""]) (map (snd . head) . head) <$> execStatement h "SELECT name FROM sqlite_master WHERE type='table' AND name<>'files'"

fullHash :: SQLiteHandle -> String -> IO [String]
fullHash h part = either (\_ -> [""]) (map (snd.head) . head) <$> execStatement h ("SELECT hash FROM files WHERE hash LIKE '"++part++"%'")

fullHashes :: SQLiteHandle -> [String] -> IO [String]
fullHashes h (part:parts) = do
    fullhash <- fullHash h part
    rest <- fullHashes h parts
    if length fullhash > 1 then do
        putStrLn $ part++" is ambiguous between\n"++(unlines fullhash)
        return [""]
        else if head fullhash /= "" then return $ (head fullhash):rest else do
            putStrLn $ part++" does not exist."
            return rest

fileError [] = return ()
fileError (f:fs) = do
    putStrLn $ f++" doesn't exist."
    fileError fs

updateTags _ _ [] = return ()
updateTags h tags (hash:hashs) = do
    curtags <- either (\_ -> [""]) (words.snd.head.head.head) <$> execStatement h ("SELECT tags FROM files WHERE hash='"++hash++"'")
    execStatement_ h $ "UPDATE files SET tags='"++(unwords $ union curtags tags)++"' WHERE hash='"++hash++"'"
    updateTags h tags hashs

addTags _ [] _ = return ()
addTags h (t:ts) hashes = do
    alltags <- allTags h
    if notElem t alltags then execStatement_ h $ "CREATE TABLE "++t++" ( hash text )" else return $ Just ""
    mapM (\a -> execStatement_ h $ "INSERT INTO "++t++" VALUES ('"++a++"')") hashes
    addTags h ts hashes

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

connectDB :: (String -> SQLiteHandle -> IO String) -> IO String
connectDB f = finddb >>= (\curdb -> if curdb == "" then return "No databases are accessable.\n" else
              (openConnection $ curdb++dbdir++dbname) >>= (\h -> f curdb h >>= (\r -> closeConnection h >> return r)))

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
    found <- either (\_ -> [""]) 
                    ((map 
                        ((if hashonly then head else ((curdb++dbdir)++) . concat).init.map snd)
                        ).head.map 
                            (filter ((all (not . flip elem nottags)).words.last.map (snd))))
                    <$> execStatement h ("SELECT files.hash, files.ext, files.tags FROM files" ++ findIncTag "files" tags)
    return $ unlines found
    

mux (List []) = connectDB $ \curdb h -> do
    found <- either (\_ -> [""]) (head.map (map (head.map (snd)))) <$> execStatement h ("SELECT name FROM sqlite_master WHERE type='table' AND name<>'files'")
    return $ (head found) ++ (concat $ map (", "++) $ tail found) ++ "\n"
    

mux (List hash) = connectDB $ \curdb h -> do
    found <- either (\_ -> [""]) (words.head.head.map (map (head.map (snd)))) <$> execStatement h ("SELECT tags FROM files WHERE hash LIKE '" ++ hash ++ "%'")
    return $ (head found) ++ (concat $ map (", "++) $ tail found) ++ "\n"
    

mux (Listdb []) = do
    dir <- getCurrentDirectory
    dbs <- listdb_ dir
    if (length dbs) > 0 then
        return $ init $ foldl (\a b -> b ++ (',':a)) "" dbs
        else return "No databases are accessable."

mux (Listdb dir) = do
    exists <- doesDirectoryExist dir
    if exists then do
        candir <- canonicalizePath dir
        dbs <- listdb_ candir
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

