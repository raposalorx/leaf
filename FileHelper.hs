module FileHelper
(
  finddb,
  listalldb,
  extension,
  fileError
) where

import System.Directory

import Constants

finddb :: IO String
finddb = do
    d <- getCurrentDirectory
    finddbhelper d
    where
    finddbhelper dir = do
        isdb <- doesDirectoryExist $ dir ++ dbdir
        parent <- canonicalizePath $ dir ++ "/.."
        if parent/=dir then if isdb then return dir else finddbhelper parent else return $ if isdb then dir else ""

listalldb :: String -> IO [String]
listalldb dir = do
    isdb <- doesDirectoryExist $ dir ++ dbdir
    parent <- canonicalizePath $ dir ++ "/.."
    if parent/=dir then if isdb then do 
        parentdbs <- listalldb parent
        return $ dir:parentdbs
        else listalldb parent
        else return $ if isdb then [dir] else []

extension :: String -> String
extension s = if head s == '.' || notElem '.' s then "" else '.': (reverse $ takeWhile (/='.') $ reverse s)

fileError [] = return ()
fileError (f:fs) = do
    putStrLn $ f++" doesn't exist."
    fileError fs


