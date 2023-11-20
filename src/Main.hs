-- {{{ begin_fold
-- script
-- #!/usr/bin/env runhaskell -i/Users/cat/myfile/bitbucket/haskelllib
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}

-- import Turtle
-- echo "turtle"

-- import Data.Set   -- collide with Data.List 
import Control.Monad
import Control.Monad (filterM, liftM, zipWithM)
import Data.Char
import Data.Typeable (typeOf) -- runtime type checker, typeOf "k"
import qualified Data.List as L
import Data.List.Split
import Data.Time
import Data.Time.Clock.POSIX
import System.Directory
import System.Environment
import System.Exit
import System.FilePath.Posix
import System.IO
import System.Posix.Files
import System.Posix.Unistd
import System.Process
import Text.Read
import Text.Regex
import Text.Regex.Base
import Text.Regex.Base.RegexLike
import Text.Regex.Posix
import Data.IORef 
import Control.Monad (unless, when)
import Control.Concurrent
import Text.RawString.QQ         -- Need QuasiQuotes too 

import qualified Text.Regex.TDFA as TD
import AronModule 

--import Data.Array

-- import Graphics.Rendering.OpenGL as GL 
-- import Graphics.Rendering.OpenGL.GLU.Matrix as GM  
-- import qualified Graphics.UI.GLFW as G
-- import Data.Set(Set) 
-- import qualified Data.Set as S 

--if (length argList) == 2 
--then case head argList of 
--    "svg" -> run cmd >> run "ls" >>= \x -> pp x 
--            where 
--                cmd = "pwd" 
--    "png" ->run cmd >> run ("ls " ++ fn) >>= \x -> pp x  
--            where 
--                cmd = "pwd" 
--    _     -> print "more arg" 
--else print "Need more arguments" 

--    takeFileName gives "file.ext"
--    takeDirectory gives "/directory"
--    takeExtension gives ".ext"
--    dropExtension gives "/directory/file"
--    takeBaseName gives "file"
--    "/directory" </> "file.ext".
--    "/directory/file" <.> "ext".
--    "/directory/file.txt" -<.> "ext".
-- |  end_fold ,}}}
       
-- KEY: xfido page, generate html, generate xfido static html page, generate static page, gene html, gene xfido, gene www
--
-- Wed Aug 22 08:54:12 2018  
-- use iterateList from AronModule 
------------------------------------------------------------------ 
-- Sat Aug  3 23:30:22 2019 
-- Add Emacs Solarized theme css dark/light: orgDark/orgLight
-- Remove some useless code
------------------------------------------------------------------ 
-- Sat 10 Apr 00:10:59 2021 
-- Add more tag name
------------------------------------------------------------------ 
-- Sat  8 May 22:20:37 2021  
-- Add more tag name
------------------------------------------------------------------ 
-- Sat 25 Dec 23:54:51 2021 
-- UPDATE: Move Html titles to src/htmlTitle.txt
------------------------------------------------------------------ 
-- Sun 25 Sep 23:11:59 2022  
-- UPDATE: Change table size in index.html page 
------------------------------------------------------------------ 
-- Wed  1 Mar 20:42:20 2023  
-- UPDATE: Clear up the bad code.
------------------------------------------------------------------
-- Sunday, 19 November 2023 21:47 PST
-- UPDATE: Found a bug when index fils is created
-- index.html Should Not been used
-- /Users/aaa/myfile/bitbucket/publicfile/text/index.html
------------------------------------------------------------------ 


indexOpen = [r|
              <!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en-GB">
<head>
    <title>Xfido</title>
    <meta http-equiv="Content-Type" content="application/xhtml+xml; charset=utf-8" />
    <meta name="description" content="Xcode Change Build from iPhone to Universal iPad " />
    <meta name="keywords" content="Build iPhone and iPad, Change Xcode Build Target " />
    <meta name="robots" content="index, follow" />
    <link rel="shortcut icon" href="/favicon.ico" type="image/x-icon" />
    <link rel="stylesheet" type="text/css" href="screen.css" media="screen" />
    <link rel="stylesheet" type="text/css" href="highlight.css">
    <link rel="stylesheet" type="text/css" href="style.css">
    <link rel="icon" href="image/xfido.png" type="image/gif" sizes="16x16">
    <meta http-equiv="X-UA-Compatible" content="IE=edge" />

    <!-- Added mathjax v3, Wed  1 Mar 15:00:23 2023  -->
    <!-- Do not use the local mathjax any more, please delete it -->
    <script>
    MathJax = {
      tex: {
        inlineMath: [['$', '$'], ['\\(', '\\)']]
      },
      svg: {
        fontCache: 'global'
      }
    };
    </script>
    <script type="text/javascript" id="MathJax-script" async
      src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-svg.js">
    </script>
    </head>
    <body>
            |]

indexClose = [r|
               </body>
               </html>
             |]
  
-- Emacs Org-mode with solarized theme css 
orgDark=[r|#+HTML_HEAD: <link rel="stylesheet" type="text/css" href="http://thomasf.github.io/solarized-css/solarized-dark.min.css" />|]
orgLight=[r|#+HTML_HEAD: <link rel="stylesheet" type="text/css" href="http://thomasf.github.io/solarized-css/solarized-light.min.css" />|]


{-| 
    === Try to generate Html from Haskell
-} 
textArea::String -> String
textArea s = "<textarea>" ++ s ++ "</textarea>"

{-| 
    >t = "color:red;"
    >span' "color:red;" "file"
-} 
span::String->String->String
span t s = "<span style=\"" ++ t ++ "\">" ++ s ++ "</span>"

{-|
    === Convert space to underscore

    UPDATE: Mon Jun 25 23:31:02 PDT 2018
    UPDATE: Wed  1 Mar 20:39:10 2023 

    @
    "cat dog" => indexCatDog.html => cat_dog.html
    "cat dog" => "cat_dog", this will be used in new html file name
    @
-}
spaceToUnderScore::String->String
spaceToUnderScore s = tail $ foldl(\x y -> x ++ "_" ++ y) "" l
                    where l = splitRegex(mkRegex "[[:space:]]+") s

-- KEY: numbers of blocks per row in the gallery page, table size
colNum = 1 

data Local = HomeMac | HomeMacBook deriving(Show)
getHost::Local->String
-- getHost HomeMac = "/Library/WebServer/Documents/zsurface"
getHost HomeMac     = "/Library/WebServer/Documents/xfido"
getHost HomeMacBook = "/Library/WebServer/Documents/xfido"


-- Friday, 08 October 2021 13:21 PDT
-- Delete it
-- replace with getEvn "HOME"
--
-- getHome::Local->String
-- getHome HomeMacBook = "/Users/aaa"
-- getHome HomeMac     = "/Users/cat"
-- getHome AmaMac      = "/Users/xxxx"

mathfont::String->String
mathfont s = "" ++ s 
--mathfont s = "$\\textcal{" ++ s ++ "}$"

-- | -------------------------------------------------------------------------------- 
-- | Sun Dec  2 15:29:29 2018 
-- | Change new index.html page to gallery layout
-- | -------------------------------------------------------------------------------- 
oldIndexFile = (getHost HomeMacBook) </> "indexOld.html"
newIndexFile = (getHost HomeMacBook) </> "index.html"

open_li  = [r|<li><a style='text-decoration:none;' href='|]
href_li  = [r|'</a>|]
close_li = [r|</li>|]

-- For live web site
remoteHost = "http://xfido.com/html/"

-- Friday, 08 October 2021 13:20 PDT
-- Add the following functions
getIndexPath::IO String
getIndexPath = do
  home <- getEnv "HOME"
  return $ home </> "myfile/bitbucket/publicfile/text/index.html"

  
pageFile::IO String
pageFile = do
  home <- getEnv "HOME"
  return $ home </> "myfile/bitbucket/publicfile/text/page.html"

localPath = "html/"
remotePath = "http://xfido.com/html/"
  
data WWWDirHtml = WWWDirHtml {htmlDir_ :: String,
                              newDir_ :: String,
                              tmpHtml_ :: String,
                              myText_ :: String
                             } deriving (Show, Eq, Ord)

-- read html files and gene newhtml dir
-- htmlDir = (getHost HomeMacBook) </> "html"
-- newDir  = (getHost HomeMacBook) </> "newhtml" 
-- tmpHtml = (getHost HomeMacBook) </> "html_" 

getWWWDir::IO WWWDirHtml
getWWWDir = do
  www <- getEnv "www"
  let r = WWWDirHtml{
        htmlDir_ = www </> "html",
        newDir_  = www </> "newhtml",
        tmpHtml_ = www </> "html_",
        myText_ = www </> "mytext"
        }
  return r


{-|
 let htmlArr = map(\x -> "index" ++ (removeSpace x) ++ ".html") array -- gene html file names from array
 let orgArr  = map(\x -> "index" ++ (removeSpace x) ++ ".org") array -- gene html file names from array
 let fileArray = htmlArr ++ orgArr
 let pathList  = map(\x -> htmlDir </> x) fileArray -- gene full path html file names from array: root/html/indexFoo.html
-}
  
newHtmlPage::[String] -> IO [(String, String)]
newHtmlPage cs = do
    wwwr <- getWWWDir
    let pathList  = map(\(a, x) -> (a, (htmlDir_ wwwr) </> x)) fileArray -- gene full path html file names from array: root/html/indexFoo.html
    filterM((liftM not) . doesFileExist . snd ) pathList -- extract new html file name which is not in xfido/html
  
  where
    htmlArr = map(\x -> (x, "index" ++ (removeSpace x) ++ ".html") )  cs -- gene html file names from array
    orgArr  = map(\x -> (x, "index" ++ (removeSpace x) ++ ".org") ) cs -- gene html file names from array
    fileArray = htmlArr ++ orgArr

  
-------------------------------------------------------------------------------- 
-- new index page code here

div_close::String
div_close = [r|</a></td>|]

imgTag::RowBlock->String
imgTag s = [r|<td style='width:300px;height:30px;'><a href='|] <> htmlURL s <> [r|'><div style='text-align:left;'> |] <> title s <> [r|</div>|]

closeOpenDiv::RowBlock->String
closeOpenDiv s = "\n\n<tr>\n" ++ (imgTag s) ++ div_close 

closeOpenDivNotDiv::RowBlock->String
closeOpenDivNotDiv s = "</tr>\n\n<tr>\n" ++ (imgTag s) ++ div_close 

photoPath = "/Library/WebServer/Documents/xfido/myphoto"
-------------------------------------------------------------------------------- 
-- for each block in the index page
data RowBlock = RowBlock{
    htmlURL :: String,
    title   :: String,
    imgURL  :: String
    } deriving (Show)

htmlTitle = "htmlTitle.txt"
helpEx::IO()
helpEx = do 
        printBox 2 ["Usage: [ genehtml.hs l -> html for localhost ][ genehtml.hs r -> html for remote ]"]
        printBox 2 ["Read file from src/htmlTitle.txt                                                  "]
        printBox 2 ["Add Html Title: src/htmlTitle.txt                                                 "]
        exitWith(ExitFailure 1) -- echo $? => 1

notBlankLine x = (len . trim) x > 0

titleToHtmlPath :: [String] -> [String]
titleToHtmlPath cx = undefined

-- NOTE: Change the path if the code is moved
rootPath :: IO String
rootPath = getEnv "g" >>= \x -> return $ x </> "haskellGeneXfidoHtml"
  
main = do 
        argList <- getArgs 
        if len argList == 0 
        then helpEx else pp "Take a while... if there are lots of html file:)"
        titlePath <- rootPath >>= \x -> return $ x </> "src" </> htmlTitle
        pp titlePath

        array <- readFileList titlePath >>= \cx -> return $ filter notBlankLine cx
        mapM_ print argList

        -- TODO: Fixed the stupid code here
        let currPath = case argList of
                            ("l":_) -> localPath
                            ("r":_) -> remotePath
                            _       -> ""
        pp argList
        -- nothing <- getLine 


        wwwr <- getWWWDir
        let htmlDir = htmlDir_ wwwr
        let newDir  = newDir_  wwwr
        let tmpHtml = tmpHtml_ wwwr
        let myText  = myText_ wwwr
        -- doesDirectoryExist newDir >>= \x -> if x then rm newDir else putStrLn "newDir does not exist" >> mkdir newDir
        doesDirectoryExist newDir >>= \x -> if x then rm newDir
                                            else putStrLn "newDir does not exist => Create new Dir" >> mkdir newDir
        -- mkdir newDir
        -- let photos = replicate 400 "/image/curve11.svg"
        let photos = replicate (len array) "/image/blockimage.svg"



        -- indexHtmlContent <- getIndexPath >>= readFileList

        
        let htmlArr = map(\x -> "index" ++ (removeSpace x) ++ ".html") array -- Generate html file names from array
        let orgArr  = map(\x -> "index" ++ (removeSpace x) ++ ".org") array -- Generate html file names from array
        let fileArray = htmlArr ++ orgArr
        let hostList  = map(\x -> currPath ++ x) fileArray -- ["indexMyPDf.html"] 
        
        newHtmlFile <- newHtmlPage array
        
        let zipList   = zip hostList array -- [("/path/indexMyPhoto.com", "My Photo")]
        let blockList = zipWith3(\x y z ->RowBlock{
                                        htmlURL = x, 
                                        title = y, 
                                        imgURL = z}) hostList array photos

        -- Gene index page link from zipList
        let indexPageLink  = map(\x -> open_li ++ (fst x) ++ href_li ++ (mathfont (snd x)) ++ close_li) zipList
        --------------------------------------------------------------------------------------------------
        -- TODO add new index page here 
        let tableHtml = zipWith(\x y -> if mod y colNum == 0 then
                                          (if y == 0 then closeOpenDiv x else closeOpenDivNotDiv x)
                                        else (imgTag x ++ div_close)) blockList [0..] 

        let newIndexHtml = ["<table style='margin-left:auto; margin-right:auto; border:1px black solid;'>"] ++ tableHtml ++ ["</tr></table>"]
        www <- getEnv "www"
        writeToFile (www </> "indexTest.html") newIndexHtml 

        -- writeToFile newIndexFile newIndexPageContent
        let indexPageContent = [indexOpen <> (unlines indexPageLink) <> indexClose]
        writeToFile newIndexFile indexPageContent
        -- writeToFile "/tmp/myindex.html" newIndexPageContent 
        --------------------------------------------------------------------------------------------------
        allHtmlDirFiles <- listDirFilter htmlDir "\\.html$" -- get all the html file from html folder, e.g.  xfido/html
        mapM print allHtmlDirFiles

        -- Read file content from xfido/html
        -- [[f1, f2..], [f3, f4..]]
        let list200 = partList 200 allHtmlDirFiles -- Read 200 files only, if too many files are read, there is issue with memory
        print "Print List ->"
        iterateList list200 (\files -> do
                        pList <- filterM(\x -> doesFileExist $ htmlDir </> x) files 
                        let fullList= map(\x -> htmlDir </> x) pList 
                        let newDirList = map(\x -> newDir </> x) files 
                        ffList <- mapM(\fn -> readFile fn >>=(\contents -> return(contents))) fullList 
                        let contents = [ [x] | x <- ffList, length x > 0]
                        zipWithM(\fn list -> writeToFile fn list) newDirList contents 
                        fl
                        mapM print files
                        fl 
                      )
        ---------------------------------------------------------------------------------- 
        -- write to index.html 
        -- update index.html with new indexPageLink
        -- new menu insert into space in haskll/text/index.html
        -- 
        --  index.html
        --  <html>
        --  indexPageLink
        --  </html>
        -- -------------------------------------------------------------------------------- 

        writeToFile oldIndexFile indexPageContent
        ----------------------------------------------------------------------------
        -- gene NEW html with content in pageFile if name in the array is not found in xfido/html
        pageFile' <- pageFile
        pagelist <- readFileList pageFile'

        let htmlPage = map(\x -> if len (removeSpace x) > 0 then x else "add content here") pagelist 

        -- if .html => write htmlPage content
        -- if .org  => write solarized theme css Dark
        -- orgLight => light theme
        mapM(\(a, x) -> let ext = takeExt x
                            hname = x
                            title = a
                        in if ext == ".org" then do 
                             writeToFile hname [orgDark] 
                           else do 
                             writeToFile hname ( replaceList htmlPage "replaceTitle00" title )  ) newHtmlFile  -- add new html page to root/html

        pp "New html files: number of html files are added"
        fl
        pp $ show $ len newHtmlFile
        pp "Done"
        pre newHtmlFile
        pre htmlPage
        fl
        pre indexPageContent
        fl
        pre blockList

        myTextFile <- listDirFilter myText "\\.txt$" -- get all the html file from html folder, e.g.  xfido/html
        mapM print myTextFile
        ----------------------------------------------------------------------------
