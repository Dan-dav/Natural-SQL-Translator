module Main where

--import PGF
import System.Process

-- to do
-- web app
-- have better way of choosing languages
-- database integration

postgreSQLstring = "host=localhost port=5432 dbname=countries user=postgres password=palm77fe"

main :: IO ()
main = do
 --   findDBfiles "Countries"
    callCommand "gf -make DBcountriesSQL.gf DBcountriesEng.gf"
    --(a, b, c, d) <- createProcess $ shell "gf -make DBcountriesSQL.gf DBcountriesEng.gf"
    putStrLn $ "a: "-- ++ show a
    putStrLn $ "b: "-- ++ show b
    putStrLn $ "c: "-- ++ show c
    --putStrLn $ "d: " ++ show d
--    gr <- readPGF "DBcountries.pgf"

--    let stCat = startCat gr
--    let langs = languages gr
    putStr "Available languages: "
--    printLangs langs

    putStr "Choose input language: "
    from <- getLine
--    let fromLang = maybe undefined id (readLanguage from)
    putStr "Choose output language: "
    to <- getLine
--    let toLang = maybe undefined id (readLanguage to)
    
    putStr $ "Type something in the language " ++ from ++ ": "
    s <- getLine
    putStrLn (from ++ " input: " ++ s)

--    let absTrees = parse gr fromLang stCat s
    putStr "Abstract representations: "
--    printTrees absTrees
--    case absTrees of
--        [] -> return ()
--        (at:ats) -> do
--            putStr $ to ++ " output: "
--            putStrLn $ linearize gr toLang at
            --if (to == "DBCountriesSQL"):
                --putStr "Send query to database? (Y/N): "

--findDBfiles db = 

--printLangs :: [Language] -> IO()
--printLangs [] = return ()
--printLangs [l] = do
--    putStrLn $ (showLanguage l)
--printLangs (l:ls) = do
--    putStr $ (showLanguage l) ++ ", "
--    printLangs ls

--printTrees :: [Tree] -> IO()
--printTrees [] = return ()
--printTrees (t:ts) = do
--    printTree t
--    printTrees ts

--printTree :: Tree -> IO()
--printTree t = do
--    let t' = unStr t
--    case t' of
--        Nothing -> return()
--        Just st -> putStrLn st
--    let cids = exprFunctions t
--    let t' = showExpr cids t
--    putStrLn t'