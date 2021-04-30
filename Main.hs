module Main where

import PGF

-- to do
-- web app
-- have better way of choosing languages
-- database integration

main :: IO ()
main = do
    putStr "Type something in English: "
    s <- getLine
    putStrLn ("English input: " ++ s)

    gr <- readPGF "DBCountries.pgf"
    let stCat = startCat gr
    let langs = languages gr
    printLangs langs

    let langEng = readLanguage "DBCountriesEng"
    case langEng of
        Nothing -> return ()
        Just lEng -> do
            let langSQL = readLanguage "DBCountriesSQL"
            case langSQL of
                Nothing -> return ()
                Just lSQL -> do
                    let absTrees = parse gr lEng stCat s
                    putStr "Abstract representations: "
                    printTrees absTrees
                    case absTrees of
                        [] -> return ()
                        (at:ats) -> do
                            putStr "SQL output: "
                            putStrLn $ linearize gr lSQL at

--init :: IO (Language, Language)
--init = do
--    let langEng = readLanguage "DBCountriesEng"
--    case langEng of
--        Nothing -> return () -- throw error?
--        Just lEng -> do
--            let langSQL = readLanguage "DBCountriesSQL"
--            case langSQL of
--                Nothing -> return () -- throw error?
--                Just lSQL -> do
--                    return (lEng, lSQL)

printLangs :: [Language] -> IO()
printLangs [] = return ()
printLangs (l:ls) = do
    putStrLn $ showLanguage l
    printLangs ls

printTrees :: [Tree] -> IO()
printTrees [] = return ()
printTrees (t:ts) = do
    printTree t
    printTrees ts

printTree :: Tree -> IO()
printTree t = do
    let t' = unStr t
    case t' of
        Nothing -> return()
        Just st -> putStrLn st
    let cids = exprFunctions t
    let t' = showExpr cids t
    putStrLn t'