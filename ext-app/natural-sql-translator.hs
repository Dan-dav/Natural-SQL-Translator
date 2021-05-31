import Network.CGI
import Text.XHtml
import Data.Char

import PGF
import System.Process

import Control.Monad (liftM, unless, when)
import Data.Maybe (fromMaybe, isJust)
import Data.List
import Data.List.Split
import Data.List.Unique
import Data.Text (pack)
import Data.Text.IO.Utf8
import System.Directory

import Database.HDBC
import Database.HDBC.PostgreSQL (connectPostgreSQL, Connection)

import Data.ByteString.Char8 (unpack)

---------------------------------------------
-- Html Pages
---------------------------------------------

database_page :: Html
database_page = h1 << "Database selection"
          +++ form << [hidden "page_id" "database",
                       paragraph << ("Database to be used: " +++ textfield "db_choice"),
                       submit "button_id" "Add a language for this database",
                       submit "button_id" "Choose and enter translator"]

language_page :: String -> [String] -> [String] -> [String] -> Html
language_page db_choice lang_list tabs cols = h1 << "Language adding"
          +++ form << ([hidden "page_id" "language",
                        hidden "db_choice" db_choice,
                        paragraph << ("Language to add for the " ++ db_choice ++ " database: " +++ (langSelector lang_list) ! [name "lang_sel"]),
                        paragraph << ("Please type names in that language (in singular and plural) for the tables and columns of the " ++ db_choice ++ " database:"),
                        fieldset << ([legend << ("Tables: "), tabFieldsTable tabs]),
                        fieldset << ([legend << ("Columns: "), colFieldsTable cols]),
                        paragraph << (reset "" "Clear all" +++ submit "button_id" "Choose another database"),
                        submit "button_id" "Submit and enter translator"])

translation_page :: String -> String -> [String] -> String -> String -> String -> Bool -> Html -> Html
translation_page db_choice text_in lang_list lang_in lang_out text_out commit_choice db_out = h1 << "Translation"
          +++ form << [hidden "page_id" "translation",
                        hidden "db_choice" db_choice,
                        (paragraph << ("From: " +++ (langSelectorChosen lang_list lang_in) ! [name "in_sel"] +++ " To: " +++ (langSelectorChosen lang_list lang_out) ! [name "out_sel"])),
                        (paragraph << ("Input: " +++ (textfield "in_field") ! [value text_in, size "100"])),
                        submit "button_id" "Translate",
                        paragraph << ("Output: " +++ (textfield "out_field") ! [value text_out, size "100"]),
                        paragraph << ("Using the database: " ++ db_choice ++ " " +++ submit "button_id" "Choose another database" +++ submit "button_id" "Add a language for this database"),
                        paragraph << ("Send SQL code to " ++ db_choice ++ " database: " +++ submit "button_id" "Send to DB" 
                            +++ "   Also commit changes: " +++ (checkbox "commit_box" "ticked") ! (commitState commit_choice))
                        ]
          +++ fieldset << ([legend << ("Output from the database"), db_out])
          --(textarea << db_out) ! [rows "30", cols "100"] ???

commitState :: Bool -> [HtmlAttr]
commitState True = [checked]
commitState False = []

---------------------------------------------
-- Helpers for showing pages
---------------------------------------------

showPage :: Html -> CGI CGIResult
showPage = output . renderHtml . page "Natural SQL Translator"

page :: String -> Html -> Html
page t b = header << thetitle << t +++ body << b

---------------------------------------------
-- The main CGI functionality
---------------------------------------------

main :: IO ()
main = do
    runCGI $ handleErrors cgiMain

cgiMain :: CGI CGIResult
cgiMain = do page_id <- getInput "page_id"
             button_id <- getInput "button_id"
             case page_id of
                 Just "database" ->
                     case button_id of
                         Just "Add a language for this database" -> do
                             -- Getting data out from prev page
                             db_choice <- getInput "db_choice"
                             -- Peforming calculations
                             let db_choice' = maybe "error" id db_choice
                             (tabs', cols') <- liftIO $ getTabsAndCols db_choice'
                             langs <- liftIO $ findGenLangs
                             -- Putting data into next page
                             showPage $ language_page db_choice' langs tabs' cols'
                         Just "Choose and enter translator" -> do
                             -- Getting data out from prev page
                             db_choice <- getInput "db_choice"
                             -- Peforming calculations
                             let db_choice' = maybe "error" id db_choice
                             liftIO $ genPGF db_choice'
                             langs <- liftIO $ findSpecLangs db_choice'
                             -- Putting data into next page
                             showPage $ translation_page db_choice' "" langs "none" "none" "" False (p << "Nothing")
                         _ -> output "Error: Database page, unknown button"
                 Just "language" ->
                     case button_id of
                         Just "Choose another database" -> do
                             -- Getting data out from prev page
                             -- Peforming calculations
                             -- Putting data into next page
                             showPage $ database_page
                         Just "Submit and enter translator" -> do
                             -- Getting data out from prev page
                             db_choice <- getInput "db_choice"
                             lang_sel <- getInput "lang_sel"
                             inputs <- getInputs
                             -- Peforming calculations
                             let db_choice' = maybe "error" id db_choice
                             let lang_sel' = maybe "error" id lang_sel
                             let col_sing_inputs' = someInputs inputs "col_sing_"
                             let col_plur_inputs' = someInputs inputs "col_plur_"
                             let col_inputs' = singPlurCombine col_sing_inputs' col_plur_inputs'
                             let tab_sing_inputs' = someInputs inputs "tab_sing_"
                             let tab_plur_inputs' = someInputs inputs "tab_plur_"
                             let tab_inputs' = singPlurCombine tab_sing_inputs' tab_plur_inputs'
                             absThere <- liftIO $ doesFileExist $ "db-" ++ db_choice' ++ "/DB" ++ db_choice' ++ ".gf"
                             liftIO $ unless absThere $ makeGFFile db_choice' Nothing col_inputs' tab_inputs' -- writing abstract and sql gf files if needed
                             liftIO $ makeGFFile db_choice' (Just lang_sel') col_inputs' tab_inputs' -- writing/overwriting natural language gf file
                             liftIO $ genPGF db_choice'
                             langs <- liftIO $ findSpecLangs db_choice'
                             -- Putting data into next page
                             showPage $ translation_page db_choice' "" langs "none" "none" "" False (p << "Nothing")
                         _ -> output "Error: Language page, unknown button"
                 Just "translation" ->
                     case button_id of
                         Just "Translate" -> do
                             -- Getting data out from prev page
                             db_choice <- getInput "db_choice"
                             text_in <- getInput "in_field"
                             lang_in <- getInput "in_sel"
                             lang_out <- getInput "out_sel"
                             commit_box <- getInput "commit_box"
                             -- Peforming calculations
                             let db_choice' = maybe "error" id db_choice
                             let text_in' = maybe "error" id text_in
                             let lang_in' = maybe "error" id lang_in
                             let lang_out' = maybe "error" id lang_out
                             let commit_choice = isJust commit_box
                             let lexed_text = lexer text_in'
                             text_out' <- liftIO $ translate db_choice' lexed_text lang_in' lang_out'
                             langs <- liftIO $ findSpecLangs db_choice'
                             -- Putting data into next page
                             showPage $ translation_page db_choice' text_in' langs lang_in' lang_out' text_out' commit_choice (p << "Nothing")
                         Just "Choose another database" -> do
                             -- Getting data out from prev page
                             -- Peforming calculations
                             -- Putting data into next page
                             showPage $ database_page
                         Just "Add a language for this database" -> do
                             -- Getting data out from prev page
                             db_choice <- getInput "db_choice"
                             -- Peforming calculations
                             let db_choice' = maybe "error" id db_choice
                             (tabs', cols') <- liftIO $ getTabsAndCols db_choice'
                             langs <- liftIO $ findGenLangs
                             -- Putting data into next page
                             showPage $ language_page db_choice' langs tabs' cols'
                         Just "Send to DB" -> do
                             -- Getting data out from prev page
                             db_choice <- getInput "db_choice"
                             text_in <- getInput "in_field"
                             lang_in <- getInput "in_sel"
                             lang_out <- getInput "out_sel"
                             text_out <- getInput "out_field"
                             commit_box <- getInput "commit_box"
                             -- Peforming calculations
                             let db_choice' = maybe "error" id db_choice
                             let text_in' = maybe "error" id text_in
                             let lang_in' = maybe "error" id lang_in
                             let lang_out' = maybe "error" id lang_out
                             let text_out' = maybe "error" id text_out
                             let commit_choice = isJust commit_box
                             db_out' <- liftIO $ maybeSendToDB db_choice' text_out commit_choice
                             langs <- liftIO $ findSpecLangs db_choice'
                             -- Putting data into next page
                             showPage $ translation_page db_choice' text_in' langs lang_in' lang_out' text_out' commit_choice db_out'
                         _ -> output "Error: Translation page, unknown button"
                 Nothing -> showPage $ database_page
                 _ -> output "Error: Unknown page"

---------------------------------------------
-- Assembling lists of available languages
---------------------------------------------

-- get languages which are available generally
-- by seeing which ("Databases" ++ _ ++ ".gf") files are there
findGenLangs = do
    let dir = "gen-gf-files" -- ""
    contents <- listDirectory dir
    let genFiles = filter isGenNat contents
    let langs = map genFileToLang genFiles
    let natLangs = filter genLangFilter langs
    return $ natLangs

isGenNat name = isPrefixOf "Databases" name && isSuffixOf ".gf" name

genFileToLang fileName = drop (length "Databases") $ take (length fileName - length ".gf") fileName

genLangFilter "" = False
genLangFilter "SQL" = False
genLangFilter _ = True

-- get languages which are available for a database
-- by seeing which ("DB" ++ db ++ _ ++ ".gf") files are there
findSpecLangs db = do
    let dir = "db-" ++ db
    contents <- listDirectory dir
    let specFiles = filter (isSpecNat db) contents
    let langs = map (specFileToLang db) specFiles
    let natLangs = filter specLangFilter langs
    return $ natLangs

isSpecNat db name = isPrefixOf ("DB" ++ db) name && isSuffixOf ".gf" name

specFileToLang db fileName = drop (length ("DB" ++ db)) $ take (length fileName - length ".gf") fileName

specLangFilter lang | length lang == 3 = True
specLangFilter _ = False

---------------------------------------------
-- Getting tables and columns for a database
---------------------------------------------

getTabsAndCols db = do
    let postgresParameters = "host=localhost port=5432 dbname=" ++ db ++ " user=postgres password=palm77fe" -- lose the password ???
    (connInteger, connStr, maybeConn) <- catchSql (connectPGHelper postgresParameters) sqlConnHandler
    case maybeConn of
        Nothing -> do
            let tabs = ["conn fail"] -- ???
            let cols = ["conn faillll"]
            return (tabs, cols)
        Just conn -> do
            let tabGetting = "SELECT table_name FROM information_schema.tables WHERE table_schema = 'public' ORDER BY table_name;" -- get list of tables
            statement <- prepare conn tabGetting
            (execInteger, execString) <- catchSql (sqlExecuter statement) sqlHandler -- either -2 and error string or the num of rows affected? and "Works fine!"
            case execInteger of
                -2 -> do
                    let tabs = ["tab fail"] -- ???
                    let cols = ["col whocares"]
                    disconnect conn
                    return (tabs, cols)
                _ -> do
                    results <- sFetchAllRows' statement
                    let tabs = concat $ processTable results
                    colLists <- mapM (getCols conn) tabs -- getting the columns in each table
                    let cols = mergeColLists colLists -- removing duplicate columns
                    disconnect conn
                    return (tabs, cols)

getCols conn tab = do
    let colGetting = "select * from " ++ tab ++ " where false;"
    statement <- prepare conn colGetting
    (execInteger, execString) <- catchSql (sqlExecuter statement) sqlHandler -- either -2 and error string or the num of rows affected? and "Works fine!"
    case execInteger of
        -2 -> do
            return ["col fail"] -- ???
        _ -> do
            getColumnNames statement

connectPGHelper parameters = do
    conn <- connectPostgreSQL parameters
    return (1, "Connected fine!", Just conn)

sqlConnHandler :: SqlError -> IO (Integer, String, Maybe Connection) -- any other errors to handle?
sqlConnHandler (SqlError seState seNativeError seErrorMsg) = do
    return (-2, seErrorMsg, Nothing)

mergeColLists colLists = sortUniq $ concat colLists

---------------------------------------------
-- PGF generation and usage
---------------------------------------------

genPGF db = do
    -- see that db folder exists
    -- raise error if not (show error screen)
    -- also raise error if abstract or sql files aren't there
    langs <- findSpecLangs db
    let langFiles = map (langFile db) langs
    a <- runProcess "gf" ("-make" : langFiles) (Just $ "db-" ++ db) Nothing Nothing Nothing Nothing
    return ()

langFile db lang = "DB" ++ db ++ lang ++ ".gf"

lexer :: String -> String
lexer s = s
-- commas, semicolons, vals, 
-- how are vals linearized in different langs now?
-- upper/lower case
-- 

translate :: String -> String -> String -> String -> IO (String)
translate db s in_l out_l = do
    gr <- liftIO $ readPGF $ "db-" ++ db ++ "/DB" ++ db ++ ".pgf"
    let stCat = startCat gr
    let fromLang = maybe undefined id (readLanguage ("DB" ++ db ++ in_l))
    let absTrees = parse gr fromLang stCat s
    let toLang = maybe undefined id (readLanguage ("DB" ++ db ++ out_l))
    case absTrees of
        [] -> return "Failed to parse"
        (at:ats) -> do
            let output = linearize gr toLang at
            case output of
                "" -> return "Failed to linearize"
                _ -> return output

---------------------------------------------
-- Making .gf files
---------------------------------------------

makeGFFile :: String -> Maybe String -> [(String, String, String)] -> [(String, String, String)] -> IO ()
makeGFFile db maybeLang cols tabs = do
    createDirectoryIfMissing False $ "db-" ++ db -- check if folder exists, if not create it
    case maybeLang of
        Nothing -> do
            Data.Text.IO.Utf8.writeFile ("db-" ++ db ++ "/DB" ++ db ++ ".gf") $ pack $ makeAbsGF db cols tabs
            Data.Text.IO.Utf8.writeFile ("db-" ++ db ++ "/DB" ++ db ++ "SQL.gf") $ pack $ makeSqlGF db cols tabs
        Just lang -> do
            Data.Text.IO.Utf8.writeFile ("db-" ++ db ++ "/DB" ++ db ++ lang ++ ".gf") $ pack $ makeNatGF db lang cols tabs

makeAbsGF :: String -> [(String, String, String)] -> [(String, String, String)] -> String
makeAbsGF db cols tabs = "abstract DB" 
    ++ db ++ " = Databases ** {\n\nflags startcat = Statement ;\n\nfun\n\n  " 
    ++ colAbsPart cols ++ " : Column ;\n\n  " 
    ++ tabAbsPart tabs 
    ++ " : Table ;\n\n}"

colAbsPart :: [(String, String, String)] -> String
colAbsPart [] = ""
colAbsPart [(n, sing, plur)] = "Col_" ++ (loseSpaces n)
colAbsPart ((n, sing, plur):cols) = "Col_" ++ (loseSpaces n) ++ ", " ++ colAbsPart cols

tabAbsPart :: [(String, String, String)] -> String
tabAbsPart [] = ""
tabAbsPart [(n, sing, plur)] = "Tab_" ++ (loseSpaces n)
tabAbsPart ((n, sing, plur):tabs) = "Tab_" ++ (loseSpaces n) ++ ", " ++ tabAbsPart tabs

makeSqlGF :: String -> [(String, String, String)] -> [(String, String, String)] -> String
makeSqlGF db cols tabs = "--# -path=.:../gen-gf-files\n\nconcrete DB" 
    ++ db ++ "SQL of DB" ++ db ++ " = DatabasesSQL ** {\n\nlin\n\n" 
    ++ colSqlPart cols ++ "\n" 
    ++ tabSqlPart tabs 
    ++ "\n}"

colSqlPart :: [(String, String, String)] -> String
colSqlPart [] = ""
colSqlPart ((n, sing, plur):cols) = "  Col_" ++ (loseSpaces n) ++ " = \"" ++ n ++ "\" ;\n" ++ colSqlPart cols

tabSqlPart :: [(String, String, String)] -> String
tabSqlPart [] = ""
tabSqlPart ((n, sing, plur):tabs) = "  Tab_" ++ (loseSpaces n) ++ " = \"" ++ n ++ "\" ;\n" ++ tabSqlPart tabs

makeNatGF :: String -> String -> [(String, String, String)] -> [(String, String, String)] -> String
makeNatGF db lang cols tabs = "--# -path=.:../gf-rgl/src/morphodict:../gen-gf-files\n\nconcrete DB" 
    ++ db ++ lang ++ " of DB" ++ db ++ " = Databases" ++ lang 
    ++ " ** open Syntax" ++ lang ++ ", (P = Paradigms" ++ lang ++ "), (D = MorphoDict" ++ lang 
    ++ ") in {\n\nlin\n\n" 
    ++ colNatPart cols ++ "\n" 
    ++ tabNatPart tabs 
    ++ "\n}"

colNatPart :: [(String, String, String)] -> String
colNatPart [] = ""
colNatPart ((n,sing,plur):cols) = "  Col_" ++ (loseSpaces n) ++ " = mkCN (P.mkN \"" ++ sing ++ "\" \"" ++ plur ++ "\") ;\n" ++ colNatPart cols

tabNatPart :: [(String, String, String)] -> String
tabNatPart [] = ""
tabNatPart ((n, sing, plur):tabs) = "  Tab_" ++ (loseSpaces n) ++ " = P.mkN \"" ++ sing ++ "\" \"" ++ plur ++ "\" ;\n" ++ tabNatPart tabs

loseSpaces n = map loseSpace n

loseSpace ' ' = '_'
loseSpace c = c

---------------------------------------------
-- Sending SQL to database, getting result
---------------------------------------------

maybeSendToDB db text_out commit_choice = do
    case text_out of
        Nothing -> return (p << "Nothing")
        Just "" -> return (p << "Nothing")
        Just text_out' -> do
            let postgresParameters = "host=localhost port=5432 dbname=" ++ db ++ " user=postgres password=palm77fe" -- lose the password ???
            conn <- connectPostgreSQL postgresParameters
            statement <- prepare conn text_out'
            (execInteger, execString) <- catchSql (sqlExecuter statement) sqlHandler
            case execInteger of
                -2 -> do
                    disconnect conn
                    return $ (textarea << ("Error:\n" ++ execString)) ! [rows "5", cols "100"]
                _ -> do
                    results <- sFetchAllRows' statement
                    resColNames <- getColumnNames statement
                    let processedResults = processTable results
                    when commit_choice $ commit conn
                    disconnect conn
                    return $ (mkHtmlTable resColNames processedResults execInteger)

--------

sqlExecuter statement = do
    execInt <- execute statement []
    return (execInt, "Works fine!")

sqlHandler :: SqlError -> IO (Integer, String) -- any other errors to handle?
sqlHandler (SqlError seState seNativeError seErrorMsg) = do
    return (-2, seErrorMsg)

--------

processTable :: [[Maybe String]] -> [[String]]
processTable table = map processRow table

processRow :: [Maybe String] -> [String]
processRow row = map processCell row

processCell :: Maybe String -> String
processCell (Just val) = val
processCell Nothing = "[null]"

--------

--prettyTable [] = ""
--prettyTable [row] = prettyRow row
--prettyTable (row:rows) = prettyRow row ++ "\n" ++ prettyTable rows

--prettyRow [] = ""
--prettyRow [cell] = cell
--prettyRow (cell:cells) = cell ++ ", " ++ prettyRow cells

--------
-- Merge these with process ones above ???
mkHtmlTable :: [String] -> [[String]] -> Integer -> Html
mkHtmlTable colNames theTable affectedRows = table << tbody << map mkHtmlRow ([["Result: " ++ show affectedRows ++ " rows were modified"], colNames, replicate (length colNames) "-----"] ++ theTable)

mkHtmlRow :: [String] -> Html
mkHtmlRow theRow = tr << (map mkHtmlCell theRow)

mkHtmlCell :: String -> Html
mkHtmlCell cell = td << cell

---------------------------------------------
-- Getting the data input on the language page
---------------------------------------------

someInputs :: [(String, String)] -> String -> [(String, String)]
someInputs [] _ = []
someInputs ((n,v):is) beg | isFieldBeg beg (n,v) = [(drop (length beg) n, map toLower v)] ++ someInputs is beg
                          | otherwise = someInputs is beg

isFieldBeg beg (name, value) = isPrefixOf beg name

extractName beg n = drop (length beg) n -- (splitOn "_" n) !! 1

--[(area, yta), (capital, huvudstad)] [(area, ytor), (capital, huvudstäder)] -> [(area, yta, ytor), (capital, huvudstad, huvudstäder)]
singPlurCombine [] _ = []
singPlurCombine (sing:sings) plurs = (singPlurOne sing plurs) : (singPlurCombine sings plurs)

singPlurOne (sing_n, sing_v) [] = (sing_n, sing_v, sing_v) -- using singular as plural too if not found
singPlurOne (sing_n, sing_v) ((plur_n, plur_v):plurs) | sing_n == plur_n = (sing_n, sing_v, plur_v)
                                                      | otherwise = singPlurOne (sing_n, sing_v) plurs

---------------------------------------------
-- Putting together parts of Html pages
---------------------------------------------

colFieldsTable cs = table << tbody << ([tr << [td << "", td << "Singular", td << "Plural"]] ++ (map colFields cs))

colFields c = tr << [td << c, td << (textfield ("col_sing_" ++ c) ! [value $ map toLower c]), td << (textfield ("col_plur_" ++ c))]

tabFieldsTable ts = table << tbody << ([tr << [td << "", td << "Singular", td << "Plural"]] ++ (map tabFields ts))

tabFields t = tr << [td << t, td << (textfield ("tab_sing_" ++ t)), td << (textfield ("tab_plur_" ++ t) ! [value $ map toLower t])]

langSelector :: [String] -> Html
langSelector ls = langSelectorChosen ls "none"

langSelectorChosen :: [String] -> String -> Html
langSelectorChosen ls chosen = select << map (langSelHelper chosen) ls

langSelHelper :: String -> String -> Html
langSelHelper chosen lang = if (lang == chosen)
                            then (option << lang) ! [value lang, selected]
                            else (option << lang) ! [value lang]
