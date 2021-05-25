import Network.CGI
import Text.XHtml
import Data.Char

import PGF
import System.Process

import Control.Monad (liftM)
import Data.Maybe (fromMaybe)
import Data.List
import Data.List.Split
import Data.Text (pack)
import Data.Text.IO.Utf8
import System.Directory

import Database.HDBC
import Database.HDBC.PostgreSQL (connectPostgreSQL)

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

language_page :: String -> [(String, String)] -> [String] -> [String] -> Html
language_page db_choice lang_list tabs cols = h1 << "Language adding"
          +++ form << ([hidden "page_id" "language",
                        hidden "db_choice" db_choice,
                        paragraph << ("Language to add for the " ++ db_choice ++ " database: " +++ (langSelector lang_list) ! [name "lang_sel"]),
                        paragraph << ("Please type names in that language for the tables and columns of the " ++ db_choice ++ " database:"),
                        fieldset << ([legend << ("Tables: ")] ++ tabFields tabs),
                        fieldset << ([legend << ("Columns: ")] ++ colFields cols),
                        paragraph << (reset "" "Clear all" +++ submit "button_id" "Choose another database"),
                        submit "button_id" "Submit and enter translator"])

translation_page :: String -> String -> [(String, String)] -> String -> String -> String -> Html -> Html
translation_page db_choice text_in lang_list lang_in lang_out text_out db_out = h1 << "Translation"
          +++ form << [hidden "page_id" "translation",
                        hidden "db_choice" db_choice,
                        (paragraph << ("From: " +++ (langSelectorChosen lang_list lang_in) ! [name "in_sel"] +++ " To: " +++ (langSelectorChosen lang_list lang_out) ! [name "out_sel"])),
                        (paragraph << ("Input: " +++ (textfield "in_field") ! [value text_in, size "100"])),
                        submit "button_id" "Translate",
                        paragraph << ("Output: " +++ (textfield "out_field") ! [value text_out, size "100"]),
                        paragraph << ("Using the database: " ++ db_choice ++ " " +++ submit "button_id" "Choose another database" +++ submit "button_id" "Add a language for this database"),
                        paragraph << ("Send SQL code to " ++ db_choice ++ " database? " +++ submit "button_id" "Send to DB")
                        ]
          +++ fieldset << ([legend << ("Output from the database"), db_out])
          --(textarea << db_out) ! [rows "30", cols "100"]
          -- +++ (mkTable testTab)
          -- +++ (simpleTable [] [] [[toHtmlFromList "a", toHtmlFromList "b"], [toHtmlFromList "c", toHtmlFromList "d"]])
          --(toHtml $ above (beside (toHtmlFromList "table?") (toHtmlFromList "cell 2?")) (beside (toHtmlFromList "table 3?") (toHtmlFromList "cell 4?"))) -- table?

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
main = runCGI $ handleErrors cgiMain

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
                             (tabs', cols') <- getTabsAndCols db_choice'
                             -- Putting data into next page
                             showPage $ language_page db_choice' testLangs tabs' cols'
                         Just "Choose and enter translator" -> do
                             -- Getting data out from prev page
                             db_choice <- getInput "db_choice"
                             -- Peforming calculations
                             let db_choice' = maybe "error" id db_choice
                             --gr <- liftIO $ readPGF "gf-files/DBCountries.pgf"
                             --liftIO $ callCommand "gf -make ../DBCountriesSQL.gf ../DBCountriesEng.gf"
                             -- langs <- genPGF db_choice'
                             -- GENERATE PGF with correct files, all available langs for it------------------------------------------------
                             -- Get available languages for this db (names and acronyms), give to translation_page-------------------------
                             -- Putting data into next page
                             showPage $ translation_page db_choice' "" testLangs "none" "none" "" (p << "Nothing")
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
                             let col_inputs' = someInputs inputs "col_"
                             let tab_inputs' = someInputs inputs "tab_"
                             boolTest1 <- liftIO $ doesFileExist "gf-files/DBanimalsEng.gf"
                             -- also write gf files for abs and sql if needed--------------------------------------------------------------------
                             liftIO $ makeGFFile db_choice' lang_sel' col_inputs' tab_inputs' -- writes/overwrites this file
                             boolTest2 <- liftIO $ doesFileExist "gf-files/DBanimalsEng.gf"
                             
                             -- gr <- liftIO $ readPGF "DBanimals.pgf"
                             -- let langs = languages gr
                             --langName = (testLangMap ! . getLangCode) lang
                             
                             --liftIO $ helpTemp -- callCommand "help" -- "gf -make gf-files/DBanimalsSQL.gf" -- it returns this to the client for some reason
                             let tempPrint = "boolTest1: " ++ show boolTest1 ++ ", boolTest2: " ++ show boolTest2 -- ++ ", x: " ++ show x -- ++ ", langs: " ++ show langs
                             -- GENERATE PGF with correct files, all available langs for it-----------------------------------------------
                             -- Get available languages for this db (names and acronyms), give to translation_page-------------------------
                             -- Putting data into next page
                             showPage $ translation_page db_choice' "" testLangs "none" "none" "" (p << tempPrint) --"Nothing"
                         _ -> output "Error: Language page, unknown button"
                 Just "translation" ->
                     case button_id of
                         Just "Translate" -> do
                             -- Getting data out from prev page
                             db_choice <- getInput "db_choice"
                             text_in <- getInput "in_field"
                             lang_in <- getInput "in_sel"
                             lang_out <- getInput "out_sel"
                             -- Peforming calculations
                             let db_choice' = maybe "error" id db_choice
                             let text_in' = maybe "error" id text_in
                             let lang_in' = maybe "error" id lang_in
                             let lang_out' = maybe "error" id lang_out
                             text_out' <- liftIO $ translate db_choice' text_in' lang_in' lang_out'
                             -- Putting data into next page
                             showPage $ translation_page db_choice' text_in' testLangs lang_in' lang_out' text_out' (p << "Nothing")
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
                             (tabs', cols') <- getTabsAndCols db_choice'
                             -- Putting data into next page
                             showPage $ language_page db_choice' testLangs tabs' cols'
                         Just "Send to DB" -> do
                             -- Getting data out from prev page
                             db_choice <- getInput "db_choice"
                             text_in <- getInput "in_field"
                             lang_in <- getInput "in_sel"
                             lang_out <- getInput "out_sel"
                             text_out <- getInput "out_field"
                             -- Peforming calculations
                             let db_choice' = maybe "error" id db_choice
                             let text_in' = maybe "error" id text_in
                             let lang_in' = maybe "error" id lang_in
                             let lang_out' = maybe "error" id lang_out
                             let text_out' = maybe "error" id text_out
                             db_out' <- liftIO $ maybeSendToDB text_out
                             -- Putting data into next page
                             showPage $ translation_page db_choice' text_in' testLangs lang_in' lang_out' text_out' db_out'
                         _ -> output "Error: Translation page, unknown button"
                 Nothing -> showPage $ database_page
                 _ -> output "Error: Unknown page"

---------------------------------------------
-- Temporary functions area
---------------------------------------------



---------------------------------------------
-- Getting tables and columns for a database (WIP)
---------------------------------------------

getTabsAndCols db = do
    return (testTabs, testCols)

---------------------------------------------
-- PGF generation and usage (WIP)
---------------------------------------------

helpTemp = do
    callCommand "dir" -- "gf -make DBcountriesSQL.gf DBcountriesEng.gf"

genPGF db = do
    callCommand "gf -make ../DBCountriesSQL.gf ../DBCountriesEng.gf"
    return ["a", "b", "c"]

translate :: String -> String -> String -> String -> IO (String)
translate db s in_l out_l = do
    gr <- liftIO $ readPGF "../DBcountries.pgf"
    let stCat = startCat gr
    let fromLang = maybe undefined id (readLanguage ("DBcountries" ++ in_l))
    let absTrees = parse gr fromLang stCat s
    let toLang = maybe undefined id (readLanguage ("DBcountries" ++ out_l))
    case absTrees of
        [] -> return ("Couldn't parse")
        (at:ats) -> do
            return (linearize gr toLang at)
    --"translation of " ++ s ++ " from " ++ in_l ++ " to " ++ out_l

---------------------------------------------
-- Temporary hardwirings (WIP)
---------------------------------------------

testCols = ["name", "capital", "area", "population", "continent", "currency", "code"]
testTabs = ["countries", "currencies"]
testLangs = [("English", "Eng"), ("Swedish", "Swe"), ("Dutch", "Dut"), ("SQL", "SQL")]
--testLangMap = fromList [("Eng", "English"), ("Swe", "Swedish"), ("Dut", "Dutch"), ("SQL", "SQL")]

---------------------------------------------
-- Making .gf files (WIP)
---------------------------------------------

makeGFFile db lang cols tabs = do
    Data.Text.IO.Utf8.writeFile ("gf-files/DB" ++ db ++ lang ++ ".gf") (pack (gfFileContents db lang cols tabs))

gfFileContents db lang cols tabs = "--# -path=.:../gf-rgl/src/morphodict\r\rconcrete DB" 
    ++ db ++ lang ++ " of DB" ++ db ++ " = Databases" ++ lang 
    ++ " ** open (P = Paradigms" ++ lang ++ "), (D = MorphoDict" ++ lang 
    ++ ") in {\r\rlin\r\r" 
    ++ colPrint cols ++ "\r" 
    ++ tabPrint tabs 
    ++ "\r}"

colPrint [] = ""
colPrint ((n,v):cols) = "  Col_" ++ n ++ " = P.mkN \"" ++ v ++ "\" ;\r" ++ colPrint cols

tabPrint [] = ""
tabPrint ((n,v):tabs) = "  Tab_" ++ n ++ " = P.mkN \"" ++ v ++ "\" ;\r" ++ tabPrint tabs

---------------------------------------------
-- Sending SQL to database, getting result (Pretty much done)
---------------------------------------------

maybeSendToDB text_out = do
    case text_out of
        Nothing -> return (p << "nothing to show yet1")
        Just "" -> return (p << "nothing to show yet2")
        Just text_out' -> do
            let postgresParameters = "host=localhost port=5432 dbname=countries user=postgres"
            conn <- connectPostgreSQL postgresParameters
            statement <- prepare conn text_out'
            (execInteger, execString) <- catchSql (sqlExecuter statement) sqlHandler
            results <- sFetchAllRows' statement
            resColNames <- getColumnNames statement
            let processedResults = processTable results
            --let prettyResults = prettyTable processedResults
            disconnect conn
            case execInteger of
                -2 -> return $ (textarea << ("Error:\n" ++ execString)) ! [rows "5", cols "100"]
                _ -> return $ (mkHtmlTable resColNames processedResults) --prettyResults

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

--testTable = [
--    [Just "top left", Just "top", Just "top right"],
--    [Just "left", Just "middle", Just "right"],
--    [Just "bottom left", Just "bottom", Just "bottom right"]]

--------

--prettyTable [] = ""
--prettyTable [row] = prettyRow row
--prettyTable (row:rows) = prettyRow row ++ "\n" ++ prettyTable rows

--prettyRow [] = ""
--prettyRow [cell] = cell
--prettyRow (cell:cells) = cell ++ ", " ++ prettyRow cells

--------
-- Merge these with process ones above ???
mkHtmlTable :: [String] -> [[String]] -> Html
mkHtmlTable colNames theTable = table << tbody << map mkHtmlRow ([colNames, replicate (length colNames) "-----"] ++ theTable)

mkHtmlRow :: [String] -> Html
mkHtmlRow theRow = tr << (map mkHtmlCell theRow)

mkHtmlCell :: String -> Html
mkHtmlCell cell = td << cell

--testTab = [["a", "brrrrrrrrrrrrrrrr"], ["cdddddddddddddddd", "d"]]

---------------------------------------------
-- Getting the data input on the language page (Done?)
---------------------------------------------

someInputs :: [(String, String)] -> String -> [(String, String)]
someInputs [] _ = []
someInputs ((n,v):is) beg | isFieldBeg beg (n,v) = [(extractName n, map toLower v)] ++ someInputs is beg
                          | otherwise = someInputs is beg

--isColField = isFieldBeg "col_"
--isTabField = isFieldBeg "tab_"

isFieldBeg beg (name, value) = isPrefixOf beg name

extractName n = (splitOn "_" n) !! 1

---------------------------------------------
-- Putting together parts of Html pages (Done?)
---------------------------------------------

colFields [] = []
colFields (c:cs) = [paragraph << (c ++ " " +++ (textfield ("col_" ++ c ++ "_input") ! [value $ map toLower c]))] ++ colFields cs

tabFields [] = []
tabFields (c:cs) = [paragraph << (c ++ " " +++ (textfield ("tab_" ++ c ++ "_input") ! [value $ map toLower c]))] ++ tabFields cs

langSelector :: [(String, String)] -> Html
langSelector ls = langSelectorChosen ls "none"

langSelectorChosen :: [(String, String)] -> String -> Html
langSelectorChosen ls chosen = select << map (langSelHelper chosen) ls

langSelHelper :: String -> (String, String) -> Html
langSelHelper chosen (name, abbr) = if (abbr == chosen)
                                    then (option << name) ! [value abbr, selected]
                                    else (option << name) ! [value abbr]
