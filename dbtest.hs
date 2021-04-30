module Dbtest where

import Database.HDBC
import Database.HDBC.PostgreSQL

main :: IO ()
main = do
    con <- connectPostgreSQL "PostgreSQL 13:5432 Countries postgres saturn25au"
    let theQuery = "SELECT * FROM Countries ;"
    st <- query con theQuery
    putStrLn st
