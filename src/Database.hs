{-# LANGUAGE OverloadedStrings #-}

-- or, on GHCI:
-- > :set -XOverloadedStrings

module Database (
    initialiseDB,
    getOrCreateMovie,
    saveRecords,
    queryMovieAllEntries,
    -- queryTotalMoviesByYear
) where

import Types
import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.Internal
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow

-- See more Database.SQLite.Simple examples at
-- https://hackage.haskell.org/package/sqlite-simple-0.4.18.0/docs/Database-SQLite-Simple.html

instance FromRow Record where
    fromRow = Record <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance FromRow Movie where
    fromRow = Movie <$> field <*> field <*> field <*> field <*> field

instance ToRow Movie where
    toRow (Movie id_ imdbId_ title_ genre_ country_)
        = toRow (id_, imdbId_, title_, genre_, country_)

instance FromRow Entry where
    fromRow = Entry <$> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow Entry where
    
    toRow (Entry year_ director_ imdbRating_ rated_ metaScore_ imdbVotes_ fk_Movie_)
        = toRow (year_ ,director_, imdbRating_, rated_, metaScore_, imdbVotes_, fk_Movie_)

initialiseDB :: IO Connection
initialiseDB = do
        conn <- open "movie.sqlite"
        execute_ conn "CREATE TABLE IF NOT EXISTS countries (\
            \id_ INTEGER PRIMARY KEY AUTOINCREMENT,\
            \imdbId_ VARCHAR(50) NOT NULL,\
            \title_ VARCHAR(80) NOT NULL, \
            \genre VARCHAR(50) NOT NULL, \
            \country VARCHAR(80) NOT NULL \
            \)"
        execute_ conn "CREATE TABLE IF NOT EXISTS entries (\
            \year VARCHAR(40) NOT NULL, \
            \director VARCHAR(40) NOT NULL, \
            \imdbRating VARCHAR(40) DEFAULT NULL, \
            \rated VARCHAR(40) NOT NULL, \
            \metaScore VARCHAR(40) NOT NULL, \
            \imdbVotes_ VARCHAR(40) NOT NULL, \
            \fk_Movie INTEGER\
            \)"
        return conn

getOrCreateMovie :: Connection -> String -> String -> String -> IO Movie
getOrCreateMovie conn title genre country = do
    results <- queryNamed conn "SELECT * FROM movies WHERE title=:title AND genre=:genre" [":title" := title, ":genre" := genre]    
    if length results > 0 then
        return . head $ results
    else do
        execute conn "INSERT INTO movies (title, genre, country) VALUES (?, ?, ?)" (title, genre, country)
        getOrCreateMovie conn title genre country

createRecord :: Connection -> Record -> IO ()
createRecord conn record = do
    movie <- getOrCreateMovie conn (title record) (genre record) (country record)
    let entry = Entry {
        year_ = year record,
        director_ = director record,
        imdbRating_ = imdbRating record,
        rated_ = rated record,
        metaScore_ = metaScore record,
        imdbVotes_ = imdbVotes record,
        fk_Movie_ = id_ movie
    }
    execute conn "INSERT INTO entries VALUES (?,?,?,?,?,?,?)" entry

saveRecords :: Connection -> [Record] -> IO ()
saveRecords conn = mapM_ (createRecord conn)

queryMovieAllEntries :: Connection -> IO [Record]
queryMovieAllEntries conn = do
    putStr "Enter movie name > "
    movie <- getLine
    putStrLn $ "Looking for " ++ movie ++ " entries..."
    let sql = "SELECT year, director, imdbRating, rated, metaScore, imdbVotes, title, genre, country FROM entries inner join movies on entries.fk_title == movies.id WHERE title=?"
    query conn sql [movie]


-- queryTotalMoviesByYear :: Connection -> IO ()
-- queryTotalMoviesByYear conn = do
    -- movieEntries <- queryMovieAllEntries conn
    -- let total = sum (map year movieEntries)
    -- print $ "Total entries: " ++ show(total)
