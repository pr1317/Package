module Main where

import System.IO
import Types
import Fetch
import Parse
import Database

main :: IO ()
main = do
    putStrLn "---------------------------------"
    putStrLn "  Welcome to the Movie data app  "
    putStrLn "  (1) Download data              "
    putStrLn "  (2) Show All Movies     "
    putStrLn "  (3) Movies by year    "
    putStrLn "  (4) Quit                       "
    putStrLn "---------------------------------"
    conn <- initialiseDB
    hSetBuffering stdout NoBuffering
    putStr "Choose an option > "
    option <- readLn :: IO Int
    case option of
        1 -> do
            let url = "https://gist.githubusercontent.com/saniyusuf/406b843afdfb9c6a86e25753fe2761f4/raw/523c324c7fcc36efab8224f9ebb7556c09b69a14/Film.JSON"
            print "Downloading..."
            json <- download url
            print "Parsing..."
            case (parseRecords json) of
                Left err -> print err
                Right recs -> do
                    print "Saving on DB..."
                    saveRecords conn (records recs)
                    print "Saved!"
                    main
        2 -> do
            entries <- queryMovieAllEntries conn
            mapM_ print entries
            main
        -- 3 -> do
            -- queryAllMovies conn
            -- main
        4 -> print "Hope you've enjoyed using the app!"
        otherwise -> print "Invalid option"

