{-# LANGUAGE DeriveGeneric #-}

module Types (
    Entry (..),
    Movie (..),
    Record (..),
    Records (..)
) where

import GHC.Generics ( Generic )

data Entry = Entry {
    year_ :: String,
    director_ :: String,
    imdbRating_ :: String,
    rated_ ::  String,
    metaScore_ :: String,
    imdbVotes_ :: String,
    fk_Movie_ :: Int
} deriving (Show)

data Movie = Movie {
    id_ :: Int,
    imdbId_ :: String,
    title_ :: String,
    genre_ :: String,
    country_ :: String
} deriving (Show)

data Record = Record {
    year :: String,
    director :: String,
    imdbRating :: String,
    rated ::  String,
    metaScore :: String,
    imdbVotes :: String,
    title :: String,
    genre :: String,
    country :: String
} deriving (Show, Generic)

data Records = Records {
    records :: [Record]
} deriving (Show, Generic)
