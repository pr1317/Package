module Parse (
    parseRecords,
) where
    
import Types
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L8

renameFields "year" = "Year"
renameFields "genre" = "Genre"
renameFields "title" = "Title"
renameFields "country" = "Country" 
renameFields other = other

customOptions = defaultOptions {
    fieldLabelModifier = renameFields
}

instance FromJSON Record where
    parseJSON = genericParseJSON customOptions

instance FromJSON Records

parseRecords :: L8.ByteString -> Either String Records
parseRecords json = eitherDecode json :: Either String Records