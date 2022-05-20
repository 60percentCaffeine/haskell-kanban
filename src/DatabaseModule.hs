
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables , DisambiguateRecordFields , DuplicateRecordFields , OverloadedLabels  #-}

module DatabaseModule where



import Prelude
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Time()
import Types
import Consts
import GHC.IO



import Data.ByteString            as B
import Data.Text                  as T
import Data.Text.Encoding         as T
{-import Data.Text.IO               as T
import Data.Text.Lazy             as TL
import Data.Text.Lazy.Encoding    as TL
import Data.Text.Lazy.IO          as TL
import Prelude                    as P-}

import Control.Exception

import Data.Char (isSpace)

trim :: String -> String
trim = f . f
   where f = Prelude.reverse . Prelude.dropWhile isSpace



{------------------------------------------------------------------------
 CREATE TABLE sotr (
        sotr_id serial  PRIMARY KEY,
        sotr_fio varchar (50) NOT NULL
    );

    CREATE TABLE  card (
        card_id          varchar (25) PRIMARY KEY,
        card_title       varchar (50) NOT NULL,      
        card_worker_id      integer NOT NULL, 
        card_column      integer check (card_column in (1,2,3)) NOT NULL,

        card_kanban_id   integer NOT NULL, 
        FOREIGN KEY (card_worker_id) REFERENCES sotr(sotr_id)
);                                                                                
---------------------------------------------------------------------------------------------}



data SotrRecord =  SotrRecord{        
                    sotr_id::Int,
                    sotr_fio::String
                  };

data CardRecord = CardRecord {        
                    cr_id::Int,
                    cr_title::String,    
                    cr_worker_id::Int,
                    cr_column::Int,
                    cr_kanban_id::Int                   
                  };








{- helpers / config files. -}
-- Path to config file.
configPath :: FilePath
configPath = "bd.conf"

data BDConfig = BDConfig
    { 
    configHost::String,  
    configPort::String,  
    configDbName::String,  
    configUser::String,
    configPassword::String
    }

contents_to_BDConfig :: String -> Maybe BDConfig
contents_to_BDConfig str = do
  case (Prelude.lines str) of
    [host, port, dbname, user, password] -> Just $ BDConfig (trim host) (trim port) (trim dbname) (trim user) (trim password)
    _ -> Nothing



get_BDConfig :: IO (Maybe BDConfig)
get_BDConfig = do
  contents_str <- Prelude.readFile configPath
  return $ contents_to_BDConfig contents_str



give_String_by_Config :: BDConfig -> String
give_String_by_Config x_config = "host='" ++ configHost x_config            ++ "' " ++
                               "port='" ++ configPort x_config           ++ "' " ++
                               "dbname='" ++ configDbName x_config       ++ "' " ++
                               "user='" ++ configUser x_config           ++ "' " ++
                               "password='" ++ configPassword x_config    ++ "' " 


{- Example "host='127.0.0.1' port=5432  dbname='postgres'  user=testuser password=qwerty" -}
getConnectionString :: BDConfig -> IO (ByteString)
--getConnectionString x_config =  return (   T.encodeUtf8 . T.pack   ( give_String_by_Config x_config     )               )
getConnectionString x_config =  return $   ( T.encodeUtf8 . T.pack $ give_String_by_Config x_config   )





{--------------------------------------------------db logic-----------------------------------------------}

{-currently not supported:   BDSotr Sotr -}
data BDRes  =  BDResOK | BDResErr String | BDInt Int | BDCard Card | BDCardRecord Card | BDSotrRecord SotrRecord
instance Show BDRes where
  show BDResOK = "BDResOK"
  show (BDResErr x) = "BDResErr " ++ x
  show (BDInt x) = "BDInt " ++ (show x)
  show (BDCard (Card _ x _ _)) = "BDCard " ++ x
  show (BDCardRecord (Card _ x _ _)) = "BDCardRecord " ++ x
  show (BDSotrRecord (SotrRecord idd fio)) = "BDSotrRecord" ++ (show idd) ++ " " ++ fio


giveerror ::BDRes
giveerror = BDResErr "helloworld"





handler :: SomeException -> IO ( BDRes )
handler ex = return (BDResErr ( "Caught exception: " ++ show ex ))


{-==================  feature: saving and loading task list; ============= -}


{----------------------add_card_to_db----------------------
returns card id                                      -}
--arguments:      title      w_id    column    kanban
add_card_to_db::  String   -> Int   -> Int   -> Int -> IO ( BDRes )
add_card_to_db   x_title x_worker_id x_column x_kanban = do
  mixedBDConfig <- get_BDConfig
  case mixedBDConfig of
    Nothing -> return  ( BDResErr "Error: highly likely config is defected" )
    Just cfg_str -> catch ( do 
      bytestr <- getConnectionString cfg_str
      conn <- connectPostgreSQL(bytestr)            
      x::[Only Int] <- query conn "insert into card  (card_title, card_worker_id, card_column, card_kanban_id)  VALUES ( ?, ?, ?, ?) RETURNING card_id;  " ( x_title, x_worker_id, x_column, x_kanban) 

      case x of
        (number:_) -> return $ BDCard ( Card (Just $ fromOnly number) x_title x_worker_id []  )
        [] -> return  ( BDResErr "Error: there may be an error in insert: the key exists" )                          )           
      handler





{----------------------update_card_in_db----------------------}
--arguments: card  
update_card_in_db :: Int -> String -> Int -> Int -> Int ->  IO ( BDRes )
update_card_in_db card_id card_title card_wid column_number canban_number = do
  mixedBDConfig <- get_BDConfig
  case mixedBDConfig of
    Nothing -> return  ( BDResErr "Error: highly likely config is defected" )
    Just cfg_str -> catch ( do 
      bytestr <- getConnectionString cfg_str
      conn <- connectPostgreSQL(bytestr)            
      
      x::[Only Int] <- query conn "UPDATE card SET card_title = ? ,  card_worker_id = ?, card_column = ?, card_kanban_id = ?  WHERE card_id = ? RETURNING card_id;  " (card_title, card_wid, column_number, canban_number, card_id) 
             
      case x of 
        [] ->  return (BDResErr "Update failed")
        _   -> return (BDResOK )                         )     
      
      handler



record_to_card :: CardRecord ->  Card
record_to_card card_record = Card (Just $ cr_id card_record)  (cr_title card_record) (cr_worker_id card_record) []


records_to_lists :: [CardRecord] -> Int -> [[Card]] 
records_to_lists _ 0 =   []
records_to_lists cards number =    ( [ (record_to_card x )  | x <- cards , (cr_column x) == (columnAmount - number + 1) ]   :  records_to_lists cards ( number - 1) )

load_cards :: IO [(Int, String, Int, Int, Int)]
load_cards = do
  str <- get_BDConfig
  case str of
    Nothing -> return []
    Just cfg_str -> catch ( do
      bytestr <- getConnectionString cfg_str
      conn <-  connectPostgreSQL(bytestr)    

      x:: [(Int, String, Int, Int, Int)] <- query_ conn "SELECT * FROM card ;  "

      return x )

      handler_card

handler_card :: SomeException -> IO [(Int, String, Int, Int, Int)]
handler_card _ = return []




{-=================== feature: users db;====================-}

handler_str :: SomeException -> IO [String]
handler_str _ = return []

concat_arr :: [[a]] -> [a]
concat_arr [] = []
concat_arr (x: xz) = x ++ concat_arr xz

load_users_from_db :: IO [String]
load_users_from_db = do
  str <- get_BDConfig
  case str of
    Nothing -> return []
    Just cfg_str -> catch ( do
      bytestr <- getConnectionString cfg_str
      conn <-  connectPostgreSQL(bytestr)    
      --actual_update_card_in_db canban_number conn )

      x:: [[String]] <- query_ conn "SELECT sotr_fio FROM sotr ORDER BY sotr_id ;  "

      return $ concat_arr x )

      handler_str





handler_str_undef :: SomeException -> IO String
handler_str_undef ex = return $ show ex



get_user_FIO_by_sotr_ID :: Int ->  IO ( String )
get_user_FIO_by_sotr_ID _sotr_id = catch ( do
  str <- get_BDConfig
  case str of
    Nothing -> return "undefined"
    Just cfg_str -> do
      bytestr <- getConnectionString cfg_str
      conn <-  connectPostgreSQL (bytestr)  

      card_records::[[String]] <- query conn "SELECT sotr_fio from sotr WHERE sotr_id = ? ;  " (Only _sotr_id) 

      case (concat_arr card_records) of 
        (x_string:_) -> return x_string
        [] -> return "undefined"        ) 
     
  handler_str_undef


update_user_in_db :: Int -> String ->  IO ( BDRes )
update_user_in_db _sotr_id _new_name = catch ( do
  str <- get_BDConfig
  case str of
    Nothing -> return ( BDResErr "Error: highly likely config is defected" )
    Just cfg_str -> do
      bytestr <- getConnectionString cfg_str
      conn <-  connectPostgreSQL (bytestr)  

      x::[Only Int] <- query conn "UPDATE sotr SET sotr_fio = ? WHERE sotr_id = ? RETURNING sotr_id;  " (_new_name, _sotr_id) 

      case x of 
        [] ->  return (BDResErr "Update failed")
        _   -> return (BDResOK )        ) 
    handler


add_user_to_db :: String ->  IO ( BDRes )
add_user_to_db _new_name = catch ( do
  str <- get_BDConfig
  case str of
    Nothing -> return ( BDResErr "Error: highly likely config is defected" )
    Just cfg_str -> do
      bytestr <- getConnectionString cfg_str
      conn <-  connectPostgreSQL (bytestr)  

      x::[Only Int] <- query conn "INSERT INTO sotr (sotr_fio) VALUES(?) RETURNING sotr_id;  " (Only _new_name) 

      case x of 
        [] ->  return (BDResErr "Insert failed")
        _   -> return (BDResOK )        ) 
     
  handler


