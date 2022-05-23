{-# LANGUAGE OverloadedStrings, ScopedTypeVariables , 

DisambiguateRecordFields , DuplicateRecordFields , OverloadedLabels  #-}



module DatabaseModule where





{-import of needed libraries-}

import Prelude

import Database.PostgreSQL.Simple

import Database.PostgreSQL.Simple.Time()

import Types

import Consts

import GHC.IO







import Data.ByteString            as B

import Data.Text                  as T

import Data.Text.Encoding         as T



import Control.Exception



import Data.Char (isSpace)







trim :: String -> String

trim = f . f

   where f = Prelude.reverse . Prelude.dropWhile isSpace





{-used data types-}

data SotrRecord =  SotrRecord{        

                    employee_id::Int,

                    employee_fullname::String

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





{-reading config contents to config if it is possible -}

contents_to_BDConfig :: String -> Maybe BDConfig

contents_to_BDConfig str = do

  case (Prelude.lines str) of

    [host, port, dbname, user, password] -> Just $ BDConfig

        (trim host) (trim port) (trim dbname) (trim user) (trim password)

        

    _ -> Nothing





{-gettin config if it is possible (otherwise nothing)-}

get_BDConfig :: IO (Maybe BDConfig)

get_BDConfig = do

  contents_str <- Prelude.readFile configPath

  return $ contents_to_BDConfig contents_str







give_String_by_Config :: BDConfig -> String

give_String_by_Config x_config = 

        "host='" ++ configHost x_config          ++ "' " ++

        "port='" ++ configPort x_config            ++ "' " ++

        "dbname='" ++ configDbName x_config       ++ "' " ++

        "user='" ++ configUser x_config           ++ "' " ++

        "password='" ++ configPassword x_config    ++ "' " 







{- Example "host='127.0.0.1' port=5432  dbname='postgres' \

    \user=testuser password=qwerty" -}

getConnectionString :: BDConfig -> IO (ByteString)               

getConnectionString x_config =  return $   ( T.encodeUtf8 . T.pack 

    $ give_String_by_Config x_config   )











{------------------db logic---------------}



{-currently not supported:   BDSotr Sotr -}

data BDRes  =  BDResOK | BDResErr String | BDInt Int |

    BDCard Card | BDCardRecord Card | BDSotrRecord SotrRecord

instance Show BDRes where

  show BDResOK = "BDResOK"

  show (BDResErr x) = "BDResErr " ++ x

  show (BDInt x) = "BDInt " ++ (show x)

  show (BDCard (Card _ x _ _)) = "BDCard " ++ x

  show (BDCardRecord (Card _ x _ _)) = "BDCardRecord " ++ x

  show (BDSotrRecord (SotrRecord idd fullname)) = "BDSotrRecord" ++

    (show idd) ++ " " ++ fullname





{--------Error processing --------}

giveerror ::BDRes

giveerror = BDResErr "error occured"









{-------Handler for catching exceptions --------}

handler :: SomeException -> IO ( BDRes )

handler ex = return (BDResErr ( "Caught exception: " ++ show ex ))

















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

      x::[Only Int] <- query conn "insert into card\

        \(card_title, card_worker_id, card_column, card_kanban_id) \

        \ VALUES ( ?, ?, ?, ?) RETURNING card_id;  " 

        ( x_title, x_worker_id, x_column, x_kanban) 



      case x of

        (number:_) -> return $ BDCard ( Card (Just $ fromOnly number)

            x_title x_worker_id []  )

        [] -> return  ( BDResErr "Error: there may be an error in insert:\

        \the key exists" )                          )           

      handler











{----------------------update_card_in_db----------------------}

--arguments: card_id, parameters, returns number of card

update_card_in_db :: Int -> String -> Int -> Int -> Int ->  IO ( BDRes )

update_card_in_db card_id card_title card_wid column_number canban_number = do

  mixedBDConfig <- get_BDConfig

  case mixedBDConfig of

    Nothing -> return  ( BDResErr "Error: highly likely config is defected" )

    Just cfg_str -> catch ( do 

      bytestr <- getConnectionString cfg_str

      conn <- connectPostgreSQL(bytestr)            

      

      x::[Only Int] <- query conn "UPDATE card SET card_title = ? , \

        \ card_worker_id = ?, card_column = ?, card_kanban_id = ?    \

        \ WHERE card_id = ? RETURNING card_id;  " 

        (card_title, card_wid, column_number, canban_number, card_id) 

             

      case x of 

        [] ->  return (BDResErr "Update failed")

        _   -> return (BDResOK )                         )     

      

      handler







{----------------------transforms db to card ---------------------}

record_to_card :: CardRecord ->  Card

record_to_card card_record = Card (Just $ cr_id card_record)  

    (cr_title card_record) (cr_worker_id card_record) []



{----------------------transforms [cardrecords] to [[card]]

for supporting columns in kanban ---------------------}

records_to_lists :: [CardRecord] -> Int -> [[Card]] 

records_to_lists _ 0 =   []

records_to_lists cards number =    ( [ (record_to_card x )  | x <- cards , 

    (cr_column x) == (columnAmount - number + 1) ]   :  

    records_to_lists cards ( number - 1) )







{----------------------load all cards from db ---------------------}

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





{----------------------gets all names of employees ---------------------}

load_users_from_db :: IO [String]

load_users_from_db = do

  str <- get_BDConfig

  case str of

    Nothing -> return []

    Just cfg_str -> catch ( do

      bytestr <- getConnectionString cfg_str

      conn <-  connectPostgreSQL(bytestr)    

      --actual_update_card_in_db canban_number conn )



      x:: [[String]] <- query_ conn "SELECT employee_fullname FROM employee ORDER BY employee_id ;  "



      return $ concat_arr x )



      handler_str











handler_str_undef :: SomeException -> IO String

handler_str_undef ex = return $ show ex







get_user_FIO_by_employee_ID :: Int ->  IO ( String )

get_user_FIO_by_employee_ID _employee_id = catch ( do

  str <- get_BDConfig

  case str of

    Nothing -> return "undefined"

    Just cfg_str -> do

      bytestr <- getConnectionString cfg_str

      conn <-  connectPostgreSQL (bytestr)  



      card_records::[[String]] <- query conn "SELECT employee_fullname from employee WHERE employee_id = ? ;  "

       (Only _employee_id) 



      case (concat_arr card_records) of 

        (x_string:_) -> return x_string

        [] -> return "undefined"        ) 

     

  handler_str_undef





update_user_in_db :: Int -> String ->  IO ( BDRes )

update_user_in_db _employee_id _new_name = catch ( do

  str <- get_BDConfig

  case str of

    Nothing -> return ( BDResErr "Error: highly likely config is defected" )

    Just cfg_str -> do

      bytestr <- getConnectionString cfg_str

      conn <-  connectPostgreSQL (bytestr)  



      x::[Only Int] <- query conn "UPDATE employee SET employee_fullname = ? \

        \WHERE employee_id = ? RETURNING employee_id;  " (_new_name, _employee_id) 



      case x of 

        [] ->  return (BDResErr "Update failed")

        _   -> return (BDResOK )        ) 

    handler





add_user_to_db :: String ->  IO ( BDRes )

add_user_to_db _new_name = catch ( do

  str <- get_BDConfig

  case str of

    Nothing -> return ( BDResErr "Error: highlsot likely config is defected" )

    Just cfg_str -> do

      bytestr <- getConnectionString cfg_str

      conn <-  connectPostgreSQL (bytestr)  



      x::[Only Int] <- query conn "INSERT INTO employee (employee_fullname) \

        \VALUES(?) RETURNING employee_id;  " (Only _new_name) 



      case x of 

        [] ->  return (BDResErr "Insert failed")

        _   -> return (BDResOK )        ) 

     

  handler





