{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

-- | Taken from http://www.yesodweb.com/book/persistent.
module Main where

import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Logger    (runStderrLoggingT)
import           Database.Persist
import           Database.Persist.MySQL
import           Database.Persist.TH
import           Control.Monad (void)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name String
    age Int Maybe
    deriving Show
BlogPost
    title String
    authorId PersonId
    deriving Show
|]

connectionInfo :: MySQLConnectInfo
connectionInfo = mkMySQLConnectInfo "localhost" "test" "test" "example"

main :: IO ()
main = runStderrLoggingT $ withMySQLPool connectionInfo 10 $ \pool -> liftIO $ do
          flip runSqlPersistMPool pool $ do
            runMigration migrateAll

            johnId <- insert $ Person "John Doe" $ Just 35
            janeId <- insert $ Person "Jane Doe" Nothing

            void $ insert $ BlogPost "My fr1st p0st" johnId
            void $ insert $ BlogPost "One more for good measure" johnId

            oneJohnPost <- selectList [BlogPostAuthorId ==. johnId] [LimitTo 1]
            liftIO $ print (oneJohnPost :: [Entity BlogPost])

            john <- get johnId
            liftIO $ print (john :: Maybe Person)

            delete janeId
            deleteWhere [BlogPostAuthorId ==. johnId]
