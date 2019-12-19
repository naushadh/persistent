{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module InsertDuplicateUpdate where

import Data.List              (sort)

import Database.Persist.MySQL
import MyInit

share [mkPersist sqlSettings, mkMigrate "duplicateMigrate"] [persistUpperCase|
  Item
     name        Text sqltype=varchar(80)
     description Text
     price       Double Maybe
     quantity    Int Maybe

     Primary name
     deriving Eq Show Ord

  ItemSize
     Id          (Key Item) sqltype=varchar(80)
     size        Int
     deriving Eq Show Ord
|]

specs :: Spec
specs = describe "DuplicateKeyUpdate" $ do
  let item1 = Item "item1" "" (Just 3) Nothing
      item2 = Item "item2" "hello world" Nothing (Just 2)
      items = [item1, item2]
      item1Size = ItemSize 10
      item2Size = ItemSize 17
      itemsSize = [item1Size, item2Size]
  describe "insertOnDuplicateKeyUpdate" $ do
    it "inserts appropriately" $ db $ do
      deleteWhere ([] :: [Filter Item])
      insertOnDuplicateKeyUpdate item1 [ItemDescription =. "i am item 1"]
      Just item <- get (ItemKey "item1")
      item @== item1

    it "performs only updates given if record already exists" $ db $ do
      deleteWhere ([] :: [Filter Item])
      let newDescription = "I am a new description"
      _ <- insert item1
      insertOnDuplicateKeyUpdate
        (Item "item1" "i am inserted description" (Just 1) (Just 2))
        [ItemDescription =. newDescription]
      Just item <- get (ItemKey "item1")
      item @== item1 { itemDescription = newDescription }

  describe "insertEntityOnDuplicateKeyUpdate" $ do
    it "inserts appropriately" $ db $ do
      deleteWhere ([] :: [Filter Item])
      deleteWhere ([] :: [Filter ItemSize])
      key <- insert item1
      insertEntityOnDuplicateKeyUpdate (Entity (ItemSizeKey key) item1Size) [ItemSizeSize =. 42]
      Just itemSize <- get (ItemSizeKey key)
      itemSize @== item1Size

    it "performs only updates given if record already exists" $ db $ do
      deleteWhere ([] :: [Filter Item])
      deleteWhere ([] :: [Filter ItemSize])
      let newCount = 13
      key <- insert item1
      insertKey (ItemSizeKey key) item1Size
      insertEntityOnDuplicateKeyUpdate
        (Entity (ItemSizeKey key) item1Size)
        [ItemSizeSize =. newCount]
      Just itemSize <- get (ItemSizeKey key)
      itemSize @== item1Size { itemSizeSize = newCount }

  describe "insertManyOnDuplicateKeyUpdate" $ do
    it "inserts fresh records" $ db $ do
      deleteWhere ([] :: [Filter Item])
      insertMany_ items
      let newItem = Item "item3" "fresh" Nothing Nothing
      insertManyOnDuplicateKeyUpdate
        (newItem : items)
        [copyField ItemDescription]
        []
      dbItems <- map entityVal <$> selectList [] []
      sort dbItems @== sort (newItem : items)
    it "updates existing records" $ db $ do
      deleteWhere ([] :: [Filter Item])
      insertMany_ items
      insertManyOnDuplicateKeyUpdate
        items
        []
        [ItemQuantity +=. Just 1]
    it "only copies passing values" $ db $ do
      deleteWhere ([] :: [Filter Item])
      insertMany_ items
      let newItems = map (\i -> i { itemQuantity = Just 0, itemPrice = fmap (*2) (itemPrice i) }) items
          postUpdate = map (\i -> i { itemPrice = fmap (*2) (itemPrice i) }) items
      insertManyOnDuplicateKeyUpdate
        newItems
        [ copyUnlessEq ItemQuantity (Just 0)
        , copyField ItemPrice
        ]
        []
      dbItems <- sort . fmap entityVal <$> selectList [] []
      dbItems @== sort postUpdate
    it "inserts without modifying existing records if no updates specified" $ db $ do
      let newItem = Item "item3" "hi friends!" Nothing Nothing
      deleteWhere ([] :: [Filter Item])
      insertMany_ items
      insertManyOnDuplicateKeyUpdate
        (newItem : items)
        []
        []
      dbItems <- sort . fmap entityVal <$> selectList [] []
      dbItems @== sort (newItem : items)

  describe "insertEntityManyOnDuplicateKeyUpdate" $ do
    it "inserts fresh records" $ db $ do
      deleteWhere ([] :: [Filter Item])
      deleteWhere ([] :: [Filter ItemSize])
      keys <- insertMany items
      let entities = zipWith (Entity . ItemSizeKey) keys itemsSize
      void $ insertEntityMany $ tail entities
      insertEntityManyOnDuplicateKeyUpdate
        entities
        [copyField ItemSizeSize]
        []
      dbItems <- selectList [] []
      sort dbItems @== sort entities
    it "updates existing records" $ db $ do
      deleteWhere ([] :: [Filter Item])
      deleteWhere ([] :: [Filter ItemSize])
      keys <- insertMany items
      let entities = zipWith (Entity . ItemSizeKey) keys itemsSize
      void $ insertEntityMany entities
      insertEntityManyOnDuplicateKeyUpdate
        entities
        []
        [ItemSizeSize +=. 1]
      dbItems <- selectList [] []
      sort dbItems @== sort (map (\(Entity k v) -> Entity k (v { itemSizeSize = itemSizeSize v + 1 })) entities)