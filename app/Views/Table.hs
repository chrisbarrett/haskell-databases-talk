{-# LANGUAGE OverloadedStrings #-}
module Views.Table
       (
         render
       , TableView
       , makeTable
       , renderTuple
       )
       where

import qualified Data.Text as T

type Header = T.Text

data TableView = TableView {
      _tableHeader :: Header
    , _tableValues :: [T.Text]
    }

makeTable :: Header -> [T.Text] -> TableView
makeTable = TableView

render :: TableView -> T.Text
render (TableView header values) =
    T.intercalate "\n" [header, sep, content, sep]
  where
    sep = T.replicate (T.length header) "-"
    content = T.intercalate "\n" values

renderTuple :: (Show a, Show b) => (a, b) -> T.Text
renderTuple (a, b) = T.concat [T.pack (show a), ", ", T.pack (show b)]
