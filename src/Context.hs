{-
    Модуль, отвечающий за формирование базового контекста статей.
    https://github.com/denisshevchenko/ruhaskell
    Все права принадлежат русскоязычному сообществу Haskell-разработчиков, 2015 г.
-}

{-# LANGUAGE OverloadedStrings #-}

module Context (
    postContext
) where

import Data.Monoid      (mconcat)
import Data.List        (intersperse)
import System.Locale    
import Misc             (aHost, TagsAndAuthors, getNameOfAuthor)

import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A
import           Text.Blaze.Html                (toHtml, toValue, (!))

import Hakyll

-- Код данной функции для формирования простой ссылки взят из исходников Hakyll.
simpleRenderLink :: String 
                 -> (Maybe FilePath) 
                 -> Maybe H.Html
simpleRenderLink _   Nothing         = Nothing
simpleRenderLink tag (Just filePath) =
    -- Формируем тег <a href...>
    Just $ H.a ! A.href (toValue $ toUrl filePath) $ toHtml tag

-- Превращает имя автора в ссылку, ведущую к списку статей данного автора.
authorField :: String -> Tags -> Context a
authorField = tagsFieldWith getNameOfAuthor simpleRenderLink (mconcat . intersperse ", ")

-- Локализация в данном случае задаётся только для русских названий месяцев.
-- Остальные поля типа TimeLocale инициализированы пустыми значениями.
ruTimeLocale :: TimeLocale
ruTimeLocale =  TimeLocale { wDays  = []
                           , months = [("января",   "янв"),  ("февраля", "фев"),
                                       ("марта",    "мар"),  ("апреля",  "апр"),
                                       ("мая",      "мая"),  ("июня",    "июн"),
                                       ("июля",     "июл"),  ("августа", "авг"),
                                       ("сентября", "сент"), ("октября", "окт"),
                                       ("ноября",   "нояб"), ("декабря", "дек")]
                           , intervals = []
                           , amPm = ("", "")
                           , dateTimeFmt = "" 
                           , dateFmt = ""
                           , timeFmt = ""
                           , time12Fmt = ""
                           }

-- Основной контекст публикаций.
postContext :: TagsAndAuthors -> Context String
postContext tagsAndAuthors = mconcat [ constField "host" aHost
                                     , dateFieldWith ruTimeLocale "date" "%d %B %Y"
                                     , tagsField "postTags" $ fst tagsAndAuthors
                                     , authorField "postAuthor" $ snd tagsAndAuthors
                                     , defaultContext
                                     ]

