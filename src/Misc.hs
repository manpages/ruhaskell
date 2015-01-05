{-
    Вспомогательный модуль.
    https://github.com/denisshevchenko/ruhaskell
    Все права принадлежат русскоязычному сообществу Haskell-разработчиков, 2015 г.
-}

{-# LANGUAGE OverloadedStrings #-}

module Misc (
    aHost,
    prepareAllTemplates,
    getNameOfAuthor,
    TagsAndAuthors,
    TagsReader
) where

import Control.Monad.Reader
import qualified Data.Map as M
import Hakyll

-- Данный URL останется актуальным до тех пор, пока сайт будет жить на GitHub Pages.
aHost :: String
aHost = "http://denisshevchenko.github.io/ruhaskell/"

-- Готовим все шаблоны из каталога templates.
prepareAllTemplates :: Rules ()
prepareAllTemplates = match "templates/*" $ compile templateCompiler

-- Читательское "облако" с тематическими тегами и с именами авторов статей.
type TagsAndAuthors = (Tags, Tags)
type TagsReader = ReaderT TagsAndAuthors Rules ()

-- Извлекает из статьи значение атрибута `author`.
getNameOfAuthor :: MonadMetadata m => Identifier -> m [String]
getNameOfAuthor identifier = do
    -- Собираем атрибуты статьи в обычный ассоциативный контейнер.
    metadata <- getMetadata identifier
    let maybeAuthor = M.lookup "author" metadata
    return $ case maybeAuthor of
        -- Поразумевается, что у статьи всегда один автор, а не несколько.
        Nothing -> ["Не указан"]
        Just nameOfAuthor -> [trim nameOfAuthor]

