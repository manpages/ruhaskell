{-
    Модуль, отвечающий за формирование страницы со ссылками на сторонние ресурсы. 
    https://github.com/denisshevchenko/ruhaskell
    Все права принадлежат русскоязычному сообществу Haskell-разработчиков, 2015 г.
-}

{-# LANGUAGE OverloadedStrings #-}

module Links (
    createPageWithExternalLinks
) where

import Data.Monoid          (mconcat)
import Context              (postContext)
import Misc                 (TagsReader)
import Control.Monad.Reader
import Hakyll

-- Формируем страницу со ссылками 
createPageWithExternalLinks :: TagsReader
createPageWithExternalLinks = do
    tagsAndAuthors <- ask
    lift $ create ["links.html"] $ do
        route idRoute
        compile $ do
            let linksContext = mconcat [ postContext tagsAndAuthors
                                       , constField "title" "Ссылки"                   
                                       , defaultContext
                                       ]

            makeItem "" >>= loadAndApplyTemplate "templates/links.html" linksContext
                        >>= loadAndApplyTemplate "templates/default.html" linksContext
                        >>= relativizeUrls
    return ()

