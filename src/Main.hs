{-
    Главный модуль.
    https://github.com/denisshevchenko/ruhaskell
    Все права принадлежат русскоязычному сообществу Haskell-разработчиков, 2015 г.
-}

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Copiers              (justCopy, justCreateAndCopy, justCompressAndCopy)
import RSSFeed              (setupRSSFeed)
import Posts                (createPosts)
import Tags                 (createPageWithAllTags, 
                             convertTagsToLinks,
                             convertAuthorsToLinks,
                             buildPostsTags,
                             buildPostsAuthors,
                             createPageWithAllAuthors)
import XMLMap               (createXMLMap)
import Archive              (createPageWithAllPosts)
import Misc                 (prepareAllTemplates)
import IndexPage            (createIndexPage)
import Control.Monad.Reader (runReaderT)
import Hakyll

main :: IO ()
main = hakyll $ do
    justCopy            "static/images/*"
    justCompressAndCopy "static/css/*"
    justCopy            "README.md"
    justCopy            "CNAME"
    justCreateAndCopy   ".nojekyll"
    
    prepareAllTemplates
    
    -- Извлекаем тематические теги, а также имена авторов из всех публикаций.
    tags <- buildPostsTags
    authors <- buildPostsAuthors

    -- Теги и имена авторов нужны всем, поэтому для удобства запускаем читателя.
    runReaderT (createPosts
                >> createPageWithAllPosts
                >> createPageWithAllTags
                >> createPageWithAllAuthors
                >> convertTagsToLinks
                >> convertAuthorsToLinks
                >> createXMLMap
                >> setupRSSFeed
                >> createIndexPage) (tags, authors)

