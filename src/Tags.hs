{-
    Модуль, отвечающий за работу с тематическими тегами и с именами авторов статей.
    https://github.com/denisshevchenko/ruhaskell
    Все права принадлежат русскоязычному сообществу Haskell-разработчиков, 2015 г.
-}

{-# LANGUAGE OverloadedStrings #-}

module Tags (
    buildPostsTags,
    buildPostsAuthors,
    buildPostsCategories,
    createPageWithAllTags,
    createPageWithAllAuthors,
    createPageWithAllCategories,
    convertTagsToLinks,
    convertCategoriesToLinks,
    convertAuthorsToLinks
) where

import Data.Monoid          (mconcat)
import Network.HTTP         (urlEncode)
import Context              (postContext)
import Misc                 (TagsReader, TagsAndAuthors, getNameOfAuthor)
import Control.Monad.Reader
import Hakyll

-- Функция извлекает из всех статей значения поля tags и собирает их в кучу.
-- Функция urlEncode необходима для корректного формирования неанглийских меток.
buildPostsTags :: MonadMetadata m => m Tags
buildPostsTags = buildTags "posts/**" $ fromCapture "tags/*.html" . urlEncode

-- Функция определяет категорию, к которой относится статья.
buildPostsCategories :: MonadMetadata m => m Tags
buildPostsCategories = buildCategories "posts/**" $ fromCapture "categories/*.html"

-- Функция извлекает из всех статей значения поля author и собирает их в кучу.
-- Функция urlEncode необходима для корректного формирования неанглийских имён авторов.
buildPostsAuthors :: MonadMetadata m => m Tags
buildPostsAuthors = buildTagsWith getNameOfAuthor "posts/**" $ fromCapture "authors/*.html" . urlEncode

-- Вспомогательная функция, формирующая страницу с облаком определённых тегов.
createPageWithTagsCloud :: Tags 
                        -> Identifier
                        -> Double
                        -> Double
                        -> String 
                        -> String
                        -> Identifier
                        -> Rules ()
createPageWithTagsCloud specificTags 
                        pageWithSpecificTags 
                        smallestFontSizeInPercent 
                        biggestFontSizeInPercent 
                        pageTitle 
                        cloudName 
                        specificTemplate = 
    create [pageWithSpecificTags] $ do
        route idRoute
        compile $ do
            let renderedCloud = \_ -> renderTagCloud smallestFontSizeInPercent 
                                                     biggestFontSizeInPercent
                                                     specificTags 
                tagsContext = mconcat [ constField "title" pageTitle 
                                      , field cloudName renderedCloud
                                      , defaultContext
                                      ]
            
            makeItem "" >>= loadAndApplyTemplate specificTemplate tagsContext
                        >>= loadAndApplyTemplate "templates/default.html" tagsContext
                        >>= relativizeUrls

-- Формируем страницу с облаком тематических тегов.
createPageWithAllTags :: TagsReader
createPageWithAllTags = do
    tagsAndAuthors <- ask
    lift $ createPageWithTagsCloud (tagsAndAuthors !! 0) 
                                   "tags.html" 
                                   110 
                                   220 
                                   "Темы публикаций"
                                   "tagsCloud"
                                   "templates/tags.html"
    return ()

-- Формируем страницу с облаком категорий.
createPageWithAllCategories :: TagsReader
createPageWithAllCategories = do
    tagsAndAuthors <- ask
    lift $ createPageWithTagsCloud (tagsAndAuthors !! 1) 
                                   "categories.html" 
                                   110 
                                   220 
                                   "Категории публикаций" 
                                   "categoriesCloud" 
                                   "templates/categories.html"
    return ()

-- Формируем страницу с облаком авторов публикаций.
createPageWithAllAuthors :: TagsReader
createPageWithAllAuthors = do
    tagsAndAuthors <- ask
    lift $ createPageWithTagsCloud (tagsAndAuthors !! 2) 
                                   "authors.html" 
                                   110 
                                   220 
                                   "Наши авторы" 
                                   "authorsCloud" 
                                   "templates/authors.html"
    return ()

convertSpecificTagsToLinks :: TagsAndAuthors 
                           -> Tags 
                           -> String 
                           -> Rules ()
convertSpecificTagsToLinks tagsAndAuthors specificTags aTitle = 
    tagsRules specificTags $ \tag pattern -> do
        let title = aTitle ++ " `" ++ tag ++ "`"
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let taggedPostsContext = mconcat [ listField "posts" (postContext tagsAndAuthors) (return posts)
                                             , constField "title" title
                                             , defaultContext
                                             ]

            makeItem "" >>= loadAndApplyTemplate "templates/posts.html" taggedPostsContext 
                        >>= loadAndApplyTemplate "templates/default.html" taggedPostsContext
                        >>= relativizeUrls

-- Делаем тематические теги ссылками, что позволит отфильтровать статьи по тегам.
convertTagsToLinks :: TagsReader
convertTagsToLinks = do
    tagsAndAuthors <- ask
    lift $ convertSpecificTagsToLinks tagsAndAuthors 
                                      (tagsAndAuthors !! 0)
                                      "Все статьи по теме"
    return ()

-- Делаем названия категорий ссылками, что позволит отфильтровать статьи по категориям.
convertCategoriesToLinks :: TagsReader
convertCategoriesToLinks = do
    tagsAndAuthors <- ask
    lift $ convertSpecificTagsToLinks tagsAndAuthors 
                                      (tagsAndAuthors !! 1)
                                      "Все статьи в категории"
    return ()

-- Делаем имена авторов ссылками, что позволит отфильтровать статьи по авторам.
convertAuthorsToLinks :: TagsReader
convertAuthorsToLinks = do
    tagsAndAuthors <- ask
    lift $ convertSpecificTagsToLinks tagsAndAuthors 
                                      (tagsAndAuthors !! 2)
                                      "Все статьи автора"
    return ()

