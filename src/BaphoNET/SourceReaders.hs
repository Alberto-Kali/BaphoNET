{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module BaphoNET.SourceReaders
    ( ingestSources
    , detectSourceType
    ) where

import BaphoNET.Domain
    ( AppConfig(..)
    , NormalizedDocument(..)
    , SourceDescriptor(..)
    , SourceKind(..)
    , SourceType(..)
    )

import Control.Applicative ((<|>))
import Control.Exception (SomeException, try)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Char as Char
import Data.Maybe (fromMaybe)
import Data.List (isInfixOf)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Network.HTTP.Simple (getResponseBody, httpLBS, parseRequest)
import System.Directory (doesFileExist)
import System.FilePath (takeExtension)
import qualified Text.Pandoc as Pandoc

ingestSources :: AppConfig -> [SourceDescriptor] -> IO [NormalizedDocument]
ingestSources cfg = fmap concat . mapM (ingestSource cfg)

ingestSource :: AppConfig -> SourceDescriptor -> IO [NormalizedDocument]
ingestSource cfg src = do
    rawContent <- readRawSource src
    case rawContent of
        Left warningText ->
            pure
                [ (emptyDocument src SourceUnknown warningText)
                    { normalizedWarnings = [warningText]
                    }
                ]
        Right content -> do
            let sourceType = resolveSourceType src
            if sourceType `elem` allowedSourceTypes cfg
                then do
                    markdownE <- toMarkdown sourceType content
                    case markdownE of
                        Left err ->
                            pure
                                [ (emptyDocument src sourceType "Failed to normalize source")
                                    { normalizedWarnings = [T.pack err]
                                    }
                                ]
                        Right markdown ->
                            let cleaned = cleanMarkdown markdown
                            in pure
                                [ NormalizedDocument
                                    { normalizedSourceId = sourceId src
                                    , normalizedTitle = sourceTitle src content cleaned
                                    , normalizedSourceType = sourceType
                                    , normalizedOriginalRef = sourceValue src
                                    , normalizedDomain = extractDomain src
                                    , normalizedMarkdown = cleaned
                                    , normalizedPlainText = markdownToPlain cleaned
                                    , normalizedAssets = extractAssetRefs content
                                    , normalizedWarnings = []
                                    }
                                ]
                else pure [emptyDocument src sourceType "Source type is not allowed"]

readRawSource :: SourceDescriptor -> IO (Either T.Text T.Text)
readRawSource src =
    case sourceKind src of
        SourceUrl -> do
            requestE <- try @SomeException $ parseRequest (T.unpack (sourceValue src))
            case requestE of
                Left ex -> pure (Left (T.pack (show ex)))
                Right request -> do
                    responseE <- try @SomeException $ httpLBS request
                    case responseE of
                        Left ex -> pure (Left (T.pack (show ex)))
                        Right response ->
                            pure . Right . TE.decodeUtf8 . BL.toStrict $ getResponseBody response
        SourceFile -> do
            let path = T.unpack (sourceValue src)
            exists <- doesFileExist path
            if exists
                then Right <$> TIO.readFile path
                else pure (Left ("File not found: " <> sourceValue src))

toMarkdown :: SourceType -> T.Text -> IO (Either String T.Text)
toMarkdown sourceType content =
    case sourceType of
        SourceHtml -> pandocConvert (Pandoc.readHtml Pandoc.def content)
        SourceMarkdown -> pure (Right content)
        SourceText -> pure (Right content)
        SourceEpub -> pure (Right content)
        SourceUnknown -> pure (Right content)

pandocConvert :: Pandoc.PandocIO Pandoc.Pandoc -> IO (Either String T.Text)
pandocConvert reader = do
    result <- Pandoc.runIO $ do
        doc <- reader
        Pandoc.writeMarkdown Pandoc.def doc
    pure (either (Left . show) Right result)

cleanMarkdown :: T.Text -> T.Text
cleanMarkdown =
    T.unlines
        . collapseBlankLines
        . filter keepLine
        . map sanitizeLine
        . T.lines
  where
    keepLine line =
        not (T.null line)
            && not (isBoilerplate line)
            && T.length line > 2
            && not (isNoiseLine line)

isBoilerplate :: T.Text -> Bool
isBoilerplate line =
    any (`T.isInfixOf` lowerLine)
        [ "cookie"
        , "subscribe"
        , "advert"
        , "navigation"
        , "all rights reserved"
        , "sign in"
        ]
  where
    lowerLine = T.toLower line

isNoiseLine :: T.Text -> Bool
isNoiseLine line =
    any (`T.isInfixOf` lowerLine)
        [ "data:image/"
        , "svg+xml;base64"
        , "tm-svg-img"
        , "habr-logo"
        , "__next_data__"
        , "javascript:"
        , "menu"
        , "navigation"
        , "share this"
        , "читать далее"
        ]
        || "![[" `T.isInfixOf` lowerLine
  where
    lowerLine = T.toLower line

sanitizeLine :: T.Text -> T.Text
sanitizeLine =
    stripMarkdownImage
        . stripHtmlTags
        . normalizeWhitespace

normalizeWhitespace :: T.Text -> T.Text
normalizeWhitespace =
    T.unwords . T.words

collapseBlankLines :: [T.Text] -> [T.Text]
collapseBlankLines = reverse . foldl step []
  where
    step acc line
        | T.null line
            , T.null (headOrEmpty acc) = acc
        | otherwise = line : acc

    headOrEmpty [] = ""
    headOrEmpty (x:_) = x

markdownToPlain :: T.Text -> T.Text
markdownToPlain =
    T.unwords
        . concatMap (T.words . stripMarkup . sanitizeLine)
        . filter (not . isNoiseLine)
        . T.lines
  where
    stripMarkup = T.filter (\c -> not (c `elem` ("#*_`[]()" :: String)))

extractAssetRefs :: T.Text -> [T.Text]
extractAssetRefs content =
    filter (\ref -> any (\suffix -> suffix `T.isSuffixOf` T.toLower ref) [".png", ".jpg", ".jpeg", ".gif", ".webp"])
        . map T.strip
        . filter (\line -> "src=" `T.isInfixOf` line || "![" `T.isInfixOf` line)
        $ T.lines content

detectSourceType :: SourceDescriptor -> SourceType
detectSourceType = resolveSourceType

resolveSourceType :: SourceDescriptor -> SourceType
resolveSourceType src =
    case sourceTypeHint src of
        Just hinted -> hinted
        Nothing ->
            case sourceKind src of
                SourceUrl ->
                    case map Char.toLower (takeExtension (T.unpack (sourceValue src))) of
                        ".md" -> SourceMarkdown
                        ".markdown" -> SourceMarkdown
                        ".txt" -> SourceText
                        ".epub" -> SourceEpub
                        _ -> SourceHtml
                SourceFile ->
                    case map Char.toLower (takeExtension (T.unpack (sourceValue src))) of
                        ".html" -> SourceHtml
                        ".htm" -> SourceHtml
                        ".md" -> SourceMarkdown
                        ".markdown" -> SourceMarkdown
                        ".txt" -> SourceText
                        ".epub" -> SourceEpub
                        _ | "html" `isInfixOf` map Char.toLower (T.unpack (sourceValue src)) -> SourceHtml
                        _ -> SourceUnknown

extractDomain :: SourceDescriptor -> Maybe T.Text
extractDomain src =
    case sourceKind src of
        SourceFile -> Nothing
        SourceUrl ->
            let raw = T.dropWhile (/= '/') (T.dropWhile (/= ':') (sourceValue src))
                trimmed = T.dropWhile (== '/') raw
            in if T.null trimmed then Nothing else Just (T.takeWhile (/= '/') trimmed)

sourceTitle :: SourceDescriptor -> T.Text -> T.Text -> T.Text
sourceTitle src rawContent cleaned =
    case sourceLabel src of
        Just label -> label
        Nothing ->
            fromMaybe fallbackTitle (htmlDerivedTitle <|> cleanedDerivedTitle)
  where
    htmlDerivedTitle =
        case resolveSourceType src of
            SourceHtml -> sanitizeTitle <$> extractHtmlTitle rawContent
            _ -> Nothing
    cleanedDerivedTitle =
        sanitizeTitle <$> listToMaybeNonEmpty (filter (not . isNoiseLine) (T.lines cleaned))
    fallbackTitle = sourceId src

extractHtmlTitle :: T.Text -> Maybe T.Text
extractHtmlTitle rawHtml =
    firstNonEmpty
        [ extractMetaContent "property=\"og:title\"" rawHtml
        , extractMetaContent "name=\"twitter:title\"" rawHtml
        , extractTagContent "title" rawHtml
        ]

extractMetaContent :: T.Text -> T.Text -> Maybe T.Text
extractMetaContent marker rawHtml = do
    section <- extractAfter marker rawHtml
    extractAttribute "content=\"" section

extractTagContent :: T.Text -> T.Text -> Maybe T.Text
extractTagContent tagName rawHtml = do
    (_, afterOpen) <- nonEmptyBreak (T.breakOn ("<" <> tagName) rawHtml)
    let afterTag = T.dropWhile (/= '>') afterOpen
    if T.null afterTag
        then Nothing
        else
            let contentStart = T.drop 1 afterTag
                (contentValue, _) = T.breakOn ("</" <> tagName <> ">") contentStart
            in nonEmptyText (normalizeWhitespace contentValue)

extractAfter :: T.Text -> T.Text -> Maybe T.Text
extractAfter marker rawHtml =
    let (_, suffix) = T.breakOn marker rawHtml
    in if T.null suffix then Nothing else Just suffix

extractAttribute :: T.Text -> T.Text -> Maybe T.Text
extractAttribute attributePrefix section = do
    (_, afterAttr) <- nonEmptyBreak (T.breakOn attributePrefix section)
    let valueStart = T.drop (T.length attributePrefix) afterAttr
        attributeValue = T.takeWhile (/= '"') valueStart
    nonEmptyText attributeValue

sanitizeTitle :: T.Text -> T.Text
sanitizeTitle =
    T.take 180
        . T.dropWhile (\c -> c == '#' || c == '-' || Char.isSpace c)
        . normalizeWhitespace
        . stripMarkdownImage
        . stripHtmlTags
        . T.replace "&#x2F;" "/"
        . T.replace "&quot;" "\""
        . T.replace "&amp;" "&"
        . T.replace "&nbsp;" " "

stripMarkdownImage :: T.Text -> T.Text
stripMarkdownImage textValue
    | "data:image" `T.isInfixOf` textValue || (T.isPrefixOf "!" (T.strip textValue) && "](" `T.isInfixOf` textValue) =
        let (_, afterImage) = T.breakOn ")" textValue
        in if T.null afterImage then textValue else T.drop 1 afterImage
    | otherwise = textValue

stripHtmlTags :: T.Text -> T.Text
stripHtmlTags = go False
  where
    go _ "" = ""
    go True textValue =
        case T.uncons textValue of
            Nothing -> ""
            Just ('>', rest) -> go False rest
            Just (_, rest) -> go True rest
    go False textValue =
        case T.uncons textValue of
            Nothing -> ""
            Just ('<', rest) -> go True rest
            Just (charValue, rest) -> T.cons charValue (go False rest)

firstNonEmpty :: [Maybe T.Text] -> Maybe T.Text
firstNonEmpty = listToMaybeNonEmpty . mapMaybe nonEmptyTextMaybe
  where
    mapMaybe _ [] = []
    mapMaybe f (x:xs) =
        case f x of
            Just value -> value : mapMaybe f xs
            Nothing -> mapMaybe f xs

    nonEmptyTextMaybe Nothing = Nothing
    nonEmptyTextMaybe (Just value) = nonEmptyText value

nonEmptyBreak :: (T.Text, T.Text) -> Maybe (T.Text, T.Text)
nonEmptyBreak pair@(_, suffix)
    | T.null suffix = Nothing
    | otherwise = Just pair

nonEmptyText :: T.Text -> Maybe T.Text
nonEmptyText textValue
    | T.null trimmed = Nothing
    | isNoiseLine trimmed = Nothing
    | otherwise = Just trimmed
  where
    trimmed = T.strip textValue

emptyDocument :: SourceDescriptor -> SourceType -> T.Text -> NormalizedDocument
emptyDocument src sourceType warningText =
    NormalizedDocument
        { normalizedSourceId = sourceId src
        , normalizedTitle = sourceId src
        , normalizedSourceType = sourceType
        , normalizedOriginalRef = sourceValue src
        , normalizedDomain = extractDomain src
        , normalizedMarkdown = warningText
        , normalizedPlainText = warningText
        , normalizedAssets = []
        , normalizedWarnings = [warningText]
        }

listToMaybeNonEmpty :: [T.Text] -> Maybe T.Text
listToMaybeNonEmpty [] = Nothing
listToMaybeNonEmpty (x:_) = Just x
