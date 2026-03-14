{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module BaphoNET.SourceReaders
    ( ingestSources
    , detectSourceType
    , readUrlHttpRaw
    ) where

import BaphoNET.BrowserFetcher (fetchUrlWithBrowser)
import BaphoNET.Domain
    ( AppConfig(..)
    , BrowserFetchResult(..)
    , ContentBlock(..)
    , FetchBackend(..)
    , ImageAsset(..)
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
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Network.HTTP.Simple (getResponseBody, httpLBS, parseRequest)
import System.Directory (doesFileExist)
import System.FilePath (takeExtension)

data RawSource = RawSource
    { rawContent :: T.Text
    , rawTitle :: Maybe T.Text
    , rawResolvedImages :: [(T.Text, Maybe T.Text)]
    , rawWarnings :: [T.Text]
    }

ingestSources :: AppConfig -> [SourceDescriptor] -> IO [NormalizedDocument]
ingestSources cfg = fmap concat . mapM (ingestSource cfg)

ingestSource :: AppConfig -> SourceDescriptor -> IO [NormalizedDocument]
ingestSource cfg src = do
    rawContentE <- readRawSource cfg src
    case rawContentE of
        Left warningText ->
            pure
                [ emptyDocument src SourceUnknown warningText
                ]
        Right rawSourceData -> do
            let sourceType = resolveSourceType src
            if sourceType `elem` allowedSourceTypes cfg
                then normalizeSource src sourceType rawSourceData
                else pure [emptyDocument src sourceType "Source type is not allowed"]

normalizeSource :: SourceDescriptor -> SourceType -> RawSource -> IO [NormalizedDocument]
normalizeSource src sourceType sourceData =
    case sourceType of
        SourceHtml -> normalizeHtmlDocument src sourceData
        SourceMarkdown -> pure [normalizeTextDocument src sourceType (rawContent sourceData) (rawWarnings sourceData)]
        SourceText -> pure [normalizeTextDocument src sourceType (rawContent sourceData) (rawWarnings sourceData)]
        SourceEpub -> pure [normalizeTextDocument src sourceType (rawContent sourceData) (rawWarnings sourceData)]
        SourceUnknown -> pure [normalizeTextDocument src sourceType (rawContent sourceData) (rawWarnings sourceData)]

normalizeHtmlDocument :: SourceDescriptor -> RawSource -> IO [NormalizedDocument]
normalizeHtmlDocument src sourceData = do
    let rawHtml = rawContent sourceData
        preparedHtml = preprocessHtml rawHtml
        imageAssets = take 8 (extractImageAssets src (rawResolvedImages sourceData) preparedHtml)
        htmlWithPlaceholders = injectImagePlaceholders imageAssets preparedHtml
        plainText = htmlToPlain htmlWithPlaceholders
        cleanedLines = cleanLines (T.lines plainText)
        titleText = sourceTitle src sourceData cleanedLines
        blocks = buildBlocks cleanedLines
        renderedText = blocksToText imageAssets blocks
    pure
        [ NormalizedDocument
            { normalizedSourceId = sourceId src
            , normalizedTitle = titleText
            , normalizedSourceType = SourceHtml
            , normalizedOriginalRef = sourceValue src
            , normalizedDomain = extractDomain src
            , normalizedPlainText = renderedText
            , normalizedBlocks = blocks
            , normalizedImages = imageAssets
            , normalizedWarnings = rawWarnings sourceData
            }
        ]

normalizeTextDocument :: SourceDescriptor -> SourceType -> T.Text -> [T.Text] -> NormalizedDocument
normalizeTextDocument src sourceType content warnings =
    let cleanedLines = cleanLines (T.lines content)
        titleText = sourceTitle src (RawSource content Nothing [] warnings) cleanedLines
        blocks = buildTextBlocks cleanedLines
    in NormalizedDocument
        { normalizedSourceId = sourceId src
        , normalizedTitle = titleText
        , normalizedSourceType = sourceType
        , normalizedOriginalRef = sourceValue src
        , normalizedDomain = extractDomain src
        , normalizedPlainText = blocksToText [] blocks
        , normalizedBlocks = blocks
        , normalizedImages = []
        , normalizedWarnings = warnings
        }

readRawSource :: AppConfig -> SourceDescriptor -> IO (Either T.Text RawSource)
readRawSource cfg src =
    case sourceKind src of
        SourceUrl ->
            case fetchBackend cfg of
                FetchSelenium -> do
                    browserE <- fetchUrlWithBrowser (seleniumConfig cfg) src
                    case browserE of
                        Right browserResult ->
                            pure
                                ( Right
                                    RawSource
                                        { rawContent = fetchedHtml browserResult
                                        , rawTitle = fetchedTitle browserResult
                                        , rawResolvedImages = fetchedResolvedImages browserResult
                                        , rawWarnings = fetchedWarnings browserResult
                                        }
                                )
                        Left browserErr -> do
                            httpE <- readUrlHttp src
                            pure $
                                case httpE of
                                    Left err -> Left ("selenium fetch failed: " <> browserErr <> "; http fallback failed: " <> err)
                                    Right sourceData ->
                                        Right sourceData
                                            { rawWarnings =
                                                ("selenium fetch failed: " <> browserErr) : rawWarnings sourceData
                                            }
                FetchHttp -> readUrlHttp src
        SourceFile -> readFileSource src

readUrlHttp :: SourceDescriptor -> IO (Either T.Text RawSource)
readUrlHttp src = do
    responseE <- readUrlHttpRaw (sourceValue src)
    pure $
        case responseE of
            Left err -> Left err
            Right responseBody ->
                Right
                    RawSource
                        { rawContent = TE.decodeUtf8 . BL.toStrict $ responseBody
                        , rawTitle = Nothing
                        , rawResolvedImages = []
                        , rawWarnings = []
                        }

readUrlHttpRaw :: T.Text -> IO (Either T.Text BL.ByteString)
readUrlHttpRaw urlText = do
    requestE <- try @SomeException $ parseRequest (T.unpack urlText)
    case requestE of
        Left ex -> pure (Left (T.pack (show ex)))
        Right request -> do
            responseE <- try @SomeException $ httpLBS request
            case responseE of
                Left ex -> pure (Left (T.pack (show ex)))
                Right response -> pure (Right (getResponseBody response))

readFileSource :: SourceDescriptor -> IO (Either T.Text RawSource)
readFileSource src = do
    let path = T.unpack (sourceValue src)
    exists <- doesFileExist path
    if exists
        then do
            content <- TIO.readFile path
            pure $
                Right
                    RawSource
                        { rawContent = content
                        , rawTitle = Nothing
                        , rawResolvedImages = []
                        , rawWarnings = []
                        }
        else pure (Left ("File not found: " <> sourceValue src))

preprocessHtml :: T.Text -> T.Text
preprocessHtml =
    limitHtmlSize
        . keepMainSection
        . stripTagSection "noscript"
        . stripTagSection "script"
        . stripTagSection "style"
        . stripTagSection "svg"
        . stripTagSection "nav"
        . stripTagSection "header"
        . stripTagSection "footer"

limitHtmlSize :: T.Text -> T.Text
limitHtmlSize = T.take 220000

keepMainSection :: T.Text -> T.Text
keepMainSection htmlText =
    case breakSection "<article" "</article>" htmlText of
        Just section -> section
        Nothing ->
            case breakSection "<main" "</main>" htmlText of
                Just section -> section
                Nothing ->
                    case breakSection "<body" "</body>" htmlText of
                        Just section -> section
                        Nothing -> htmlText

breakSection :: T.Text -> T.Text -> T.Text -> Maybe T.Text
breakSection openTag closeTag htmlText = do
    (_, afterOpen) <- nonEmptyBreak (T.breakOn openTag (T.toLower htmlText))
    let originalSuffix = T.drop (T.length htmlText - T.length afterOpen) htmlText
        afterOpenTag = T.dropWhile (/= '>') originalSuffix
        contentStart = T.drop 1 afterOpenTag
        lowerContent = T.toLower contentStart
        (sectionLower, _) = T.breakOn closeTag lowerContent
    if T.null sectionLower
        then Nothing
        else Just (T.take (T.length sectionLower) contentStart)

stripTagSection :: T.Text -> T.Text -> T.Text
stripTagSection tagName = go
  where
    openTag = "<" <> tagName
    closeTag = "</" <> tagName <> ">"

    go textValue =
        let lowered = T.toLower textValue
        in case T.breakOn openTag lowered of
            (prefixLower, suffixLower)
                | T.null suffixLower -> textValue
                | otherwise ->
                    let prefixLen = T.length prefixLower
                        suffix = T.drop prefixLen textValue
                        afterOpen = T.drop (T.length openTag) suffix
                        (_, afterCloseLower) = T.breakOn closeTag suffixLower
                    in if T.null afterCloseLower
                        then T.take prefixLen textValue
                        else
                            let afterClose = T.drop (T.length closeTag) (T.drop (T.length suffixLower - T.length afterCloseLower) afterOpen)
                            in T.take prefixLen textValue <> go afterClose

htmlToPlain :: T.Text -> T.Text
htmlToPlain =
    T.unlines
        . collapseBlankLines
        . map sanitizeLine
        . T.lines
        . decodeEntities
        . stripAllTags
        . markBlockBoundaries

markBlockBoundaries :: T.Text -> T.Text
markBlockBoundaries =
    replaceMany
        [ ("</p>", "\n\n")
        , ("<p", "\n\n<p")
        , ("</div>", "\n")
        , ("<div", "\n<div")
        , ("</section>", "\n\n")
        , ("<section", "\n<section")
        , ("</article>", "\n\n")
        , ("<article", "\n<article")
        , ("</main>", "\n\n")
        , ("<main", "\n<main")
        , ("<br>", "\n")
        , ("<br/>", "\n")
        , ("<br />", "\n")
        , ("</li>", "\n")
        , ("<li", "\n<li")
        , ("</ul>", "\n")
        , ("</ol>", "\n")
        , ("</h1>", "\n\n")
        , ("</h2>", "\n\n")
        , ("</h3>", "\n\n")
        , ("</h4>", "\n\n")
        , ("</h5>", "\n\n")
        , ("</h6>", "\n\n")
        , ("<h1", "\n\n<h1")
        , ("<h2", "\n\n<h2")
        , ("<h3", "\n\n<h3")
        , ("<h4", "\n\n<h4")
        , ("<h5", "\n\n<h5")
        , ("<h6", "\n\n<h6")
        ]
        . T.toLower

stripAllTags :: T.Text -> T.Text
stripAllTags textValue =
    case T.breakOn "<" textValue of
        (prefix, suffix)
            | T.null suffix -> prefix
            | otherwise ->
                let afterTag = T.drop 1 (T.dropWhile (/= '>') suffix)
                in prefix <> stripAllTags afterTag

decodeEntities :: T.Text -> T.Text
decodeEntities =
    replaceMany
        [ ("&nbsp;", " ")
        , ("&amp;", "&")
        , ("&quot;", "\"")
        , ("&#39;", "'")
        , ("&lt;", "<")
        , ("&gt;", ">")
        ]

replaceMany :: [(T.Text, T.Text)] -> T.Text -> T.Text
replaceMany replacements textValue =
    foldl (\acc (needle, replacement) -> T.replace needle replacement acc) textValue replacements

extractImageAssets :: SourceDescriptor -> [(T.Text, Maybe T.Text)] -> T.Text -> [ImageAsset]
extractImageAssets src resolvedImages rawHtml =
    go 1 rawHtml
  where
    resolvedMap = zip [1 :: Int ..] resolvedImages

    go _ remainder
        | T.null remainder = []
    go ordinal remainder =
        case T.breakOn "<img" (T.toLower remainder) of
            (_, suffixLower) | T.null suffixLower -> []
            (beforeChunkLower, suffixLower) ->
                let prefixLen = T.length beforeChunkLower
                    beforeChunk = T.take prefixLen remainder
                    suffix = T.drop prefixLen remainder
                    afterOpen = T.drop 4 suffix
                    (tagBody, rest) = T.breakOn ">" afterOpen
                    tagText = "<img" <> tagBody <> ">"
                    resolvedPair = lookup ordinal resolvedMap
                    assetRef = fromMaybe (fromMaybe "" (extractAttribute "src" tagText)) (fst <$> resolvedPair)
                    altText = (snd =<< resolvedPair) <|> extractAttribute "alt" tagText
                    following = T.drop 1 rest
                    beforeText = trimContext beforeChunk
                    afterText = trimContext following
                    asset =
                        ImageAsset
                            { assetId = sourceId src <> "-img-" <> T.pack (show ordinal)
                            , assetSourceId = sourceId src
                            , assetOriginalRef = assetRef
                            , assetOrdinal = ordinal
                            , assetSurroundingTextBefore = beforeText
                            , assetSurroundingTextAfter = afterText
                            , assetAltText = altText
                            , assetCaption = Nothing
                            , assetCaptionConfidence = Nothing
                            , assetCaptionSource = Nothing
                            , assetCaptionRejectedReason = Nothing
                            , assetFetched = False
                            , assetLocalPath = Nothing
                            , assetWarnings = []
                            }
                in if isDecorativeAsset asset
                    then go ordinal following
                    else asset : go (ordinal + 1) following

injectImagePlaceholders :: [ImageAsset] -> T.Text -> T.Text
injectImagePlaceholders assets = go assets
  where
    go [] htmlText = htmlText
    go (asset:rest) htmlText =
        case T.breakOn "<img" (T.toLower htmlText) of
            (_, suffixLower) | T.null suffixLower -> htmlText
            (prefixLower, suffixLower) ->
                let prefixLen = T.length prefixLower
                    prefix = T.take prefixLen htmlText
                    suffix = T.drop prefixLen htmlText
                    afterOpen = T.drop 4 suffix
                    (_, restTextRaw) = T.breakOn ">" afterOpen
                    restText = T.drop 1 restTextRaw
                    placeholder = "<p>BAPHONET_IMAGE " <> assetId asset <> "</p>"
                in prefix <> placeholder <> go rest restText

buildBlocks :: [T.Text] -> [ContentBlock]
buildBlocks =
    map toBlock
  where
    toBlock line
        | "BAPHONET_IMAGE " `T.isPrefixOf` line =
            BlockImageRef (T.strip (T.drop 15 line))
        | "baphonet_image " `T.isPrefixOf` line =
            BlockImageRef (T.strip (T.drop 15 line))
        | looksLikeHeading line = BlockHeading line
        | otherwise = BlockParagraph line

buildTextBlocks :: [T.Text] -> [ContentBlock]
buildTextBlocks =
    map
        (\line ->
            if looksLikeHeading line
                then BlockHeading line
                else BlockParagraph line
        )

blocksToText :: [ImageAsset] -> [ContentBlock] -> T.Text
blocksToText assets blocks =
    T.unlines (concatMap render blocks)
  where
    render block =
        case block of
            BlockHeading txt -> [txt]
            BlockParagraph txt -> [txt]
            BlockImageRef imageId ->
                case lookupImage imageId assets of
                    Nothing -> []
                    Just asset ->
                        [ "Иллюстрация: " <> maybe "Изображение без описания." id (assetCaption asset <|> assetAltText asset)
                        ]

lookupImage :: T.Text -> [ImageAsset] -> Maybe ImageAsset
lookupImage imageId = foldr (\asset acc -> if assetId asset == imageId then Just asset else acc) Nothing

cleanLines :: [T.Text] -> [T.Text]
cleanLines =
    collapseBlankLines
        . filter keepLine
        . map sanitizeLine
  where
    keepLine line =
        not (T.null line)
            && not (isBoilerplate line)
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
        , "sprite"
        ]
  where
    lowerLine = T.toLower line

sanitizeLine :: T.Text -> T.Text
sanitizeLine =
    normalizeWhitespace
        . stripHtmlTags
        . stripMarkdownImage
        . T.strip

normalizeWhitespace :: T.Text -> T.Text
normalizeWhitespace = T.unwords . T.words

collapseBlankLines :: [T.Text] -> [T.Text]
collapseBlankLines = reverse . foldl step []
  where
    step [] line = [line]
    step acc@(previous:_) line
        | T.null line && T.null previous = acc
        | otherwise = line : acc

sourceTitle :: SourceDescriptor -> RawSource -> [T.Text] -> T.Text
sourceTitle src sourceData cleanedLines =
    case sourceLabel src of
        Just label -> label
        Nothing ->
            fromMaybe (sourceId src) (rawTitle sourceData <|> htmlTitle <|> lineTitle)
  where
    htmlTitle =
        case resolveSourceType src of
            SourceHtml -> sanitizeTitle <$> extractHtmlTitle (rawContent sourceData)
            _ -> Nothing
    lineTitle = sanitizeTitle <$> listToMaybeNonEmpty cleanedLines

extractHtmlTitle :: T.Text -> Maybe T.Text
extractHtmlTitle rawHtml =
    firstNonEmpty
        [ extractMetaContent "property=\"og:title\"" rawHtml
        , extractMetaContent "name=\"twitter:title\"" rawHtml
        , extractTagContent "title" rawHtml
        ]

extractMetaContent :: T.Text -> T.Text -> Maybe T.Text
extractMetaContent marker rawHtml = do
    (_, suffix) <- nonEmptyBreak (T.breakOn marker rawHtml)
    extractAttribute "content" suffix

extractTagContent :: T.Text -> T.Text -> Maybe T.Text
extractTagContent tagName rawHtml = do
    (_, suffix) <- nonEmptyBreak (T.breakOn ("<" <> tagName) (T.toLower rawHtml))
    let originalSuffix = T.drop (T.length rawHtml - T.length suffix) rawHtml
        afterTag = T.dropWhile (/= '>') originalSuffix
        innerText = T.takeWhile (/= '<') (T.drop 1 afterTag)
    nonEmptyText innerText

extractAttribute :: T.Text -> T.Text -> Maybe T.Text
extractAttribute attrName textValue = do
    (_, suffix) <- nonEmptyBreak (T.breakOn (attrName <> "=\"") (T.toLower textValue))
    let originalSuffix = T.drop (T.length textValue - T.length suffix) textValue
        valueStart = T.drop (T.length attrName + 2) originalSuffix
    nonEmptyText (T.takeWhile (/= '"') valueStart)

trimContext :: T.Text -> T.Text
trimContext =
    T.unwords . take 24 . reverse . take 24 . reverse . T.words . sanitizeLine

looksLikeHeading :: T.Text -> Bool
looksLikeHeading line =
    T.length line < 120
        && length (T.words line) <= 12
        && T.length line > 5

isDecorativeAsset :: ImageAsset -> Bool
isDecorativeAsset asset =
    T.null (assetOriginalRef asset)
        || "data:image" `T.isPrefixOf` T.toLower (assetOriginalRef asset)
        || ".svg" `T.isSuffixOf` T.toLower (assetOriginalRef asset)
        || "logo" `T.isInfixOf` T.toLower (assetOriginalRef asset)
        || "icon" `T.isInfixOf` T.toLower (assetOriginalRef asset)
        || "sprite" `T.isInfixOf` T.toLower (assetOriginalRef asset)
        || "share" `T.isInfixOf` T.toLower (assetOriginalRef asset)
        || "avatar" `T.isInfixOf` T.toLower (assetOriginalRef asset)

stripMarkdownImage :: T.Text -> T.Text
stripMarkdownImage textValue
    | "data:image" `T.isInfixOf` textValue =
        T.strip (snd (T.breakOnEnd ")" textValue))
    | otherwise = textValue

stripHtmlTags :: T.Text -> T.Text
stripHtmlTags = go False
  where
    go _ "" = ""
    go True txt =
        case T.uncons txt of
            Nothing -> ""
            Just ('>', rest) -> go False rest
            Just (_, rest) -> go True rest
    go False txt =
        case T.uncons txt of
            Nothing -> ""
            Just ('<', rest) -> go True rest
            Just (charValue, rest) -> T.cons charValue (go False rest)

sanitizeTitle :: T.Text -> T.Text
sanitizeTitle =
    T.take 180
        . normalizeWhitespace
        . stripHtmlTags
        . stripMarkdownImage
        . T.replace "&#x2F;" "/"
        . T.replace "&quot;" "\""
        . T.replace "&amp;" "&"
        . T.replace "&nbsp;" " "

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
                        _ -> SourceUnknown

extractDomain :: SourceDescriptor -> Maybe T.Text
extractDomain src =
    case sourceKind src of
        SourceFile -> Nothing
        SourceUrl ->
            let withoutScheme = snd (T.breakOn "//" (sourceValue src))
                trimmed = T.drop 2 withoutScheme
            in if T.null trimmed then Nothing else Just (T.takeWhile (/= '/') trimmed)

emptyDocument :: SourceDescriptor -> SourceType -> T.Text -> NormalizedDocument
emptyDocument src sourceType warningText =
    NormalizedDocument
        { normalizedSourceId = sourceId src
        , normalizedTitle = sourceId src
        , normalizedSourceType = sourceType
        , normalizedOriginalRef = sourceValue src
        , normalizedDomain = extractDomain src
        , normalizedPlainText = warningText
        , normalizedBlocks = [BlockParagraph warningText]
        , normalizedImages = []
        , normalizedWarnings = [warningText]
        }

firstNonEmpty :: [Maybe T.Text] -> Maybe T.Text
firstNonEmpty = listToMaybeNonEmpty . foldr collect []
  where
    collect (Just txt) acc
        | T.null (T.strip txt) = acc
        | otherwise = T.strip txt : acc
    collect Nothing acc = acc

nonEmptyBreak :: (T.Text, T.Text) -> Maybe (T.Text, T.Text)
nonEmptyBreak pair@(_, suffix)
    | T.null suffix = Nothing
    | otherwise = Just pair

nonEmptyText :: T.Text -> Maybe T.Text
nonEmptyText txt =
    let trimmed = T.strip txt
    in if T.null trimmed then Nothing else Just trimmed

listToMaybeNonEmpty :: [T.Text] -> Maybe T.Text
listToMaybeNonEmpty [] = Nothing
listToMaybeNonEmpty (x:_) = Just x
