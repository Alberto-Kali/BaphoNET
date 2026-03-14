{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module BaphoNET.SourceReaders
    ( ingestSources
    , detectSourceType
    ) where

import BaphoNET.Domain
    ( AppConfig(..)
    , ContentBlock(..)
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
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Network.HTTP.Simple (getResponseBody, httpLBS, parseRequest)
import System.Directory (doesFileExist)
import System.FilePath (takeExtension)

ingestSources :: AppConfig -> [SourceDescriptor] -> IO [NormalizedDocument]
ingestSources cfg = fmap concat . mapM (ingestSource cfg)

ingestSource :: AppConfig -> SourceDescriptor -> IO [NormalizedDocument]
ingestSource cfg src = do
    rawContent <- readRawSource src
    case rawContent of
        Left warningText ->
            pure
                [ emptyDocument src SourceUnknown warningText
                ]
        Right content -> do
            let sourceType = resolveSourceType src
            if sourceType `elem` allowedSourceTypes cfg
                then normalizeSource src sourceType content
                else pure [emptyDocument src sourceType "Source type is not allowed"]

normalizeSource :: SourceDescriptor -> SourceType -> T.Text -> IO [NormalizedDocument]
normalizeSource src sourceType content =
    case sourceType of
        SourceHtml -> normalizeHtmlDocument src content
        SourceMarkdown -> pure [normalizeTextDocument src sourceType content]
        SourceText -> pure [normalizeTextDocument src sourceType content]
        SourceEpub -> pure [normalizeTextDocument src sourceType content]
        SourceUnknown -> pure [normalizeTextDocument src sourceType content]

normalizeHtmlDocument :: SourceDescriptor -> T.Text -> IO [NormalizedDocument]
normalizeHtmlDocument src rawHtml = do
    let preparedHtml = preprocessHtml rawHtml
        imageAssets = take 6 (extractImageAssets src preparedHtml)
        htmlWithPlaceholders = injectImagePlaceholders imageAssets preparedHtml
        plainText = htmlToPlain htmlWithPlaceholders
        cleanedLines = cleanLines (T.lines plainText)
        titleText = sourceTitle src rawHtml cleanedLines
        blocks = buildBlocks cleanedLines imageAssets
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
            , normalizedWarnings = []
            }
        ]

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
limitHtmlSize = T.take 180000

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
    (_, afterOpen) <- nonEmptyBreak (T.breakOn openTag htmlText)
    let afterOpenTag = T.dropWhile (/= '>') afterOpen
        contentStart = T.drop 1 afterOpenTag
        (section, _) = T.breakOn closeTag contentStart
    if T.null section then Nothing else Just section

stripTagSection :: T.Text -> T.Text -> T.Text
stripTagSection tagName = go
  where
    openTag = "<" <> tagName
    closeTag = "</" <> tagName <> ">"

    go textValue =
        case T.breakOn openTag textValue of
            (prefix, suffix)
                | T.null suffix -> prefix
                | otherwise ->
                    let afterOpen = T.drop (T.length openTag) suffix
                        (_, afterClose) = T.breakOn closeTag afterOpen
                    in if T.null afterClose
                        then prefix
                        else prefix <> go (T.drop (T.length closeTag) afterClose)

normalizeTextDocument :: SourceDescriptor -> SourceType -> T.Text -> NormalizedDocument
normalizeTextDocument src sourceType content =
    let cleanedLines = cleanLines (T.lines content)
        titleText = sourceTitle src content cleanedLines
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
        , normalizedWarnings = []
        }

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

extractImageAssets :: SourceDescriptor -> T.Text -> [ImageAsset]
extractImageAssets src rawHtml =
    go 1 rawHtml
  where
    go _ remainder
        | T.null remainder = []
    go ordinal remainder =
        case T.breakOn "<img" remainder of
            (_, suffix) | T.null suffix -> []
            (beforeChunk, suffix) ->
                let afterOpen = T.drop 4 suffix
                    (tagBody, rest) = T.breakOn ">" afterOpen
                    tagText = "<img" <> tagBody <> ">"
                    assetRef = fromMaybe "" (extractAttribute "src" tagText)
                    altText = extractAttribute "alt" tagText
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
        case T.breakOn "<img" htmlText of
            (_, suffix) | T.null suffix -> htmlText
            (prefix, suffix) ->
                let afterOpen = T.drop 4 suffix
                    (_, restTextRaw) = T.breakOn ">" afterOpen
                    restText = T.drop 1 restTextRaw
                    placeholder = "<p>BAPHONET_IMAGE " <> assetId asset <> "</p>"
                in prefix <> placeholder <> go rest restText

buildBlocks :: [T.Text] -> [ImageAsset] -> [ContentBlock]
buildBlocks linesList _ =
    map toBlock linesList
  where
    toBlock line
        | "BAPHONET_IMAGE " `T.isPrefixOf` line =
            BlockImageRef (T.strip (T.drop (T.length ("BAPHONET_IMAGE " :: T.Text)) line))
        | "baphonet_image " `T.isPrefixOf` line =
            BlockImageRef (T.strip (T.drop (T.length ("baphonet_image " :: T.Text)) line))
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
    assetMap = foldl (\acc asset -> (assetId asset, asset) : acc) [] assets
    render block =
        case block of
            BlockHeading txt -> [txt]
            BlockParagraph txt -> [txt]
            BlockImageRef imageId ->
                case lookup imageId assetMap of
                    Nothing -> []
                    Just asset ->
                        [ "Иллюстрация: " <> maybe "Изображение без описания." id (assetCaption asset <|> assetAltText asset)
                        ]

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
    step acc line
        | T.null line && not (null acc) && T.null (head acc) = acc
        | otherwise = line : acc

sourceTitle :: SourceDescriptor -> T.Text -> [T.Text] -> T.Text
sourceTitle src rawContent cleanedLines =
    case sourceLabel src of
        Just label -> label
        Nothing -> fromMaybe (sourceId src) (htmlTitle <|> lineTitle)
  where
    htmlTitle =
        case resolveSourceType src of
            SourceHtml -> sanitizeTitle <$> extractHtmlTitle rawContent
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
    (_, suffix) <- nonEmptyBreak (T.breakOn ("<" <> tagName) rawHtml)
    let afterTag = T.dropWhile (/= '>') suffix
        innerText = T.takeWhile (/= '<') (T.drop 1 afterTag)
    nonEmptyText innerText

extractAttribute :: T.Text -> T.Text -> Maybe T.Text
extractAttribute attrName textValue = do
    (_, suffix) <- nonEmptyBreak (T.breakOn (attrName <> "=\"") textValue)
    let valueStart = T.drop (T.length attrName + 2) suffix
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
        || "data:image" `T.isPrefixOf` assetOriginalRef asset
        || ".svg" `T.isSuffixOf` T.toLower (assetOriginalRef asset)
        || "logo" `T.isInfixOf` T.toLower (assetOriginalRef asset)
        || "icon" `T.isInfixOf` T.toLower (assetOriginalRef asset)
        || "sprite" `T.isInfixOf` T.toLower (assetOriginalRef asset)
        || "share" `T.isInfixOf` T.toLower (assetOriginalRef asset)

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
            let raw = T.dropWhile (/= '/') (T.dropWhile (/= ':') (sourceValue src))
                trimmed = T.dropWhile (== '/') raw
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
