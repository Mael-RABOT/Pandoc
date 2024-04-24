{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-LYN-4-1-mypandoc-mael.rabot
-- File description:
-- PrintUniversalContent.hs
-}

module PrintUniversalContent where

import Text.Printf (printf)

import Types (Optional, Header(..), Item(..), Paragraph(..), UniversalContent(..), Section(..), Links(..), Text(..))

printHeader :: Header -> IO ()
printHeader (Header title author date) =
    putStrLn ("Title: " ++ title) >>
    putStrLn ("Author: " ++ maybe "N/A" id author) >>
    putStrLn ("Date: " ++ maybe "N/A" id date)

printLinks :: Links -> IO ()
printLinks (Link url content) = printf "Link: %s, Content: %s\n" url content
printLinks (Image url alt) = printf "Image: %s, Alt: %s\n" url alt

printParagraph :: Paragraph -> IO ()
printParagraph (Text str) = putStrLn $ show str
printParagraph (Content items) = mapM_ printItem items

printSection :: Section -> IO ()
printSection (Section name content) =
    putStrLn ("Section: " ++ maybe "N/A" id name) >>
    mapM_ printItem content

printItem :: Item -> IO ()
printItem (ParagraphItem paragraph) = printParagraph paragraph
printItem (ListItem items) = mapM_ printItem items
printItem (SectionItem section) = printSection section
printItem (CodeBlockItem str) = putStrLn str
printItem (LinksItem links) = printLinks links

printUniversalContent :: UniversalContent -> IO ()
printUniversalContent (UniversalContent header body) =
    putStrLn "Header:" >>
    printHeader header >>
    putStrLn "Body:" >>
    mapM_ printItem body
