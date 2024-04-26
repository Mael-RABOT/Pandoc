{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-LYN-4-1-mypandoc-mael.rabot
-- File description:
-- PrintUniversalContent.hs
-}

module PrintUniversalContent where

import Text.Printf (printf)

import Types ( Header(..)
    , Item(..)
    , Paragraph(..)
    , UniversalContent(..)
    , Section(..)
    , Links(..) )

printHeader :: Header -> IO ()
printHeader (Header t a d) =
    putStrLn ("Title: " ++ t) >>
    putStrLn ("Author: " ++ maybe "N/A" id a) >>
    putStrLn ("Date: " ++ maybe "N/A" id d)

printLinks :: Links -> IO ()
printLinks (Link url cont) = printf "Link: %s, Content:\n" url
    >> mapM_ printItem cont
printLinks (Image url alt) = printf "Image: %s, Alt:\n" url
    >> mapM_ printItem alt

printParagraph :: Paragraph -> IO ()
printParagraph (Text str) = putStrLn $ show str
printParagraph (Content items) = putStrLn "p" >> mapM_ printItem items
    >> putStrLn "/p"

printSection :: Section -> IO ()
printSection (Section name content) =
    putStrLn ("Section: " ++ maybe "N/A" id name) >>
    putStrLn "[" >> mapM_ printItem content >> putStrLn "]"

printItem :: Item -> IO ()
printItem (ParagraphItem paragraph) = printParagraph paragraph
printItem (ListItem items) = putStrLn "list[" >> mapM_ printItem items
    >> putStrLn "]"
printItem (SectionItem section) = printSection section
printItem (CodeBlockItem cblock) = putStrLn "codeb[" >> mapM_ printItem cblock
    >> putStrLn "]"
printItem (LinksItem links) = printLinks links

printUniversalContent :: UniversalContent -> IO ()
printUniversalContent (UniversalContent h b) =
    putStrLn "Header:" >>
    printHeader h >>
    putStrLn "Body:" >>
    mapM_ printItem b
