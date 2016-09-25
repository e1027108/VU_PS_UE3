{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Main where

import Control.Applicative
import Control.Monad
import Data.IORef
import Debug.Trace
import Datatypes as DT

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core


-- | Main entry point.
main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup w = do
    -- active elements
    return w # set title "Editor" 

    elSave <- UI.button # set UI.text "Save" # set style [("margin-right", "5px")]
    elLoad <- UI.button # set UI.text "Load" # set style [("margin-right", "5px")]
    elPath <- UI.input # set UI.value "C:\\" # set style [("width", "200px")]
    elCheck <- UI.button # set UI.text "Check Syntax"
    elComment <- UI.label # set style [("margin-left","20px"),("width","300px")]
    elText <- UI.textarea # set style [("width", "100%"),("height", "100%"),("padding-left","10px"),("-webkit-box-sizing", "border-box"),
     ("-moz-box-sizing", "border-box"),("box-sizing","border-box"),("tab-size","2")] # set (attr "spellcheck") "false"

    --This would be missing automatic scroll bars I think
    {-elText <- UI.div # set style [("border","1px"),("width", "100%"),("height", "100%"),("padding-left","10px"),("-webkit-box-sizing", "border-box"),
     ("-moz-box-sizing", "border-box"),("box-sizing","border-box"),("tab-size","2")] # set (attr "contenteditable" ) "true" # set (attr "spellcheck") "false"-}

    inputs <- liftIO $ newIORef []

    -- functionality
    let
        interrupt = 0
    
        redoLayout :: UI ()
        redoLayout = void $ do
            layout <- mkLayout =<< liftIO (readIORef inputs)
            getBody w # set children [layout] 

        mkLayout :: [Element] -> UI Element
        mkLayout xs = column $
            [row [element elSave, element elLoad, element elPath, element elComment] # set style [("margin-bottom", "10px")]
            , row [element elText] # set style [("width","1000px"),("height","600px"), ("margin-bottom", "10px")], row [element elCheck]]

        loadContents :: UI Element
        loadContents = do
            path <- elPath # get value
            content <- liftIO(readFile path)
            --let formatted = formatHTML content
            --element elText # set html formatted
            element elText # set html content
        
        {- -- in text area this will just print the tags with it
        formatHTML :: String -> String
        formatHTML input = do
            let front = "<html><body><div style=\"color:blue\">"
            let back = "</div></body></html>"
            let middle = insertBreak (lines input)
            trace (show (front ++ middle ++ back)) (front ++ middle ++ back)
            
        insertBreak :: [String] -> String
        insertBreak arr
            | (length arr) == 0 = ""
            | otherwise = (((head arr) ++ "<br/>") ++ insertBreak (tail arr))-}
            
        saveContents :: UI ()
        saveContents = do
            path <- elPath # get value
            content <- elText # get value
            liftIO(writeFile path content)
        
        checkSyntax :: UI Element
        checkSyntax = do
            code <- elText # get value
            if (snd (DT.checkSyntax code)) then
                element elComment # set text "checkSyntax: true" # set style [("color","#44FF44")]
            else
                element elComment # set text "checkSyntax: false" # set style [("color","#FF4444")]
        
        resetComment :: UI Element
        resetComment = do
            element elComment # set text ""

    on UI.click elLoad $ \_ -> loadContents
    on UI.click elSave $ \_ -> saveContents
    on UI.click elCheck $ \_ -> checkSyntax
    on UI.valueChange elText $ \_ -> resetComment
    redoLayout