{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

import Control.Applicative
import Control.Monad
import Data.IORef
import Data.Maybe

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
    elComment <- UI.label # set style [("margin-left","20px"),("width","300px")]
    elText <- UI.textarea # set style [("width", "100%"),("height", "100%"),("padding-left","10px"),("-webkit-box-sizing", "border-box"),
     ("-moz-box-sizing", "border-box"),("box-sizing","border-box"),("tab-size","2")] # set (attr "spellcheck") "false"
     
    --This would be missing automatic scroll bars I think
    {-elText <- UI.thehtml # set style [("border","1px"),("width", "100%"),("height", "100%"),("padding-left","10px"),("-webkit-box-sizing", "border-box"),
     ("-moz-box-sizing", "border-box"),("box-sizing","border-box"),("tab-size","2")] # set (attr "contenteditable" ) "true" # set (attr "spellcheck") "false"-}

    inputs <- liftIO $ newIORef []

    -- functionality
    let
        redoLayout :: UI ()
        redoLayout = void $ do
            layout <- mkLayout =<< liftIO (readIORef inputs)
            getBody w # set children [layout] 

        mkLayout :: [Element] -> UI Element
        mkLayout xs = column $
            [row [element elSave, element elLoad, element elPath, element elComment] # set style [("margin-bottom", "10px")]
            , row [element elText] # set style [("width","1000px"),("height","600px")]]

        loadContents :: UI Element
        loadContents = do
            path <- elPath # get value
            content <- liftIO(readFile path)
            element elText # set text content
            
        saveContents :: UI ()
        saveContents = do
            path <- elPath # get value
            content <- elText # get value
            liftIO(writeFile path content)
        
        --check syntax prototype, huge changes needed
        checkSyntax :: UI ()
        checkSyntax = void $ do
            elComment <- UI.label # set style [("color","green")]
            element elComment # set text "checkSyntax: true"
            --redoLayout

    on UI.click elLoad $ \_ -> loadContents
    on UI.click elSave $ \_ -> saveContents
    on UI.valueChange elText $ \_ -> checkSyntax --doesn't work
    redoLayout