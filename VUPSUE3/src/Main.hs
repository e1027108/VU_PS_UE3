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

    elSave   <- UI.button # set UI.text "Save"
    elLoad   <- UI.button # set UI.text "Load"
    elPath   <- UI.input # set UI.value "C:\\"
    elText   <- UI.textarea

    inputs   <- liftIO $ newIORef []
    
    -- functionality
    let
        redoLayout :: UI ()
        redoLayout = void $ do
            layout <- mkLayout =<< liftIO (readIORef inputs)
            getBody w # set children [layout]

        mkLayout :: [Element] -> UI Element
        mkLayout xs = column $
            [row [element elSave, element elLoad, element elPath], row [element elText] ]
    
    redoLayout