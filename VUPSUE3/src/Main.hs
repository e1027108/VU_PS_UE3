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

    elSave    <- UI.button # set UI.text "Save"
    elLoad <- UI.button # set UI.text "Load"
    elText   <- UI.textarea
    elResult <- UI.span

    inputs   <- liftIO $ newIORef []
    
    -- functionality
    let
        displayTotal = void $ do
            xs <- mapM (get value) =<< liftIO (readIORef inputs)
            element elResult # set text (showNumber . sum $ map readNumber xs)
        
        redoLayout :: UI ()
        redoLayout = void $ do
            layout <- mkLayout =<< liftIO (readIORef inputs)
            getBody w # set children [layout]
            displayTotal

        mkLayout :: [Element] -> UI Element
        mkLayout xs = column $
            [row [element elSave, element elLoad], row [element elText] ]
        
        addInput :: UI ()
        addInput = do
            elInput <- UI.input # set value "0"
            on (domEvent "livechange") elInput $ \_ -> displayTotal
            liftIO $ modifyIORef inputs (elInput:)
        
        removeInput :: UI ()
        removeInput = liftIO $ modifyIORef inputs (drop 1)
    
    on UI.click elSave    $ \_ -> addInput    >> redoLayout
    on UI.click elLoad $ \_ -> removeInput >> redoLayout
    addInput >> redoLayout


{-----------------------------------------------------------------------------
    Functionality
------------------------------------------------------------------------------}
type Number = Maybe Double

instance Num Number where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

readNumber :: String -> Number
readNumber s = listToMaybe [x | (x,"") <- reads s]    
showNumber   = maybe "--" show