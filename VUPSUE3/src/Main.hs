import System.IO
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk hiding (Action, backspace)

main :: IO ()
main = do
  void initGUI
  window <- windowNew
  
  set window [  windowTitle := "Editor",
                windowResizable := True,
                windowDefaultWidth := 800,
                windowDefaultHeight := 600 ]
  
  widgetShowAll window
  mainGUI