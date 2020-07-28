{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE FlexibleContexts  #-}

import Reflex.Dom
import Data.FileEmbed
import Control.Monad
import qualified Data.Text as T
import Data.IntMap.Strict as IM
import qualified Data.Vector as V

main :: IO ()
main = mainWidgetWithCss css game
  where css = $(embedFile "css/tictactoe/game.css")

game :: MonadWidget t m => m ()
game = elClass "div" "game" $ do
  rec
    let  dxsts = constDyn $ V.singleton (V.replicate 9 T.empty, "O")
         ddynFold = join dddynFold
         ddynState = snd <$> ddynFold
         dynState = join ddynState
         dynSquares = join $ fst <$> ddynFold
         dynWinner = calculateWinner <$> dynSquares
         evFoldState = attachPromptlyDyn ddynState evJump
         dynLength = V.length <$> (fromUniqDynamic . uniqDynamic $ dynState)
         evLength = updated dynLength

    ddynStatus <- foldDyn (status dynWinner) (constDyn "Next player: X") $ leftmost [evLength, evJump]
    dddynFold <- widgetHold (foldState evMove (dxsts, 1)) $ foldState evMove <$> evFoldState
    evMove <- gameBoard dynSquares dynWinner
    evJump <- gameInfo dynLength $ join ddynStatus
  return ()
 where

    gameInfo dynLength dynStatus =
      elClass "div" "game-info" $ do
        elClass "div" "status" $ dynText dynStatus
        el "ol" $ do
          (_, evJump) <- runEventWriterT $ simpleList (flip take [1..] <$> dynLength) jumpTo
          return $ head <$> evJump

    jumpTo dynI = el "li" $ do
      (e, _) <- elAttr' "button" ("type" =: "button") $ dynText $ showI <$> dynI
      tellEvent $ tagPromptlyDyn ((:[]) <$> dynI) $ domEvent Click e

    status dynWinner i _ = ffor2 dynWinner (constDyn i) status'

    status' winner i
      | winner == "X" = "Winner: X"
      | winner == "O" = "Winner: O"
      | otherwise = if even i then "Next player: O" else "Next player: X"

    showI i | i == 1    = "Go to game start"
            | otherwise = "Go to move #" <> (T.pack . show $ (i - 1))

    foldState evMove (dxsts, i) = do
      let dxst = flip V.unsafeIndex (i - 1) <$> dxsts
      ddynFold <- foldDyn fun ((fst <$> dxst, dxsts), V.unsafeTake i) $ (head . IM.keys) <$> evMove
      return $ fst <$> ddynFold

    gameBoard dynSquares dynWinner = elClass "div" "game-board" $ do
      let keyss = V.fromList $ V.fromList <$> [[1..3], [4..6], [7..9]]
      rec
        (_, evMove) <- runEventWriterT $ board (fanInt evMove) dynSquares dynWinner keyss
      return evMove

    fun j ((dxs, dxsts), f) =
      let dxsts' = f <$> dxsts
          dynTurn' = invers . snd . V.unsafeLast <$> dxsts'
          dxs' = ffor2 dynTurn' dxs $ flip V.unsafeUpd . (:[]) . (,) (j - 1)
          dxst' = zipDyn dxs' dynTurn'
      in ((dxs', ffor2 dxsts' dxst' V.snoc), id)

    invers "X" = "O"
    invers "O" = "X"

    calculateWinner squares =
      let lines = V.fromList $ V.fromList <$> [
            [0, 1, 2],
            [3, 4, 5],
            [6, 7, 8],
            [0, 3, 6],
            [1, 4, 7],
            [2, 5, 8],
            [0, 4, 8],
            [2, 4, 6]]
          g = flip (.) V.unsafeIndex . (.) . V.unsafeIndex
          f squares _ line = do
            let fst = g squares line 0
                snd = g squares line 1
                trd = g squares line 2
            if fst /= T.empty && fst == snd && fst == trd then Left fst else Right T.empty
      in either id id $ V.foldM' (f squares) T.empty lines

    board = ((V.mapM_ .) . ) . (((elClass "div" "board-row" .) .) .) . ((V.mapM_ .) .) . square

    square evSelector dynSquares dynWinner key  = do
      let dynSymbol = flip V.unsafeIndex (key - 1) <$> dynSquares
      (e, _) <- elAttr' "button" ("type" =: "button" <> "class" =: "square") $ dynText dynSymbol
      let  evClickSquare = IM.singleton key <$> domEvent Click e
      tellEvent $ gate (current (filterCliks <$> zipDyn dynWinner dynSymbol)) evClickSquare

    filterCliks p
      | p == (T.empty, T.empty) = True
      | otherwise = False
