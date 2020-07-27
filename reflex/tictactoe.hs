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
         ddynStateList = snd <$> ddynFold
         dynStateList = join ddynStateList
         dynSquares = join $ fst <$> ddynFold
         dynWinner = calculateWinner <$> dynSquares
         evFoldState = attachPromptlyDyn ddynStateList evJumpTo

         uniqDynStateList = fromUniqDynamic . uniqDynamic $ dynStateList
         dynLength = V.length <$> uniqDynStateList
         evLength = updated dynLength

    ddynStatus <- foldDyn (status dynWinner) (constDyn "Next player: X") $ leftmost [evLength, evJumpTo]
    dddynFold <- widgetHold (foldState evNewMove (dxsts, 1)) $ foldState evNewMove <$> evFoldState
    evNewMove <- gameBoard dynSquares dynWinner
    evJumpTo <- gameInfo dynLength $ join ddynStatus
  return ()
 where

    gameInfo dynLength dynStatus =
      elClass "div" "game-info" $ do
        elClass "div" "status" $ dynText dynStatus
        el "ol" $ do
          (_, evJumpTo) <- runEventWriterT $ simpleList (flip take [1..] <$> dynLength) jumpTo
          return $ head <$> evJumpTo

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

    foldState evNewMove (dxsts, i) = do
      let dxst = flip V.unsafeIndex (i - 1) <$> dxsts
      ddynFold <- foldDyn fun ((fst <$> dxst, dxsts), V.unsafeTake i) $ (head . IM.keys) <$> evNewMove
      return $ fst <$> ddynFold

    gameBoard dynSquares dynWinner = elClass "div" "game-board" $ do
      let keyss = V.fromList $ V.fromList <$> [[1..3], [4..6], [7..9]]
      rec
        (_, evNewMove) <- runEventWriterT $ board (fanInt evNewMove) dynSquares dynWinner keyss
      return evNewMove

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

          g squares line = V.unsafeIndex squares . V.unsafeIndex line
          f squares _ line = do
            let first  = g squares line 0
                second = g squares line 1
                third  = g squares line 2
            if first /= T.empty && first == second && first == third then Left first else Right T.empty

      in either id id $ V.foldM' (f squares) T.empty lines

    board evSelector dynSquares dynWinner keyss =
      elClass "div" "board" $ do
        el "div" $ V.mapM_ (boardRow evSelector dynSquares dynWinner) keyss

    boardRow evSelector dynSquares dynWinner keys =
      elClass "div" "board-row" $ V.mapM_ (square evSelector dynSquares dynWinner) keys

    square evSelector dynSquares dynWinner key  = do
      let dynPlayer = flip V.unsafeIndex (key - 1) <$> dynSquares
      (e, _) <- elAttr' "button" ("type" =: "button" <> "class" =: "square") $ dynText dynPlayer
      let  evClickSquare = IM.singleton key <$> domEvent Click e
      tellEvent $ gate (current (filterCliks <$> zipDyn dynWinner dynPlayer)) evClickSquare

    filterCliks p
      | p == (T.empty, T.empty) = True
      | otherwise = False
