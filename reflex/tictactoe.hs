{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecursiveDo          #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE MonoLocalBinds       #-}

import Reflex.Dom
import Data.FileEmbed
import Control.Monad ( join )
import Data.Text as T ( empty, pack )
import Data.IntMap.Strict as IM ( keys, singleton )
import Data.Vector as V ( empty, singleton, snoc, replicate, fromList, unsafeUpd
                        , unsafeIndex,  unsafeTake, unsafeLast, length, foldM' )

main :: IO ()
main = mainWidgetWithCss css game
  where css = $(embedFile "css/tictactoe/game.css")

game :: MonadWidget t m => m ()
game = elClass "div" "game" $ do
  rec
    let  evJumpTo = head <$> evJumpTo'
         dxsts = constDyn $ V.singleton (V.replicate 9 T.empty, "O")
         ddynStateList = (fst <$>) <$> ddynStateListWinner
         dynStateList = join ddynStateList
         evGameBoard = attachPromptlyDyn ddynStateList evJumpTo

         uniqDynStateList = fromUniqDynamic . uniqDynamic $ dynStateList
         dynLength = V.length <$> uniqDynStateList
         evLength = updated dynLength
         dynWinner = join $ (snd <$>) <$> ddynStateListWinner

    ddynStatus <- foldDyn (status dynWinner) (constDyn "Next player: X") $ leftmost [evLength, evJumpTo]
    ddynStateListWinner <- widgetHold (gameBoard (dxsts, 1)) $ gameBoard <$> evGameBoard
    (_, evJumpTo') <- runEventWriterT $ gameInfo dynLength $ join ddynStatus
  return ()
 where

    gameInfo dynLength dynStatus =
      elClass "div" "game-info" $ do
        elClass "div" "status" $ dynText dynStatus
        el "ol" $ do
          (_, evJumpTo') <- runEventWriterT $ simpleList (flip take [1..] <$> dynLength) jumpTo
          tellEvent evJumpTo'

    status dynWinner i _ = ffor2 dynWinner (constDyn i) status'

    status' winner i
      | winner == "X" = "Winner: " <> "X"
      | winner == "O" = "Winner: " <> "O"
      | otherwise = if even i then "Next player: O" else "Next player: X"

    jumpTo dynI = el "li" $ do
      (e, _) <- elAttr' "button" ("type" =: "button") $ dynText $ showI <$> dynI
      tellEvent $ tagPromptlyDyn ((:[]) <$> dynI) $ domEvent Click e

    showI i | i == 1    = "Go to game start"
            | otherwise = "Go to move #" <> (pack . show $ (i - 1))

    gameBoard (dxsts, i) = elClass "div" "game-board" $ do
      rec
        let dxst = flip V.unsafeIndex (i - 1)  <$> dxsts
            dxs = fst <$> dxst
            ddynFold' = fst <$> ddynFold
            dynStateList = join $ snd <$> ddynFold'
            dynSquares = join $ fst <$> ddynFold'
            dynTurn = snd . unsafeLast <$> dynStateList
            dynWinner = calculateWinner <$> dynSquares

        ddynFold <- foldDyn fun ((dxs, dxsts), V.unsafeTake i) $ (head . IM.keys) <$> evMap
        (_, evMap) <- runEventWriterT $ board (fanInt evMap) dynSquares dynTurn dynWinner
      return $ zipDyn dynStateList dynWinner

    fun j ((dxs, dxsts), f) =
      let dxsts' = f <$> dxsts
          dynTurn = snd . V.unsafeLast <$> dxsts'
          dynTurn' = invers <$> dynTurn
          dxs' = ffor2 dynTurn' dxs $ flip V.unsafeUpd . (:[]) . (,) (j - 1)
          dxst' = zipDyn dxs' dynTurn'
      in ((dxs', ffor2 dxsts' dxst' V.snoc), id)

    invers "X" = "O"
    invers "O" = "X"

    calculateWinner squares =
      let lines = V.fromList [
            V.fromList [0, 1, 2],
            V.fromList [3, 4, 5],
            V.fromList [6, 7, 8],
            V.fromList [0, 3, 6],
            V.fromList [1, 4, 7],
            V.fromList [2, 5, 8],
            V.fromList [0, 4, 8],
            V.fromList [2, 4, 6]]

          f squares _ line = do
            let first  = V.unsafeIndex squares (V.unsafeIndex line 0)
                second = V.unsafeIndex squares (V.unsafeIndex line 1)
                third  = V.unsafeIndex squares (V.unsafeIndex line 2)
            if first /= T.empty && first == second && first == third then Left first else Right T.empty

      in either id id $ V.foldM' (f squares) T.empty lines

    board evSelector dynSquares dynTurn dynWinner =
      elClass "div" "board" $ do
        elClass "div" "board-row" $ do
          square evSelector dynSquares dynTurn dynWinner 1
          square evSelector dynSquares dynTurn dynWinner 2
          square evSelector dynSquares dynTurn dynWinner 3
        elClass "div" "board-row" $ do
          square evSelector dynSquares dynTurn dynWinner 4
          square evSelector dynSquares dynTurn dynWinner 5
          square evSelector dynSquares dynTurn dynWinner 6
        elClass "div" "board-row" $ do
          square evSelector dynSquares dynTurn dynWinner 7
          square evSelector dynSquares dynTurn dynWinner 8
          square evSelector dynSquares dynTurn dynWinner 9

    square evSelector dynSquares dynTurn dynWinner key  = do
      let dynPlayer = flip V.unsafeIndex (key - 1) <$> dynSquares
          evTurn = tagPromptlyDyn dynTurn $ selectInt evSelector key
      ddynSymbol <- foldDyn (<$) dynPlayer $ leftmost [evTurn, updated dynPlayer]
      (e, _) <- elAttr' "button" ("type" =: "button" <> "class" =: "square") $ dynText $ join ddynSymbol
      let  evClickSquare = IM.singleton key <$> domEvent Click e
      tellEvent $ gate (current (filterCliks <$> zipDyn dynWinner dynPlayer)) evClickSquare

    filterCliks p
      | p == (T.empty, T.empty) = True
      | otherwise = False
