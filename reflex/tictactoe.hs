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
import Data.Vector as V ( empty, singleton, cons, snoc, replicate, fromList, unsafeUpd
                       , unsafeIndex, unsafeHead, unsafeTail, unsafeTake, unsafeDrop
                       , unsafeLast, length, null, uniq, filter, map, mapM_ )

main :: IO ()
main = mainWidgetWithCss css game
 where css = $(embedFile "css/tictactoe/game.css")

game :: MonadWidget t m => m ()
game = elClass "div" "game" $ do
 rec
   let  dim = 3; dim2 = dim ^ 2
        keysGrid = matrix dim $ V.fromList [1..dim2]
        evJumpTo = head <$> evJumpTo'
        dxsts = constDyn $ V.singleton (V.replicate dim2 T.empty, "O")
        ddynStateList = (fst <$>) <$> ddynStateListWinner
        dynStateList = join ddynStateList
        evGameBoard = attachPromptlyDyn ddynStateList evJumpTo

        uniqDynStateList = fromUniqDynamic . uniqDynamic $ dynStateList
        dynLength = V.length <$> uniqDynStateList
        evLength = updated dynLength
        dynWinner = join $ (snd <$>) <$> ddynStateListWinner

   ddynStatus <- foldDyn (status dynWinner) (constDyn "Next player: X") $ leftmost [evLength, evJumpTo]
   ddynStateListWinner <- widgetHold (gameBoard dim keysGrid (dxsts, 1)) $ gameBoard dim keysGrid <$> evGameBoard
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

   gameBoard dim keysGrid (dxsts, i) = elClass "div" "game-board" $ do
     rec
       let dxst = flip V.unsafeIndex (i - 1)  <$> dxsts
           dxs = fst <$> dxst
           ddynFold' = fst <$> ddynFold
           dynStateList = join $ snd <$> ddynFold'
           dynState = join $ fst <$> ddynFold'
           dynTurn = snd . unsafeLast <$> dynStateList
           dynWinner = checkWinner dim <$> dynState

       ddynFold <- foldDyn fun ((dxs, dxsts), V.unsafeTake i) $ (head . IM.keys) <$> evMap
       (_, evMap) <- runEventWriterT $ board (fanInt evMap) dynState dynTurn dynWinner keysGrid
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

   checkWinner n vs =
    let lines = matrix n vs
        columns = transpose lines
        diagonal1 = diagonal lines
        diagonal2 = diagonal' lines
        elements = lines <> columns <> V.singleton diagonal1 <> V.singleton  diagonal2
        vvs = V.filter (\vs -> V.length vs == 1 && V.unsafeHead vs /= T.empty) $ V.map V.uniq elements
    in if vvs == V.empty then T.empty else V.unsafeHead . V.unsafeHead $ vvs

   transpose vvs
     | V.null $ V.unsafeHead vvs = V.empty
     | otherwise = V.cons (V.map V.unsafeHead vvs) (transpose (V.map V.unsafeTail vvs))

   diagonal vvs =
     let go vvs i
          | i == V.length vvs = V.empty
          | otherwise = V.cons (V.unsafeIndex (V.unsafeIndex vvs i) i) (go vvs (i + 1))
     in go vvs 0

   diagonal' vvs =
     let go vvs n i
          | i == n = V.empty
          | otherwise = V.cons (V.unsafeIndex (V.unsafeIndex vvs i) (n - i - 1)) (go vvs n (i + 1))
     in go vvs (V.length vvs) 0

   matrix n vs
     | V.null vs = V.empty
     | otherwise = V.cons (V.unsafeTake n vs) $ matrix n $ V.unsafeDrop n vs

   board evSelector dynState dynTurn dynWinner keyss =
     elClass "div" "board" $ do
       el "div" $ V.mapM_ (boardRow evSelector dynState dynTurn dynWinner) keyss

   boardRow evSelector dynState dynTurn dynWinner keys =
     elClass "div" "board-row" $ V.mapM_ (square evSelector dynState dynTurn dynWinner) keys

   square evSelector dynState dynTurn dynWinner key  = do
     let dynPlayer = flip V.unsafeIndex (key - 1) <$> dynState
         evTurn = tagPromptlyDyn dynTurn $ selectInt evSelector key
     ddynSymbol <- foldDyn (<$) dynPlayer $ leftmost [evTurn, updated dynPlayer]
     (e, _) <- elAttr' "button" ("type" =: "button" <> "class" =: "square") $ dynText $ join ddynSymbol
     let  evClickSquare = IM.singleton key <$> domEvent Click e
     tellEvent $ gate (current (filterCliks <$> zipDyn dynWinner dynPlayer)) evClickSquare

   filterCliks p
     | p == (T.empty, T.empty) = True
     | otherwise = False
