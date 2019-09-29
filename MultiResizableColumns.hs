{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}

module XMonad.Layout.MultiResizeableColumns (
    -- * Usage
    -- $usage
    multiResizeableColumns,
    multiResizeableColumns,
    ResizeMirror(..)
    ) where

import XMonad
import qualified.Xmonad.StackSet as W
import Control.Monad
import Data.List ((\\)),mapAccumL)

data ResizeMirror = ShrinkMirror | ExpandMirror deriving Typeable

instance Message ResizeMirror

data MultiResizeableColumns a = MultiResizeableColumns
        {
            isChanged       :: !Bool
        ,   columnList      :: ![Int]
        ,   storedCols      :: ![Int]
        ,   sizeUnit        :: !Double
        ,   sizeList        :: ![(Double,[Double])]
        ,   initialCols     :: !Int
        } deriving (Show, Read, Eq)

multiResizableColumns colList delta sizeL nInitCol = newOne
where newOne = MultiResizableColumns True colList [] delta sizeL nInitCol

instance LayoutClass MultiResizableColumns a where
  doLayout this scr s = return (zip vWins rect_list,result)
    where
      this'  | changed   = doLayoutBody this s
             | otherwise = this
      result | this == this' = Nothing
             | otherwise     = Just this'
      vWins     = (W.integrate s)
      changed   = isChanged this || (length vWins /= sum (columnList this))
      rect_list = asRectList scr this'

  handleMessage this msg =
    do mstack <- (W.stack . W.workspace . W.current) `fmap` gets windowset
       return $ mstack >>= handleMessageM
    where
      handleMessageM stack = msum [
        fmap (\x -> resize     x stack) (fromMessage msg)
       ,fmap (\x -> resizeM    x stack) (fromMessage msg)
       ,fmap (\x -> incmastern x stack) (fromMessage msg)
       ]
      incmastern (IncMasterN x) s = setChanged $ addMasterWindowCount this s x
      resize Shrink        = doResize (-1) 0
      resize Expand        = doResize ( 1) 0
      resizeM ShrinkMirror = doResize 0    (-1)
      resizeM ExpandMirror = doResize 0    ( 1)
      setChanged obj   = obj { isChanged = True }
      doResize x y s = setChanged $ resizeFocused this s w h
          where (w,h) = let delta = (sizeUnit this) in (delta*x,delta*y)
  description _ = "MultiResizableColumns"



-- main layout functions
-- "some'" is new value of "some"
--
doLayoutBody obj stack = obj' { isChanged = False }
where oNew = refreshColumns obj (length $ W.integrate stack)
      obj' = refreshSizeList obj oNew stack

refreshSizeList objOld objNew stack = objNew {
      sizeList = getNewSizeListByColumns objOld objNew stack
  }

refreshColumns obj nwins = oActual'
where oStored' = refreshStoredColumns obj      nwins
      oActual' = refreshActualColumns oStored' nwins

refreshStoredColumns obj nwins = obj { storedCols = vStoredColumns' }
where vStoredColumns' = getResizedColumnList' nwins (initialCols obj) (storedCols obj)

refreshActualColumns obj nwins = obj { columnList = vActualColumns' }
where vActualColumns' = getActualColumnList nwins (initialCols obj) (storedCols obj)


getNewSizeListByColumns objOld objNew stack = zip vWidths' vvHeights'
where
  (vOldCols,vNewCols) = (columnList objOld,columnList objNew)
  (nOldCols,nNewCols) = (length vOldCols,  length vNewCols  )
  nInitial      = (initialCols objOld)
  vSizeCompl    = zip (getCompl nNewCols) (repeat $ take nInitial $ repeat (1/fromIntegral nInitial))
  vSizeListC    = override (sizeList objOld) vSizeCompl
  getSizeList'  = adjustRefresh (sizeUnit objOld)
  vWidths'      = getSizeList' (fmap fst vSizeListC) nOldCols nNewCols
  vvHeights     = fmap snd vSizeListC
  vvHeights'    = override (zipWith3 getSizeList' vvHeights vOldCols vNewCols) vvHeights

getResizedColumns takeFunc nWins nInit vCols = vCols'
  where nRest = nWins - sum vCols
        vCols' | nRest > 0  = vCols ++ (takeBaseFunc takeFunc) nRest (repeat nInit)
               | otherwise  = (takeBaseFunc takeFunc) nWins vCols

getResizedColumnList' = getResizedColumns tuUntil
getActualColumnList   = getResizedColumns tuActual


reduceInvalidCols v = filter (> 0) v

tuUntil  nRest nElem = nElem
tuActual nRest nElem = nRest

takeBaseFunc f n []      = []
takeBaseFunc f n (x:xs)  | n <= x    = [f n x]
                       | otherwise = (x:takeBaseFunc f (n-x) xs)

elemAddValue n list val = elemReplace n list ( (list !! n) + val )

elemReplace n [] val = []
elemReplace n list@(x:xs) val
  | n >= nL = list
  | (n+nL)<0 = list
  | n == 0  = val:xs
  | n >  0  = take n list ++ (val:drop (n+1) list)
  | n <  0  = elemReplace (n + nL) list val
where
  nL = length list

getRidOf index list = (take index list) ++ (drop (index+1) list)
insertTo index list value = (take index list) ++ (value :(drop index list))

-- utilities

adjustRefresh  :: Double -> [Double] -> Int -> Int -> [Double]
adjustRefresh unit h from to
  | to   <  0     = h
  | from <  0     = adjustRefresh unit h 0 to
  | length h < to = adjustRefresh unit (getCompled h to) from to
  | from <  to    = adjustRefresh unit vRejusted (from+1) to
  | from >= to    = (normalize (take to h)) ++ (drop to h)
where
  vRejusted   = resizeFrontOf h (from+1) from 0 unit
--}

-- calculate focused index
columnIndexF  this stack = fst (positionF  this stack)
columnIndexOf cols nWin  = fst (positionOf cols nWin)

positionF this stack = positionOf (columnList this) (length (W.up stack))

positionOf []     nWin = (0,nWin)
positionOf (c:cs) nWin
  | nWin >=c  = (nx+1,ny)
  | otherwise = (0,nWin)
where
  (nx,ny) = positionOf cs (nWin - c)

-- adjust delta
--
--
adjustResize []   index unit delta = []
adjustResize list index unit 0     = normalize list
adjustResize list index unit delta
| length list <= index = list
| otherwise  = normalize (insertTo index rest' target')
  where
    rest     = normalize ( getRidOf index list )
    rest'    = compressRationalListForSpace rest unit (delta + (list !! index))
    target'  = 1 - sum rest'

-- compress rest of columns until capable to allocate space
compressRationalListForSpace :: [Double] -> Double -> Double -> [Double]
compressRationalListForSpace list unit space
| bOverCap  = take nLength $ repeat nLowLim
| otherwise = fmap ( (+ nLowLim) . (* (1 - nRequired))) (normalize list)
  where
    bOverCap  = nMinimum >= (1-nSpace')
    nUnit'    = between 0 1 unit
    nSpace'   = between nLowLim nMaxSpace space
    nLowLim   = nUnit'
    nLength   = length list
    nMinimum  = (nLowLim * fromIntegral nLength)
    nMaxSpace = (1 - nMinimum)
    nRequired = (nSpace'+nMinimum)

addMasterWindowCount this s x = refreshActualColumns this' nWin
  where
      this'  = this { storedCols = changeWindowCount nWin (storedCols this) (columnIndexF this s) x }
      nWin   = (length $ W.integrate s)

changeWindowCount limit cols _     0   = cols
changeWindowCount limit cols index val
| not $ avail cols     = changeWindowCount limit (reduceInvalidCols cols) index' val
| (sum cols + val) < 0 = []
| cols == []           = [val]
| overflow < 0         = reduceInvalidCols $ changeWindowCount limit (reduceInvalidCols cols) index' (max 0 (val+overflow))
| otherwise            = reduceInvalidCols $ changeWindowCount limit vCols' index' (val-nDir)
  where
    overflow = (limit-(val+(cols!!index')))
    vCols'   = reduceInvalidCols $ elemAddValue index' cols nDir
    nDir     = if val > 0 then 1 else (0-1)
    nLen     = length cols - 1
    avail  v = all    (> 0) v
    index'   = between 0 (nLen - 1) posIndex
      where posIndex | 0 > index = nLen + index
                     | otherwise = index

resizeFocused this stack deltaWidth deltaHeight = this {
  sizeList = zip vWidth' vvHeights'
}
where
  (cx,cy)    = positionF this stack
  vWidth     = fmap fst (sizeList this)
  vvHeights  = fmap snd (sizeList this)
  vHeights   = (vvHeights !! cx)
  nColumns    = length $ columnList this
  nActualRows = columnList this !! cx
  vWidth'    = resizeFrontOf vWidth   nColumns    cx deltaWidth  (sizeUnit this)
  vHeights'  = resizeFrontOf vHeights nActualRows cy deltaHeight (sizeUnit this)
  vvHeights' = elemReplace cx vvHeights vHeights'

resizeFrontOf list actualLen index 0   unit = list
resizeFrontOf list actualLen index val unit = vActual' ++ vRest
where vActual' = adjustResize (take actualLen list) index unit val
      vRest    = drop actualLen list

-- current-state to rect-list
asRectList scr this = foldl (++) [] rectCols
where
  (_,rectCols) = mapAccumL accumRect scr $ toPhysicalPixels this scr

-- rational size list to physical rect
toPhysicalPixels this scr = zip vWidths vHeights
where
  vSizeReq = zipWith limitSize (columnList this) $ sizeList this
  limitSize nRows oSize = snd_r oSize (take nRows (snd oSize))
  vWidths  = allocateToPhysicalSpace (rect_width scr) (fmap fst vSizeReq)
  vHeights = fmap ((allocateToPhysicalSpace (rect_height scr)) . snd) vSizeReq

-- rational value list (sum of list is 1)
-- to physical pixel list (sum of list is "space").
allocateToPhysicalSpace space list = front ++ [last_element]
where
  front        = fmap (round.(* fromIntegral space)) (init' list)
  last_element = space - sum front

accumRect rect_base size = (rect_next,heights)
where
  rect_next = rect_base { rect_x     = fromIntegral (fst size) + rect_x rect_base }
  rect_this = rect_base { rect_width = fst size }
  heights   = snd $ mapAccumL accumRectHeight rect_this (snd size)

accumRectHeight rect_base height = (rect_next,rect_out)
where
  rect_next = rect_base { rect_y      = fromIntegral height + rect_y rect_base }
  rect_out  = rect_base { rect_height = height }

--utils

init' ls = drop 1 (init (0:ls))
-- normalize :: [Double] -> [Double]
normalize ls
| total <= 0 = ls
| otherwise  = fmap (/ total) ls
  where
    total = sum ( fmap (max 0) ls )

between from to val
| from > to = between to from val
| otherwise = max from $ min val to

snd_r x = (,) (fst x)
getCompl        n    = take n [ 1/x | x <- [1..] ]
getCompled list n    = override list $ getCompl n
override   list base = list ++ drop (length list) base
