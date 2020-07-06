{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module MassivTalk.Day1.Intro09_Stencil where

import Control.Exception
import Control.Monad (msum, void, when)
import Data.Char
import qualified Data.Map.Strict as Map
import Data.Massiv.Array as A
import Data.Maybe
import Data.Word
import System.Console.ANSI as C


------------------
-- Game of Life --
------------------


lifeRules :: Word8 -> Word8 -> Word8
lifeRules 0 3 = 1
lifeRules 1 2 = 1
lifeRules 1 3 = 1
lifeRules _ _ = 0

lifeStencil :: Stencil Ix2 Word8 Word8
lifeStencil = makeStencil (Sz (3 :. 3)) (1 :. 1) $ \ get ->
  lifeRules <$> get (0 :. 0) <*>
  (get (-1 :. -1) + get (-1 :. 0) + get (-1 :. 1) +
   get ( 0 :. -1)         +         get ( 0 :. 1) +
   get ( 1 :. -1) + get ( 1 :. 0) + get ( 1 :. 1))

--
-- >>> :t makeStencil

lifeStep :: Array S Ix2 Word8 -> Array DW Ix2 Word8
lifeStep = mapStencil Wrap lifeStencil

-- >>> :t mapStencil






--------------------
-- Langton's Loop --
--------------------

-- |
--    T
--  L C R
--    B
data VonNeumann =
  VonNeumann {-# UNPACK #-}!Word8 -- Center
             {-# UNPACK #-}!Word8 -- Top
             {-# UNPACK #-}!Word8 -- Right
             {-# UNPACK #-}!Word8 -- Bottom
             {-# UNPACK #-}!Word8 -- Left
  deriving (Eq, Ord, Show)

langtonStencil :: Stencil Ix2 Word8 VonNeumann
langtonStencil =
  makeStencil (Sz2 3 3) (1 :. 1) $ \get -> VonNeumann <$>
                                           get ( 0 :.  0) <*>
                                           get (-1 :.  0) <*>
                                           get ( 0 :.  1) <*>
                                           get ( 1 :.  0) <*>
                                           get ( 0 :. -1)

transition :: Array P Ix2 Word8 -> Array P Ix2 Word8
transition = compute . mapStencil Wrap (nextState <$> langtonStencil)

nextState :: VonNeumann -> Word8
nextState s@(VonNeumann c t r b l) =
  fromMaybe (error $ "Unrecognized state: " ++ show s) $
  msum
    [ nextStateMaybe s
    , nextStateMaybe (VonNeumann c r b l t)
    , nextStateMaybe (VonNeumann c b l t r)
    , nextStateMaybe (VonNeumann c l t r b)
    ]


initArray :: Sz2 -> Array P Ix2 Word8
initArray sz@(Sz2 m n) =
  computeAs P $ makeLoadArrayS sz 0 $ \w ->
    iforM_ initialState $ \(i :. j) -> w (si + i :. sj + j)
  where
    (si :. sj) = m - 33 :. n - 33

printArray :: Source r Ix2 Word8 => Array r Ix2 Word8 -> IO ()
printArray arr = iforM_ arr $ \ (_ :. j) w8 -> do
  C.setSGR [C.SetColor C.Background C.Vivid (stateToColor w8)]
  putStr "  "
  let Sz (_ :. n) = size arr
  when (j + 1 == n) $ putChar '\n'

runLangton :: Sz2 -> Int -> IO ()
runLangton sz n =
  (C.hideCursor >> go 0 (initArray sz)) `finally`
  (C.clearScreen >> C.showCursor)
  where
    go i arr
      | i > n = void getChar
      | otherwise = do
        C.setCursorPosition 0 0
        printArray arr
        C.setSGR []
        go (i + 1) $ transition arr

stateToColor :: Word8 -> C.Color
stateToColor = \case
  0 -> C.Black
  1 -> C.Blue
  2 -> C.Red
  3 -> C.Green
  4 -> C.Yellow
  5 -> C.Magenta
  6 -> C.White
  7 -> C.Cyan
  n -> error $ "Unexpected state" ++ show n


initialState :: Array P Ix2 Word8
initialState =
  compute $
  A.map (fromIntegral . subtract 48 . ord) (stateStr :: Array P Ix2 Char)
  where
    stateStr =
      fromLists'
        Par
        [ "022222222000000"
        , "217014014200000"
        , "202222220200000"
        , "272000021200000"
        , "212000021200000"
        , "202000021200000"
        , "272000021200000"
        , "212222221222220"
        , "207107107111112"
        , "022222222222220"
        ]


nextStateMaybe :: VonNeumann -> Maybe Word8
nextStateMaybe s =
  case s of
    VonNeumann 0 0 0 0 0 -> Just 0
    VonNeumann 0 0 0 0 1 -> Just 2
    VonNeumann 0 0 0 0 2 -> Just 0
    VonNeumann 0 0 0 0 3 -> Just 0
    VonNeumann 0 0 0 0 5 -> Just 0
    VonNeumann 0 0 0 0 6 -> Just 3
    VonNeumann 0 0 0 0 7 -> Just 1
    VonNeumann 0 0 0 1 1 -> Just 2
    VonNeumann 0 0 0 1 2 -> Just 2
    VonNeumann 0 0 0 1 3 -> Just 2
    VonNeumann 0 0 0 2 1 -> Just 2
    VonNeumann 0 0 0 2 2 -> Just 0
    VonNeumann 0 0 0 2 3 -> Just 0
    VonNeumann 0 0 0 2 6 -> Just 2
    VonNeumann 0 0 0 2 7 -> Just 2
    VonNeumann 0 0 0 3 2 -> Just 0
    VonNeumann 0 0 0 5 2 -> Just 5
    VonNeumann 0 0 0 6 2 -> Just 2
    VonNeumann 0 0 0 7 2 -> Just 2
    VonNeumann 0 0 1 0 2 -> Just 2
    VonNeumann 0 0 1 1 2 -> Just 0
    VonNeumann 0 0 2 0 2 -> Just 0
    VonNeumann 0 0 2 0 3 -> Just 0
    VonNeumann 0 0 2 0 5 -> Just 0
    VonNeumann 0 0 2 1 2 -> Just 5
    VonNeumann 0 0 2 2 2 -> Just 0
    VonNeumann 0 0 2 3 2 -> Just 2
    VonNeumann 0 0 5 2 2 -> Just 2
    VonNeumann 0 1 2 3 2 -> Just 1
    VonNeumann 0 1 2 4 2 -> Just 1
    VonNeumann 0 1 2 5 2 -> Just 5
    VonNeumann 0 1 2 6 2 -> Just 1
    VonNeumann 0 1 2 7 2 -> Just 1
    VonNeumann 0 1 2 7 5 -> Just 1
    VonNeumann 0 1 4 2 2 -> Just 1
    VonNeumann 0 1 4 3 2 -> Just 1
    VonNeumann 0 1 4 4 2 -> Just 1
    VonNeumann 0 1 4 7 2 -> Just 1
    VonNeumann 0 1 6 2 5 -> Just 1
    VonNeumann 0 1 7 2 2 -> Just 1
    VonNeumann 0 1 7 2 5 -> Just 5
    VonNeumann 0 1 7 5 2 -> Just 1
    VonNeumann 0 1 7 6 2 -> Just 1
    VonNeumann 0 1 7 7 2 -> Just 1
    VonNeumann 0 2 5 2 7 -> Just 1
    VonNeumann 1 0 0 0 1 -> Just 1
    VonNeumann 1 0 0 0 6 -> Just 1
    VonNeumann 1 0 0 0 7 -> Just 7
    VonNeumann 1 0 0 1 1 -> Just 1
    VonNeumann 1 0 0 1 2 -> Just 1
    VonNeumann 1 0 0 2 1 -> Just 1
    VonNeumann 1 0 0 2 4 -> Just 4
    VonNeumann 1 0 0 2 7 -> Just 7
    VonNeumann 1 0 0 5 1 -> Just 1
    VonNeumann 1 0 1 0 1 -> Just 1
    VonNeumann 1 0 1 1 1 -> Just 1
    VonNeumann 1 0 1 2 4 -> Just 4
    VonNeumann 1 0 1 2 7 -> Just 7
    VonNeumann 1 0 2 0 2 -> Just 6
    VonNeumann 1 0 2 1 2 -> Just 1
    VonNeumann 1 0 2 2 1 -> Just 1
    VonNeumann 1 0 2 2 4 -> Just 4
    VonNeumann 1 0 2 2 6 -> Just 3
    VonNeumann 1 0 2 2 7 -> Just 7
    VonNeumann 1 0 2 3 2 -> Just 7
    VonNeumann 1 0 2 4 2 -> Just 4
    VonNeumann 1 0 2 6 2 -> Just 6
    VonNeumann 1 0 2 6 4 -> Just 4
    VonNeumann 1 0 2 6 7 -> Just 7
    VonNeumann 1 0 2 7 1 -> Just 0
    VonNeumann 1 0 2 7 2 -> Just 7
    VonNeumann 1 0 5 4 2 -> Just 7
    VonNeumann 1 1 1 1 2 -> Just 1
    VonNeumann 1 1 1 2 2 -> Just 1
    VonNeumann 1 1 1 2 4 -> Just 4
    VonNeumann 1 1 1 2 5 -> Just 1
    VonNeumann 1 1 1 2 6 -> Just 1
    VonNeumann 1 1 1 2 7 -> Just 7
    VonNeumann 1 1 1 5 2 -> Just 2
    VonNeumann 1 1 2 1 2 -> Just 1
    VonNeumann 1 1 2 2 2 -> Just 1
    VonNeumann 1 1 2 2 4 -> Just 4
    VonNeumann 1 1 2 2 5 -> Just 1
    VonNeumann 1 1 2 2 7 -> Just 7
    VonNeumann 1 1 2 3 2 -> Just 1
    VonNeumann 1 1 2 4 2 -> Just 4
    VonNeumann 1 1 2 6 2 -> Just 1
    VonNeumann 1 1 2 7 2 -> Just 7
    VonNeumann 1 1 3 2 2 -> Just 1
    VonNeumann 1 2 2 2 4 -> Just 4
    VonNeumann 1 2 2 2 7 -> Just 7
    VonNeumann 1 2 2 4 3 -> Just 4
    VonNeumann 1 2 2 5 4 -> Just 7
    VonNeumann 1 2 3 2 4 -> Just 4
    VonNeumann 1 2 3 2 7 -> Just 7
    VonNeumann 1 2 4 2 5 -> Just 5
    VonNeumann 1 2 4 2 6 -> Just 7
    VonNeumann 1 2 5 2 7 -> Just 5
    VonNeumann 2 0 0 0 1 -> Just 2
    VonNeumann 2 0 0 0 2 -> Just 2
    VonNeumann 2 0 0 0 4 -> Just 2
    VonNeumann 2 0 0 0 7 -> Just 1
    VonNeumann 2 0 0 1 2 -> Just 2
    VonNeumann 2 0 0 1 5 -> Just 2
    VonNeumann 2 0 0 2 1 -> Just 2
    VonNeumann 2 0 0 2 2 -> Just 2
    VonNeumann 2 0 0 2 3 -> Just 2
    VonNeumann 2 0 0 2 4 -> Just 2
    VonNeumann 2 0 0 2 5 -> Just 0
    VonNeumann 2 0 0 2 6 -> Just 2
    VonNeumann 2 0 0 2 7 -> Just 2
    VonNeumann 2 0 0 3 2 -> Just 6
    VonNeumann 2 0 0 4 2 -> Just 3
    VonNeumann 2 0 0 5 1 -> Just 7
    VonNeumann 2 0 0 5 2 -> Just 2
    VonNeumann 2 0 0 5 7 -> Just 5
    VonNeumann 2 0 0 7 2 -> Just 2
    VonNeumann 2 0 1 0 2 -> Just 2
    VonNeumann 2 0 1 1 2 -> Just 2
    VonNeumann 2 0 1 2 2 -> Just 2
    VonNeumann 2 0 1 4 2 -> Just 2
    VonNeumann 2 0 1 7 2 -> Just 2
    VonNeumann 2 0 2 0 2 -> Just 2
    VonNeumann 2 0 2 0 3 -> Just 2
    VonNeumann 2 0 2 0 5 -> Just 2
    VonNeumann 2 0 2 0 7 -> Just 3
    VonNeumann 2 0 2 1 2 -> Just 2
    VonNeumann 2 0 2 1 5 -> Just 2
    VonNeumann 2 0 2 2 1 -> Just 2
    VonNeumann 2 0 2 2 2 -> Just 2
    VonNeumann 2 0 2 2 7 -> Just 2
    VonNeumann 2 0 2 3 2 -> Just 1
    VonNeumann 2 0 2 4 2 -> Just 2
    VonNeumann 2 0 2 4 5 -> Just 2
    VonNeumann 2 0 2 5 2 -> Just 0
    VonNeumann 2 0 2 5 5 -> Just 2
    VonNeumann 2 0 2 6 2 -> Just 2
    VonNeumann 2 0 2 7 2 -> Just 2
    VonNeumann 2 0 3 1 2 -> Just 2
    VonNeumann 2 0 3 2 1 -> Just 6
    VonNeumann 2 0 3 2 2 -> Just 6
    VonNeumann 2 0 3 4 2 -> Just 2
    VonNeumann 2 0 4 2 2 -> Just 2
    VonNeumann 2 0 5 1 2 -> Just 2
    VonNeumann 2 0 5 2 1 -> Just 2
    VonNeumann 2 0 5 2 2 -> Just 2
    VonNeumann 2 0 5 5 2 -> Just 1
    VonNeumann 2 0 5 7 2 -> Just 5
    VonNeumann 2 0 6 2 2 -> Just 2
    VonNeumann 2 0 6 7 2 -> Just 2
    VonNeumann 2 0 7 1 2 -> Just 2
    VonNeumann 2 0 7 2 2 -> Just 2
    VonNeumann 2 0 7 4 2 -> Just 2
    VonNeumann 2 0 7 7 2 -> Just 2
    VonNeumann 2 1 1 2 2 -> Just 2
    VonNeumann 2 1 1 2 6 -> Just 1
    VonNeumann 2 1 2 2 2 -> Just 2
    VonNeumann 2 1 2 2 4 -> Just 2
    VonNeumann 2 1 2 2 6 -> Just 2
    VonNeumann 2 1 2 2 7 -> Just 2
    VonNeumann 2 1 4 2 2 -> Just 2
    VonNeumann 2 1 5 2 2 -> Just 2
    VonNeumann 2 1 6 2 2 -> Just 2
    VonNeumann 2 1 7 2 2 -> Just 2
    VonNeumann 2 2 2 2 7 -> Just 2
    VonNeumann 2 2 2 4 4 -> Just 2
    VonNeumann 2 2 2 4 6 -> Just 2
    VonNeumann 2 2 2 7 6 -> Just 2
    VonNeumann 2 2 2 7 7 -> Just 2
    VonNeumann 3 0 0 0 1 -> Just 3
    VonNeumann 3 0 0 0 2 -> Just 2
    VonNeumann 3 0 0 0 4 -> Just 1
    VonNeumann 3 0 0 0 7 -> Just 6
    VonNeumann 3 0 0 1 2 -> Just 3
    VonNeumann 3 0 0 4 2 -> Just 1
    VonNeumann 3 0 0 6 2 -> Just 2
    VonNeumann 3 0 1 0 2 -> Just 1
    VonNeumann 3 0 1 2 2 -> Just 0
    VonNeumann 3 0 2 5 1 -> Just 1
    VonNeumann 4 0 1 1 2 -> Just 0
    VonNeumann 4 0 1 2 2 -> Just 0
    VonNeumann 4 0 1 2 5 -> Just 0
    VonNeumann 4 0 2 1 2 -> Just 0
    VonNeumann 4 0 2 2 2 -> Just 1
    VonNeumann 4 0 2 3 2 -> Just 6
    VonNeumann 4 0 2 5 2 -> Just 0
    VonNeumann 4 0 3 2 2 -> Just 1
    VonNeumann 5 0 0 0 2 -> Just 2
    VonNeumann 5 0 0 2 1 -> Just 5
    VonNeumann 5 0 0 2 2 -> Just 5
    VonNeumann 5 0 0 2 3 -> Just 2
    VonNeumann 5 0 0 2 7 -> Just 2
    VonNeumann 5 0 0 5 2 -> Just 0
    VonNeumann 5 0 2 0 2 -> Just 2
    VonNeumann 5 0 2 1 2 -> Just 2
    VonNeumann 5 0 2 1 5 -> Just 2
    VonNeumann 5 0 2 2 2 -> Just 0
    VonNeumann 5 0 2 2 4 -> Just 4
    VonNeumann 5 0 2 7 2 -> Just 2
    VonNeumann 5 1 2 1 2 -> Just 2
    VonNeumann 5 1 2 2 2 -> Just 0
    VonNeumann 5 1 2 4 2 -> Just 2
    VonNeumann 5 1 2 7 2 -> Just 2
    VonNeumann 6 0 0 0 1 -> Just 1
    VonNeumann 6 0 0 0 2 -> Just 1
    VonNeumann 6 0 2 1 2 -> Just 0
    VonNeumann 6 1 2 1 2 -> Just 5
    VonNeumann 6 1 2 1 3 -> Just 1
    VonNeumann 6 1 2 2 2 -> Just 5
    VonNeumann 7 0 0 0 7 -> Just 7
    VonNeumann 7 0 1 1 2 -> Just 0
    VonNeumann 7 0 1 2 2 -> Just 0
    VonNeumann 7 0 1 2 5 -> Just 0
    VonNeumann 7 0 2 1 2 -> Just 0
    VonNeumann 7 0 2 2 2 -> Just 1
    VonNeumann 7 0 2 2 5 -> Just 1
    VonNeumann 7 0 2 3 2 -> Just 1
    VonNeumann 7 0 2 5 2 -> Just 5
    VonNeumann 7 0 2 7 2 -> Just 0
    _                    -> Nothing



----------------



stateTransitions :: Map.Map VonNeumann Word8
stateTransitions = Map.fromList stateTransitions'

stateTransitions' :: [(VonNeumann, Word8)]
stateTransitions' = concat (w8col1 ++ w8col2 ++ w8col3 ++ w8col4 ++ w8col5)
  where
    toVon i8s =
      case fmap fromIntegral i8s of
        (c:t:r:b:l:_:_:[i]) ->
          [ (VonNeumann c t r b l, i)
          -- , (VonNeumann c r b l t, i)
          -- , (VonNeumann c b l t r, i)
          -- , (VonNeumann c l t r b, i)
          ]
        _ -> error $ "Invalid pattern: " ++ show i8s
    w8states = fmap (fmap (fmap (subtract 48 . ord))) stateTransitionsStr
    w8col1 = fmap (toVon . head) w8states
    w8col2 = fmap (toVon . head . tail) w8states
    w8col3 = fmap (toVon . head . tail . tail) w8states
    w8col4 = fmap (toVon . head . tail . tail . tail) w8states
    w8col5 = toVon <$> concatMap (tail . tail . tail . tail) w8states


stateTransitionsStr :: [[String]]
stateTransitionsStr =
  [ ["00000->0", "02527->1", "11322->1", "20242->2", "30102->1"]
  , ["00001->2", "10001->1", "12224->4", "20245->2", "30122->0"]
  , ["00002->0", "10006->1", "12227->7", "20252->0", "30251->1"]
  , ["00003->0", "10007->7", "12243->4", "20255->2", "40112->0"]
  , ["00005->0", "10011->1", "12254->7", "20262->2", "40122->0"]
  , ["00006->3", "10012->1", "12324->4", "20272->2", "40125->0"]
  , ["00007->1", "10021->1", "12327->7", "20312->2", "40212->0"]
  , ["00011->2", "10024->4", "12425->5", "20321->6", "40222->1"]
  , ["00012->2", "10027->7", "12426->7", "20322->6", "40232->6"]
  , ["00013->2", "10051->1", "12527->5", "20342->2", "40252->0"]
  , ["00021->2", "10101->1", "20001->2", "20422->2", "40322->1"]
  , ["00022->0", "10111->1", "20002->2", "20512->2", "50002->2"]
  , ["00023->0", "10124->4", "20004->2", "20521->2", "50021->5"]
  , ["00026->2", "10127->7", "20007->1", "20522->2", "50022->5"]
  , ["00027->2", "10202->6", "20012->2", "20552->1", "50023->2"]
  , ["00032->0", "10212->1", "20015->2", "20572->5", "50027->2"]
  , ["00052->5", "10221->1", "20021->2", "20622->2", "50052->0"]
  , ["00062->2", "10224->4", "20022->2", "20672->2", "50202->2"]
  , ["00072->2", "10226->3", "20023->2", "20712->2", "50212->2"]
  , ["00102->2", "10227->7", "20024->2", "20722->2", "50215->2"]
  , ["00112->0", "10232->7", "20025->0", "20742->2", "50222->0"]
  , ["00202->0", "10242->4", "20026->2", "20772->2", "50224->4"]
  , ["00203->0", "10262->6", "20027->2", "21122->2", "50272->2"]
  , ["00205->0", "10264->4", "20032->6", "21126->1", "51212->2"]
  , ["00212->5", "10267->7", "20042->3", "21222->2", "51222->0"]
  , ["00222->0", "10271->0", "20051->7", "21224->2", "51242->2"]
  , ["00232->2", "10272->7", "20052->2", "21226->2", "51272->2"]
  , ["00522->2", "10542->7", "20057->5", "21227->2", "60001->1"]
  , ["01232->1", "11112->1", "20072->2", "21422->2", "60002->1"]
  , ["01242->1", "11122->1", "20102->2", "21522->2", "60212->0"]
  , ["01252->5", "11124->4", "20112->2", "21622->2", "61212->5"]
  , ["01262->1", "11125->1", "20122->2", "21722->2", "61213->1"]
  , ["01272->1", "11126->1", "20142->2", "22227->2", "61222->5"]
  , ["01275->1", "11127->7", "20172->2", "22244->2", "70007->7"]
  , ["01422->1", "11152->2", "20202->2", "22246->2", "70112->0"]
  , ["01432->1", "11212->1", "20203->2", "22276->2", "70122->0"]
  , ["01442->1", "11222->1", "20205->2", "22277->2", "70125->0"]
  , ["01472->1", "11224->4", "20207->3", "30001->3", "70212->0"]
  , ["01625->1", "11225->1", "20212->2", "30002->2", "70222->1"]
  , ["01722->1", "11227->7", "20215->2", "30004->1", "70225->1"]
  , ["01725->5", "11232->1", "20221->2", "30007->6", "70232->1"]
  , ["01752->1", "11242->4", "20222->2", "30012->3", "70252->5"]
  , ["01762->1", "11262->1", "20227->2", "30042->1", "70272->0"]
  , ["01772->1", "11272->7", "20232->1", "30062->2"]
  ]


toCase :: [(VonNeumann, Word8)] -> String
toCase ss =
  "case s of\n" ++ unlines ["  " ++ show vn ++ " -> " ++ show w8 | (vn, w8) <- ss]
