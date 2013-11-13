{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

-- base, mtl, array, containers, random, monad-loops, ncurses, astar, hfov
import Control.Monad.State
import Data.Array.IArray
import Data.Array.IO
import Data.List (sortBy, find)
import Data.Ord (comparing)
import Data.Char
import qualified Data.Set as Set
import Data.Maybe
import System.Random
import qualified Data.List.Zipper as Z
import Control.Monad.Loops (iterateUntil)
import Data.Graph.AStar (aStar)
import UI.NCurses
import FOV

type Vector2     = (Int, Int)
type Room        = (Vector2, Vector2) -- xy, size
type TileMapPart = [(Vector2, Char)] -- update for array 
type TileMap     = Array Vector2 Char
type Neighbours  = Array Vector2 (Set.Set Vector2)

newtype GameT s m a = GameT (StateT s m a)
    deriving (Functor, Monad, MonadIO, MonadTrans, MonadState s)

type Game = GameT GameState Curses

runGame1 :: GameState -> Game a -> Curses GameState
runGame1 s (GameT f) = execStateT f s

io :: IO a -> Game a
io = liftIO

curses :: Curses a -> Game a
curses = lift

data Side = T | B | L | R -- room sides

data ItemType = Weapon -- '|'
              | Armor  -- '['
              | Potion -- '!'
              | Scroll -- '?'
              | Corpse -- '%'
              | Food   -- ','
              deriving (Eq, Show, Read)

data ItemEffect = None
                | Healing Int
                | Death
                | Teleport
                | Yuck
                | Transmute
                deriving (Eq, Show, Read)

-- TODO: use this
data EntityStats = EntityStats
    { strength     :: Int
    , agility      :: Int
    , intelligence :: Int
    , beauty       :: Int
    } deriving (Eq, Show, Read)

data Item = Item
    { iType    :: ItemType
    , iName    :: String
    , iSpeed   :: Int
    , iDamage  :: Int
    , iDefence :: Int
    , iWeight  :: Int -- mass in grams
    , iEffect  :: ItemEffect
    } deriving (Eq, Show, Read)

data Inventory = Inventory
    { equWeapon :: Item
    , equArmor  :: Item
    , equMisc   :: Item
    , storedItems :: [Item]
    } deriving (Eq, Show, Read)

data Entity = Entity
    { pc       :: Bool
    , pos      :: Vector2
    , sym      :: Char
    , name     :: String
    , hp       :: (Int, Int)
    , inv      :: Inventory
    , stats    :: EntityStats
    , weight   :: Int -- mass in grams
    , speed    :: Int
    , nextMove :: Int
    , seenL    :: Array Vector2 Bool -- ^what the player has seen of the level
    , seeingL  :: Array Vector2 Bool -- ^what the player *currently* sees
    } deriving (Eq, Show, Read)

data Level = Level 
    { tilemap  :: TileMap
    , stairs   :: (Vector2, Vector2) -- up and down stairs
    , entities :: [Entity]
    , items    :: [(Vector2, Item)]
    , narr     :: Neighbours
    } deriving (Eq, Show, Read)

data GameState = GameState
    { levels   :: Z.Zipper Level
    , msgs     :: [(Int, Bool, String)] -- ^ turn added, seen by player, message
    , turn     :: Int
    , pquit    :: Bool
    , dlvl     :: Int
    } deriving (Eq, Show)

cur :: Z.Zipper Level -> Level
cur = Z.cursor

-- | get the next actor and the rest
entityPQ :: [Entity] -> (Entity, [Entity])
entityPQ en = 
  let (p:ren) = sortBy (comparing nextMove) en 
  in (p, ren)

-- | get the player character and the rest
getPC :: [Entity] -> (Entity, [Entity])
getPC en = 
  let l = sortBy (comparing pc) en
  in (last l, init l)

-- | check if the Entity is the player character
isPC :: Entity -> Bool
isPC = pc

-- empty level
emptyL :: TileMap
emptyL = listArray ((1,1), (80, 22)) (repeat ' ')

-- enemies don't use seenL/seeingL, so use this dummy array
nullA :: Array Vector2 Bool
nullA = listArray ((1,1), (2,2)) (repeat False)

testWeapons :: [Item]
testWeapons = [Item Weapon "Sword of Test +1" 100 4 0 1500 None
              ,Item Weapon "Shard of Glass" 150 2 0 300 None
              ,Item Weapon "Staff of Skepticism" 50 10 0 4000 None
              ]

testArmors :: [Item]
testArmors = [Item Armor "Tested Leather Armor" 100 0 2 5000 None
             ,Item Armor "Fancy Suit" 150 0 1 2000 None
             ,Item Armor "Power Armor mk2" 66 0 6 30000 None
             ]

testMisc :: [Item]
testMisc = [Item Scroll "Scroll of Modern Medicine" 100 0 0 100 (Healing 6)
           ,Item Potion "Potion of Alcohol" 100 0 0 700 (Healing (-1))
           ,Item Potion "Homeopathic Potion" 100 0 0 500 (Healing 0)
           ,Item Scroll "Scroll of Death" 100 0 0 100 Death
           ,Item Scroll "Book of Teleportation" 100 0 0 1000 Teleport
           ,Item Corpse "Rodent Corpse" 100 0 0 1000 Yuck
           ,Item Scroll "Book of Transmutation" 100 0 0 1000 Transmute
           ]

noWeapon, noArmor, noMisc :: Item
noWeapon = Item Weapon "Prosthetic Fists" 100 1 0 0 None
noArmor  = Item Armor "Wizard's Cape" 100 0 0 0 None
noMisc   = Item Scroll "New York Times Magazine" 100 0 0 0 None

defaultStats :: EntityStats
defaultStats = EntityStats
  { strength = 8
  , agility = 8
  , intelligence = 8
  , beauty = 8
  }

testEnemies :: [Entity]
testEnemies = 
  [Entity False (0,0) 'L' "Lamar" (5,5) undefined defaultStats 
   30000 50 0 nullA nullA
  ,Entity False (0,0) 'A' "Giant Ant" (7,7) undefined defaultStats 
   10000 99 0 nullA nullA
  ,Entity False (0,0) 'm' "Mom" (8,8) undefined defaultStats 
   60000 101 0 nullA nullA
  ,Entity False (0,0) 'b' "Bear" (13,13) undefined defaultStats 
   120000 120 0 nullA nullA
  ,Entity False (0,0) 'd' "Dog" (3,3) undefined defaultStats 
   8000 95 0 nullA nullA
  ,Entity False (0,0) 'a' "Armadillo" (1,1) undefined defaultStats 
   1000 85 0 nullA nullA
  ,Entity False (0,0) 'E' "Etsilopp" (10,10) undefined defaultStats 
   100000 100 0 nullA nullA
  ]

testBoss :: Entity
testBoss = 
  Entity False (0,0) 'G' "Dreadlord Gates" (32,32) 
   undefined defaultStats 75000 135 0 nullA nullA

testItems :: [Item]
testItems = testWeapons ++ testArmors ++ testMisc

randInv :: IO Inventory
randInv = do
  w <- randomElem testWeapons
  a <- randomElem testArmors
  m <- randomElem testMisc
  return Inventory { equWeapon = w
                   , equArmor  = a
                   , equMisc   = m
                   , storedItems = [] }

-- to save disk space, the neighbours array could be discarded here
-- and then rebuilt on load
{-
save :: Game ()
save = get >>= io . writeFile "savefile" . show

load :: Game ()
load = io (readFile "saveFile") >>= io . readIO >>= put
-}

mkPlayer :: Vector2 -> IO Entity
mkPlayer pos' =
--  pinv <- randInv
  return Entity 
         { pc       = True
         , name     = "Player"
         , pos      = pos'
         , sym      = '@'
         , hp       = (20, 20)
         , inv      = Inventory noWeapon noArmor noMisc [] 
         , stats    = defaultStats
         , weight   = 75000
         , speed    = 100
         , nextMove = 0
         , seenL    = listArray ((1,1), (80,22)) (repeat False)
         , seeingL  = listArray ((1,1), (80,22)) (repeat False)
         }

-- max carry weight is 5kg per strength point
maxCarryWeight :: Entity -> Int
maxCarryWeight e = 5000 * (strength (stats e))

mkEnemiesOnLevel :: Int -- number of enemies
                 -> Int -- nextMove of enemies, set to the nextMove of the player
                 -- when descending the dungeon
                 -> TileMap
                 -> IO [Entity]
mkEnemiesOnLevel dl nm l = do
  en <- randomRIO (1 * dl, 3 * dl) -- amount of enemies
  es <- randFloors l en            -- positions
  ei <- replicateM en randInv      -- inventories
  em <- replicateM en (randomElem testEnemies) -- random enemies
  ep <- randFloor l                -- position of boss, maybe
  bi <- randInv                    -- inventory of boss, maybe
  return $
    -- add the boss on the fifth level
    [testBoss { pos = ep, inv = bi, nextMove = nm } | dl == 5] ++ 
    -- add normal enemies
    zipWith3 (\e p i -> e { pos = p, inv = i, nextMove = nm }) em es ei

mkItemsOnLevel :: TileMap -> IO [(Vector2, Item)]
mkItemsOnLevel l = do
  i  <- randomRIO (3,9)
  is <- replicateM i (randomElem testItems)
  ip <- randFloors l i
  return (zip ip is)

-- | make a random pair within given bounds
randomV2 :: (Vector2, Vector2) -> IO Vector2
randomV2 ((x1, y1), (x2, y2)) = do
  x <- randomRIO (x1, x2)
  y <- randomRIO (y1, y2)
  return (x,y)

isFloor :: TileMap -> Vector2 -> Bool
isFloor l p = l ! p `elem` ".⋅"

-- | find a random part of the level that's walkable
randFloor :: TileMap -> IO Vector2
randFloor l = randomV2 (bounds l) `satisfying` isFloor l

-- | find n non-repeating random positions that are walkable
randFloors :: TileMap -> Int -> IO [Vector2]
randFloors l n = do
  let floorTiles = map fst $ filter (\(_,c) -> c `elem` ".⋅") $ assocs l
  when (length floorTiles <= n) $
    error "randFloors: given level had fewer floor tiles than were requested"
  flip execStateT [] $
    replicateM_ n $ do
      ps' <- get
      xy  <- liftIO $ randomElem floorTiles `satisfying` (`notElem` ps')
      modify (xy:)

-- | check if two rooms overlap
roomOverlap :: Room -> Room -> Bool
roomOverlap ((x1, y1), (sx1, sy1)) ((x2, y2), (sx2, sy2)) =
  not (x1 > x2 + sx2 + 1
     ||y1 > y2 + sy2 + 1
     ||x2 > x1 + sx1 + 1
     ||y2 > y2 + sy1 + 1)

-- | check if the given room overlaps any of the rooms in the list
roomOverlapAny :: Room -> [Room] -> Bool
roomOverlapAny r = any (roomOverlap r)

-- | create a room without checking if it's within level bounds
createRoom :: Vector2 -> IO Room
createRoom (sx, sy) = do
  -- all rooms need to be at least 2 tiles away from the level bounds
  -- 1 tile to make room for any paths, and +1 tile because FOV crashes
  -- if it tries to look at nothingness.
  x  <- randomRIO (3, sx - 3)
  y  <- randomRIO (3, sy - 3)
  rx <- randomRIO (2, sx `div` 2)
  ry <- randomRIO (2, sy `div` 2)
  return ((x, y), (rx, ry))

-- | check if a room is within (x2, y2) bounds
goodRoom :: Vector2 -> Room -> Bool
goodRoom (sx, sy) ((x, y), (rx, ry)) = x + rx < sx - 3 && y + ry < sy - 3

-- | try really hard to make a servicable dungeon
mkRandRooms :: Vector2 -> IO TileMap
mkRandRooms lbounds = do
  -- try 1000 times to create good non-overlapping rooms
  rooms <- flip execStateT [] $
    replicateM_ 1000 $ do
      rooms <- get
      r <- liftIO (createRoom lbounds) `satisfying` goodRoom lbounds
      unless (r `roomOverlapAny` rooms) (modify (r:))

  when (null rooms)
    (error "dungeon generator mkRandRooms didn't make any rooms")

  let room_lps = mkRooms rooms :: TileMapPart
      narrP    = mkNeighbourArray (emptyL // room_lps) " "

  paths1 <- connectRandomRoomToAll narrP rooms
  paths2 <- connectRandomRooms narrP rooms (length rooms `div` 2)

  return $ emptyL // (room_lps ++ paths1 ++ paths2)

-- | connect a random room from the list to all rooms. Cheap way to
-- ensure that all rooms are connected.
connectRandomRoomToAll :: Neighbours -> [Room] -> IO TileMapPart
connectRandomRoomToAll narrP rooms = do
  rc <- randomElem rooms
  concat `fmap` catMaybes `fmap` forM rooms (\r ->
    if r /= rc
      then connectRooms narrP r rc
      else return Nothing)

-- | make n random connections between rooms
connectRandomRooms :: Neighbours -> [Room] -> Int -> IO TileMapPart
connectRandomRooms narrP rooms n =
  concat `fmap` catMaybes `fmap` replicateM n
    (do r1 <- randomElem rooms
        r2 <- randomElem rooms
        if r1 /= r2 then connectRooms narrP r1 r2
                    else return Nothing)

-- | connect two rooms, using corridors generated by A* pathfinding.
-- Randomly picks a side, so all rooms must have one tile free space
-- around them.
connectRooms :: Neighbours -> Room -> Room -> IO (Maybe TileMapPart)
connectRooms narrP r1 r2 = do
  side1 <- randomRIO (1, 4::Int)
  side2 <- randomRIO (1, 4::Int)
  let (f1, r1p') =
        case side1 of
         1 -> (above, roomSideR r1 T)
         2 -> (below, roomSideR r1 B)
         3 -> (left,  roomSideR r1 L)
         4 -> (right, roomSideR r1 R)
         _ -> error "wait, what?"
      (f2, r2p') =
        case side2 of
         1 -> (above, roomSideR r2 T)
         2 -> (below, roomSideR r2 B)
         3 -> (left,  roomSideR r2 L)
         4 -> (right, roomSideR r2 R)
         _ -> error "you're drunk; go home"
  r1p <- r1p'
  r2p <- r2p'
  return $ mkPath narrP r1p r2p f1 f2

-- | produce a random tile location from given side of the given room
roomSideR :: Room -> Side -> IO Vector2
roomSideR ((x1, y1), (sx, sy)) s =
  case s of
    T -> randomRIO (x1+1, x1+sx-1) >>= \x -> return (x, y1)
    B -> randomRIO (x1+1, x1+sx-1) >>= \x -> return (x, y1+sy)
    R -> randomRIO (y1+1, y1+sy-1) >>= \y -> return (x1+sx, y)
    L -> randomRIO (y1+1, y1+sy-1) >>= \y -> return (x1, y)

-- utils for Data.Graph.AStar
-- | distance between two level points (taxi metric)
distance :: Vector2 -> Vector2 -> Int
distance (a1,a2) (b1, b2) =
    (b1 - a1) + (b2 - a2)

maybeLookup :: TileMap -> Vector2 -> Maybe Char
maybeLookup l p@(x,y) =
  let ((x1, y1), (x2, y2)) = bounds l
   in if x > x2 || x < x1 || y > y2 || y < y1
       then Just (l ! p)
       else Nothing

-- | create a array of the sets of neighbours for each tile
mkNeighbourArray :: TileMap -> String -> Neighbours
mkNeighbourArray l tt =
  let (a@(x1,y1), b@(x2,y2)) = bounds l
   in listArray (a, b) [setOfNeighbours l tt (x,y) |
                        x <- [x1..x2],
                        y <- [y1..y2]]

-- | create a set of all neighbours of a point that are walkable
setOfNeighbours :: TileMap -> String -> Vector2 -> Set.Set Vector2
setOfNeighbours l tt p =
  Set.fromList
  . map fst
  . filter (\(p',_) -> l ! p' `elem` tt)
  . filter (\(_,m) -> isNothing m) $
  [ (above p, l `maybeLookup` above p)
  , (below p, l `maybeLookup` below p)
  , (right p, l `maybeLookup` right p)
  , (left  p, l `maybeLookup` left p)]

-- | aStar with defaults
simpleAStar :: Neighbours
            -> Vector2
            -> Vector2
            -> Maybe [Vector2]
simpleAStar narrP start goal =
   let adjdist _ _ = 1
       nset p = narrP ! p
       dist = distance goal
   in aStar nset adjdist dist (==goal) start

-- | make a path between two rooms
mkPath :: Neighbours
       -> Vector2   -- ^ from
       -> Vector2   -- ^ to
       -> (Vector2 -> Vector2)
       -- pass above/below/right/left here, because if we're making
       -- a path from a room, the first tile and last tiles are
       -- impassable walls
       -> (Vector2 -> Vector2)
       -> Maybe TileMapPart
mkPath narrP p1 p2 f1 f2 = do
  p' <- simpleAStar narrP (f1 p1) (f2 p2)
  return $ zip p' (repeat '#') ++ [(f1 p1, '#'), (p1,'+'), (p2,'+')]

-- level construction
mkHwall :: Char -> Int -> Vector2 -> TileMapPart
mkHwall ch len (x,y) = zip (zip [x..(x-1+len)] (repeat y)) (repeat ch)

mkVwall :: Char -> Int -> Vector2 -> TileMapPart
mkVwall ch len (x,y) = zip (zip (repeat x) [y..(y-1+len)]) (repeat ch)

-- borders:
-- ┘ ┐ ┌ └ │ ─ ├  ┬  ┤  ┴  ┼ ╭ ╮ ╯ ╰
-- ═ ║ ╒ ╓ ╔ ╕ ╖ ╗ ╘ ╙ ╚ ╛ ╜ ╝ ╞ ╟
-- ╠ ╡ ╢ ╣ ╤ ╥ ╦ ╧ ╨ ╩ ╪ ╫ ╬
mkRoom :: Room -> TileMapPart
mkRoom ((x1, y1), (sx, sy)) = concat
  -- floors
  [[((x,y), '⋅') | y <- [y1..(y1+sy)], x <- [x1..(x1+sx)]]
  -- walls
  ,mkHwall '═' sx (x1, y1)
  ,mkHwall '═' sx (x1, y1 + sy)
  ,mkVwall '║' (sy+1) (x1, y1)
  ,mkVwall '║' (sy+1) (x1 + sx, y1)
  -- corners
  ,[((x1, y1),    '╔'), ((x1+sx, y1+sy), '╝')
  ,((x1,  y1+sy), '╚'), ((x1+sx, y1),    '╗')]]

mkRooms :: [Room] -> TileMapPart
mkRooms = concatMap mkRoom

walkableL :: TileMap -> Vector2 -> Bool
walkableL l p@(x,y) =
  let ((x1,y1), (x2,y2)) = bounds l
   in (x < x2 || x > x1 || y < y2 || y > y1) && (l ! p `elem` ".⋅+#SDk>")

-- TODO: this needs to be fixed to account for messages over 75 chars
displayMessageLog :: Game ()
displayMessageLog = do
  g <- get
  let bs@(_, (x2,y2)) = bounds (tilemap (Z.cursor (levels g)))
      ms = take (y2 -5) (msgs g)
      pp = zip [3..] ms
  curses $ do
    draw $ do
      clear bs
      drawStringAt (1,1) "Message log: ('p' to return)"
      forM_ pp $ \(y, (t, _, m)) ->
        drawStringAt (1, y) $ take (x2 - 5) "[" ++ show t ++ "] " ++ m
    _ <- waitForChrs "p"
    draw $ clear bs

helpString :: [String]
helpString = ["Hello, hoodie here (press ? again to return)"
             ,""
             ,"Move with the Vi keys: hijkyubn"
             ,"Attack by moving into enemies"
             ,"Wait for a turn by pressing ."
             ,"i to look at your inventory"
             ,"e/d - equip drop"
             ,"a to apply your misc item"
             ,", to pick up items below you"
             ,"Descend to the next level with > when you are on stairs"
             ,"Press p to display the message log,"
             ,""
             ,"q to quit, coward."]

displayHelp :: Game ()
displayHelp = do
  bs <- bounds `fmap` tilemap `fmap` Z.cursor `fmap` gets levels
  curses $ do
    draw $ do
      clear bs
      forM_ [1..length helpString] $ \y ->
        drawStringAt (1, y) $ center 80 (helpString !! (y-1))
    _ <- waitForChrs "?"
    return ()

displayPlayerInventory :: Game ()
displayPlayerInventory = do
  l <- Z.cursor `fmap` gets levels
  
  let p       = fst (getPC (entities l))
      iItems  = zip [0..] (storedItems (inv p))
      w       = equWeapon (inv p)
      a       = equArmor (inv p)
      m       = equMisc (inv p)
      letters = ['a'..]
  
  curses $ do
    draw $ do
      clear (bounds (tilemap l))
      let cw = sum $ map iWeight (w:a:m:storedItems (inv p))
      let mw = maxCarryWeight p
      drawStringAt (1,1) $ 
        "Inventory. (current weight: " ++ show cw ++ " max: " ++ show mw ++ ")"
      drawStringAt (1,2) "Items:"
      drawStringAt (1,3) $ " [Wielded] " ++ iName w
      drawStringAt (1,4) $ " [Worn] "    ++ iName a
      drawStringAt (1,5) $ " [Active] "  ++ iName m
      forM_ iItems $ \(y, i) ->
        drawStringAt (1, y + 7) $ "  " ++ letters !! y : ". " ++ iName i
    render
    _ <- waitForChrs $ ' ':['A'..'z']
    return ()

withReverse :: Update () -> Update ()
withReverse upact = do
  setAttribute AttributeReverse True
  upact
  setAttribute AttributeReverse False

itemSym :: Item -> Char
itemSym i = case iType i of
  Weapon -> '|'
  Armor  -> '['
  Potion -> '!'
  Scroll -> '?'
  Corpse -> '%'
  Food   -> ','
  
drawEverything :: Game ()
drawEverything  = do
  g <- get
  l <- Z.cursor `fmap` gets levels
  let (p, en') = getPC (entities l)
      tm       = tilemap l
      seen     = seenL p
      seeing   = seeingL p
      (u,d)    = stairs l

  curses $ draw $ do
    clear (bounds tm)
    -- draw level
    forM_ (indices tm) $ \i -> do 
      when (seen ! i) (drawStringAt i [tm ! i])
    withReverse $
      forM_ (indices tm) $ \i ->
        when (seeing ! i) $
          drawStringAt i [tm ! i]
    -- draw stairs
    when (seen ! u) $ drawStringAt u "<"
    when (seen ! d) $ drawStringAt d ">"
    -- draw items
    forM_ (items l) $ \(ip,i) -> 
      when (seeing ! ip) (drawStringAt ip [itemSym i])
    -- draw enemies
    forM_ en' $ \e ->
      when (seeing ! pos e) (drawStringAt (pos e) [sym e])
    -- draw player
    withReverse $ drawStringAt (pos p) "@"
    -- concat unseen messages and draw them
    let dms = concat $ reverse $ map (\(_,_,m) -> m++" ") $ 
                takeWhile (\(_,b,_) -> not b) (msgs g)
    unless (null dms) $
      withReverse $ drawStringAt (1,1) dms
    -- draw a statusbar, displaying time, speed, dungeon level, and hp
    withReverse $ drawStringAt (1, 24) $
      "Time: " ++ show (nextMove p) ++ " Speed: " ++ show (speed p) 
      ++ " DL: " ++ show (dlvl g) ++ " HP: " ++ show (fst (hp p)) 
      ++ "/" ++ show (snd (hp p))

above, below, right, left :: Vector2 -> Vector2
above (x, y) = (x, y - 1)
below (x, y) = (x, y + 1)
right (x, y) = (x + 1, y)
left  (x, y) = (x - 1, y)

aboveleft, aboveright, belowleft, belowright :: Vector2 -> Vector2
aboveleft  (x, y) = (x - 1, y - 1)
aboveright (x, y) = (x + 1, y - 1)
belowleft  (x, y) = (x - 1, y + 1)
belowright (x, y) = (x + 1, y + 1)

-- | maps directional keys to movement functions
keyToDir :: Char -> Vector2 -> Vector2
keyToDir c = 
  case c of
    'k' -> above
    'j' -> below
    'l' -> right
    'h' -> left
    'y' -> aboveleft
    'u' -> aboveright
    'b' -> belowleft
    'n' -> belowright
    '.' -> id
    _   -> error "movePlayer: controls misconfigured"

-- | Move the player, fail to move the player, rest, or attack an entity
movePlayer :: Char -> Game ()
movePlayer c = do
  l  <- Z.cursor `fmap` gets levels
  let (p,en') = getPC en
      en = entities l
      tm = tilemap l
      -- position player wants to move to
      newp    = keyToDir c (pos p)
      
  -- is there someone there?
  case en' `getEntityAt` newp of
      -- we're moving into an enemy
      Just e -> attackEntity p e
      -- or not
      Nothing ->
        if tm `walkableL` newp
          then do
            -- print a message if there are items under the player
            is <- itemsAt newp
            case length is of
              0 -> return ()
              1 -> addMessage $ "You see here " ++ iName (head is)
              _ -> when (pos p /= newp) $ do
                    addMessage $ "You see here " ++ 
                      concatMap (\i -> iName i ++ ", ") (init is)
                      ++ "and " ++ iName (last is)
            p `moveEntityTo` newp
          else addMessage "You bump into an obstacle!"

itemsAt :: Vector2 -> Game [Item]
itemsAt v = do
  l <- items `fmap` Z.cursor `fmap` gets levels
  return $ map snd $ filter (\(p,_) -> p == v) l

modifyEntityHp :: Int -> Entity -> Entity
modifyEntityHp n e = 
  let (hp', maxhp') = hp e
      newhp = min maxhp' (hp' + n)
  in e { hp = (newhp, maxhp') }

getEntityAt :: [Entity] -> Vector2 -> Maybe Entity
getEntityAt es p = find (\e -> pos e == p) es

anyEntityAt :: [Entity] -> Vector2 -> Bool
anyEntityAt es p = p `elem` map pos es

addMessage :: String -> Game ()
addMessage m = do
  g <- get
  put $ g { msgs = (turn g, False, m) : msgs g }

removePlayerFromCurLevel :: Game ()
removePlayerFromCurLevel = do
  g  <- get
  ls <- gets levels
  let l = Z.cursor ls
      (_,en) = getPC (entities l)
      l' = l { entities = en }
  put $ g { levels = Z.replace l' ls }

addPlayerOnCurrentLevel :: Entity -> Game ()
addPlayerOnCurrentLevel e = do
  g  <- get
  ls <- gets levels
  let l = Z.cursor ls
      l' = l { entities = e : entities l }
  put $ g { levels = Z.replace l' ls }

ageEntities :: Int -> [Entity] -> [Entity]
ageEntities n es =
  map (\e -> e { nextMove = n }) es
          
-- | merge the old "seen" array with the new "seeing" array
-- to produce an updated "seen" array
updateSeen :: Array Vector2 Bool
           -> Array Vector2 Bool
           -> IO (Array Vector2 Bool)
updateSeen old_seen new_seeing = do
  u_n_s <- thaw new_seeing :: IO (IOArray Vector2 Bool)
  upds <- filter snd `fmap` getAssocs u_n_s
  return $ old_seen // upds

-- | update the "seen" and "seeing" arrays
updateVision' :: GameState -> IO GameState
updateVision' g = do
  let l       = Z.cursor $ levels g
      tm      = tilemap l
      (p,en') = getPC (entities l)
      pp      = pos p
      seen    = seenL p
  
  (seeing',seen') <- do
    s <- ioInit
    newSeeing <- newArray (bounds tm) False :: IO (IOUArray Vector2 Bool)

    let lightPoint x y = writeArray newSeeing (x, y) True
        isOpaque x y   = return (tm ! (x, y) `elem` " ═║╔╝╚╗")

    -- use FOV's circle to write into the newSeeing array
    circle s pp 5 lightPoint isOpaque
    newSeeing' <- freeze newSeeing :: IO (Array Vector2 Bool)
    newSeen' <- updateSeen seen newSeeing'
    return (newSeeing', newSeen')

  let np = p { seenL = seen', seeingL = seeing' }
      ne = np : en'
      nl = l { entities = ne }
      g' = g { levels = Z.replace nl (levels g) }

  return g'

getGoodCh :: Game Char
getGoodCh = curses $ waitForChrs "?hjklyubnqpria.,><ed" 

setQuit :: Game ()
setQuit = do 
  g <- get
  put $ g { pquit = True }

-- | Checks if the player is dead and quits if they are.
isPlayerDead :: Game ()
isPlayerDead = do
  php <- fst `fmap` hp `fmap` fst `fmap` getPC `fmap` entities `fmap` Z.cursor `fmap` gets levels
  when (php <= 0) $ do
    addMessage "Oh no! You are dead!"
    setQuit

-- | Turn or time is a measure of how long the player has been in the dungeon.
-- An entity with 100 speed and 100 speed armor takes 10 "time" to move one
-- square. Higher speed = less time taken.
setTurn :: Game ()
setTurn = do
  g <- get
  p <- fst `fmap` getPC `fmap` entities `fmap` Z.cursor `fmap` gets levels
  put $ g { turn = nextMove p }

-- | Creates a brand-new, fully filled Game.
mkDungeonLevel :: Game ()
mkDungeonLevel = do
  l  <- io $ mkRandRooms (80, 22) -- tilemap
  p  <- io $ randFloor l          -- player position
  ep <- io $ randFloor l          -- exit position
  is <- io $ mkItemsOnLevel l     -- items
  p' <- io $ mkPlayer p           -- player entity
  enemies' <- io $ mkEnemiesOnLevel 1 0 l
  let l' = Level 
            { tilemap  = l
            , stairs   = (p, ep)
            , entities = p' : enemies'
            , items    = is
            , narr     = mkNeighbourArray l ".⋅+#SDk"
            }
  put GameState
    { levels   = Z.insert l' Z.empty
    , msgs     = [(0, False, "Welcome to hoodie! (press ? for help)")]
    , turn     = 0
    , pquit    = False
    , dlvl     = 1
    }

descend :: Game ()
descend = do
  level <- Z.cursor `fmap` gets levels
  let exit = snd (stairs level)
  if pos (fst (getPC (entities level))) == exit
     then do
       curses $ draw $ clear (bounds (tilemap level))
       descendLevel
       addMessage "You descend the stairs!"
     else addMessage "There are no down stairs here!"

-- | changes all the things that need the change between floors
descendLevel :: Game ()
descendLevel = do
  l <- cur `fmap` gets levels
  let (p,_) = getPC (entities l) -- get current player
  -- remove the player from the current level
  removePlayerFromCurLevel
  ls <- gets levels
  -- check if there's a level below us already generated, otherwise make
  -- a new level
  (p'', ls'') <- if Z.beginp ls
           then do -- we're making a new level
             tm <- io $ mkRandRooms (80,22)
             pp <- io $ randFloor tm -- new player position
             ep <- io $ randFloor tm -- exit position
             is <- io $ mkItemsOnLevel tm
             en <- io $ mkEnemiesOnLevel 1 (nextMove p + 1) tm
             let p' = p { pos     = pp
                        , seenL   = listArray ((1,1), (80,22)) (repeat False)
                        , seeingL = listArray ((1,1), (80,22)) (repeat False)
                        }
             -- insert the level at the cursor
             return (p', Z.insert Level 
               { tilemap  = tm
               , stairs   = (pos p', ep)
               , entities = en
               , items    = is
               , narr     = mkNeighbourArray tm ".⋅+#SDk"
               } ls)
           -- there's already a generated level below us, so just switch
           else do
             let l' = Z.cursor $ Z.left ls
                 -- put the player on the up stairs of the level we're going to
                 exit = fst $ stairs l'
                 en = entities l'
                 -- adjust the nextMove of all entities on the previous
                 -- level to that of the player's current
                 l'' = l' { entities = ageEntities (nextMove p + 1) en }
                 -- move the player to the position of the up stairs on
                 -- the next level
                 p' = p { pos = exit 
                        , seenL   = listArray ((1,1), (80,22)) (repeat False)
                        , seeingL = listArray ((1,1), (80,22)) (repeat False)
                        }
             return (p', Z.replace l'' (Z.left ls))
  g <- get
  put $ g { levels = ls''
          , dlvl = 1 + dlvl g }
  -- add the player back on the new level
  addPlayerOnCurrentLevel p''

-- | go up if we can
ascend :: Game ()
ascend = do
  g <- get
  level <- Z.cursor `fmap` gets levels
  let exit  = fst (stairs level)
      (p,_) = getPC (entities level)
  if Z.endp (Z.right (levels g))
    then if pos p == exit
            then addMessage "Are you sure you want to exit the dungeon?"
            else addMessage "There are no up stairs here!"
    else
      if pos p == exit
        then do
          let prevlevel = Z.cursor $ Z.right $ levels g
              prevexit = snd $ stairs prevlevel
              -- need to set the next move time of entities on the previous level
              -- the player's current
              preven   = ageEntities (nextMove p) (entities prevlevel)
              -- set the player's location to the previous' level down stairs
              -- and reset the vision
              newp     = p { pos = prevexit                         
                           , seenL   = listArray ((1,1), (80,22)) (repeat False)
                           , seeingL = listArray ((1,1), (80,22)) (repeat False)
                           }
              -- add the player to the list of the entities on the previous
              -- level
              newen    = newp : preven
              newl     = prevlevel { entities = newen }
          curses $ draw $ clear (bounds (tilemap level))
          -- remove the player from the current level
          removePlayerFromCurLevel
          g' <- get
          -- and put him in the one we're going to, and decrement the
          -- dungeon level 
          put $ g' { levels = Z.replace newl $ Z.right (levels g')
                  , dlvl = dlvl g' - 1 }
          addMessage "You ascend the stairs!"
        else addMessage "There are no up stairs here!"
  
-- | This is the main update function, updating one entity per call. The
-- entity to be updated is the one with the lowest nextMove value. When the
-- entity perform a task (including nothing at all), the time cost of the task
-- is added to its nextMove. "turn" is used for the lack of a better word.
gameTurn :: Game ()
gameTurn = do
  processNegativeHpEntities
  l <- Z.cursor `fmap` gets levels
  let en@(e, _) = entityPQ (entities l)
  if isPC e 
    then do
      setTurn
      drawEverything
      ageMessages
      c <- getGoodCh
      case c of 
        'q' -> setQuit
        'p' -> displayMessageLog
        '?' -> displayHelp
        ',' -> playerPickupItem
        'd' -> playerDropItem
        'a' -> entityApplyItem e
        'i' -> displayPlayerInventory
        'e' -> playerWieldItem
        'r' -> mkDungeonLevel >> updateVision
        '>' -> descend >> updateVision
        '<' -> ascend >> updateVision
        _   -> movePlayer c >> updateVision
    else processEnemies en
  isPlayerDead
  
modifyEntity :: Entity -> (Entity -> Entity) -> Game ()
modifyEntity e f = do
  g  <- get
  ls <- gets levels
  let l = Z.cursor ls
      en = entities l
      ne = map (\e' -> if pos e' == pos e then f e' else e') en
      nl = l { entities = ne }
  put $ g { levels = Z.replace nl (levels g) }

-- |    The time needed to perform an action is:
--
--                        1000
--       --------------------------------------
--       (speed of entity + speed of item used)       
--         ----------------------------------
--                         2
--
-- or equivalently: 2000 / (speed of entity + speed of item used)
--
-- for moving around, the item speed used is that of the equipped armor
itemUseCost :: Entity -> Item -> Int
itemUseCost e i = 2000 `div` (speed e + iSpeed i)

-- TODO: consume item or item charge on use
-- | Use your misc item to do something
entityApplyItem :: Entity -> Game ()
entityApplyItem e = do
  g <- get
  let pm     = equMisc (inv e)
      verb   = case iType pm of
                 Potion -> "drink"
                 Scroll -> "read"
                 Corpse -> "squeeze"
                 _      -> "use"
      isPlayer = pc e
      useMsg = 
        if isPlayer
           then "You " ++ verb ++ " the " ++ iName pm ++ "!"
           -- don't bother using verbs for others :)
           else name e ++ " uses the " ++ iName pm ++ "!"
  -- Notes: 
  -- * modifyEntity should be called once per effect.
  -- * effects must have addEntityTime, otherwise AI can enter an infinite loop
  case iEffect pm of
    -- item does nothing
    None      -> do
      addMessage useMsg
      when isPlayer $ addMessage "You reflect on your life..."
      modifyEntity e ( addEntityTime (itemUseCost e pm))
    -- item heals (or hurts)
    Healing n -> do
      addMessage useMsg
      modifyEntity e ( modifyEntityHp n
                     . addEntityTime (itemUseCost e pm))
    -- item kills entity (doh)
    Death     -> do
      -- always add useMsg
      addMessage useMsg
      -- only add these when the player uses items
      when isPlayer $ addMessage "You feel stupid!"
      modifyEntity e ( modifyEntityHp (-999)
                     . addEntityTime (itemUseCost e pm))
    -- item randomly teleports entity
    Teleport  -> do
      addMessage useMsg
      newp <- io $ randFloor (tilemap $ Z.cursor $ levels g) `satisfying` (/= pos e)
      modifyEntity e ( modifyEntityPos newp
                     . addEntityTime (itemUseCost e pm))
      addMessage "Woosh!"
      updateVision
    -- item does nothing
    Yuck      -> do
      addMessage useMsg
      when isPlayer $ addMessage "Yuck!"
      modifyEntity e ( addEntityTime (itemUseCost e pm))
    -- item turns entity's weapon into another one
    Transmute -> do
      addMessage useMsg
      let curWeapon = equWeapon $ inv e
      -- find a new weapon that's different from the current one
      newWeapon <- io $ randomElem testWeapons `satisfying` (/= curWeapon)
      -- equip it
      modifyEntity e ( modifyInventory (\i -> i { equWeapon = newWeapon })
                     . addEntityTime (itemUseCost e pm))
      when isPlayer $ addMessage $ "Your " ++ iName curWeapon 
                    ++ " turns into " ++ iName newWeapon ++ "!"

satisfying :: Monad m => m a -> (a -> Bool) -> m a
satisfying = flip iterateUntil

modifyInventory :: (Inventory -> Inventory) -> Entity -> Entity
modifyInventory f e = e { inv = f (inv e) }
                      
-- remove the nth item from the items at position                      
removeFloorItem :: Vector2 -> Int -> Game ()
removeFloorItem v n = do
  g  <- get
  ls <- gets levels
  is <- itemsAt v
      -- new items on the floor
  let new  = map (\x -> (v, x)) (removeElem is n)
      -- the rest of them
      rest = filter (\x -> fst x /= v) (items l)
      l = Z.cursor ls
      ne = l { items = new ++ rest }
  put $ g { levels = Z.replace ne ls }

-- display a menu to pick an item, and return the item and its index in the 
-- list
pickItem :: [Item] -> Game (Maybe (Int, Item))
pickItem is = do
  bs <- bounds `fmap` tilemap `fmap` Z.cursor `fmap` gets levels
  let iItems  = zip [0..] is
      letters = ['a'..]
  ch <- curses $ do
    draw $ do
      clear bs
      drawStringAt (1,1) "Items:"
      forM_ iItems $ \(y, i) ->
        drawStringAt (1, y + 2) $ letters !! y : ". " ++ iName i
    render
    waitForChrs ['A'..'z']
  let idx = ord ch - 97
  if idx > length is || null is
    then do addMessage "No such item."
            return Nothing
    else return $ Just (idx, is !! idx)

-- the player can pick up items. 
  -- the item is removed from the floor
  -- the item is placed into the general inventory of the player
  -- if there is more than one item, the player gets a menu to choose which
  -- item to pick up.
-- the player has an inventory with 15 slots
-- 'i' displays an inventory. from there the player can
--  'e' - equip an item from the general inventory
       -- the item is equipped
       -- the item is removed from the general inventory
       -- the item that was previous equipped is added back into the G.I.
       -- unless it's one of the default items
--  'd' - drop an item
       -- the item is removed from the general inventory
       -- the item is added to the items list at the player's feet
--  'U' - unwield an item
       -- the item is unequipped, and replaced with the defaults 
       -- (fists, wizard's robe, NYT)

playerDropItem :: Game ()
playerDropItem = do
  g  <- get
  ls <- gets levels
  let l = Z.cursor ls
      (p,_) = getPC (entities l)
  mi <- pickItem (storedItems (inv p))
  case mi of
    Nothing    -> return ()
    Just (n,i) -> do
      is <- itemsAt (pos p)
      if length is >= 3
        then
          addMessage "There's no room to drop anything there!"
        else do
          let nl = l { items = (pos p, i) : items l }
          put $ g { levels = Z.replace nl ls } 
          modifyEntity p ( modifyInventory (removeStoredItemNth n)
                         . addEntityTime (itemUseCost p i))
          addMessage $ "You drop the " ++ iName i

playerWieldItem :: Game ()
playerWieldItem = do
  -- player
  p <- fst `fmap` getPC `fmap` entities `fmap` Z.cursor `fmap` gets levels
  -- pick an item in the inventory
  mi <- pickItem (storedItems (inv p))
  case mi of
    Nothing    -> return ()
    -- wield the selected item, remove it from the general inventory
    Just (n,i) -> do
      modifyEntity p ( modifyInventory (wieldItem i
                     . removeStoredItemNth n)
                     . addEntityTime (itemUseCost p i))
      addMessage $ "You wield the " ++ iName i

removeStoredItemNth :: Int -> Inventory -> Inventory
removeStoredItemNth n iv = iv { storedItems = removeElem (storedItems iv) n }

wieldItem :: Item -> Inventory -> Inventory
wieldItem i iv =
  let (ww, wa, wm) = (equWeapon iv, equArmor iv, equMisc iv)
  in case iType i of
      Weapon -> iv { equWeapon = i, storedItems = ww : storedItems iv }
      Armor  -> iv { equArmor  = i, storedItems = wa : storedItems iv }
      _      -> iv { equMisc   = i, storedItems = wm : storedItems iv }

addItem :: Item -> Inventory -> Inventory
addItem i iv = iv { storedItems = i : storedItems iv }

-- | Pick up an item, if there is one below the player.
playerPickupItem :: Game ()
playerPickupItem = do
  ls <- gets levels
  let l = Z.cursor ls
      -- player and friends
      (p, _) = getPC (entities l)
  -- is there an item below us?
  is <- itemsAt (pos p)
  if length (storedItems (inv p)) >= 10
    then addMessage "You can't carry anything more!"
    else
      case length is of
        0 -> addMessage "There's nothing here to pick up!"
        1 -> do 
          let i = head is
          addMessage $ "You pick up the " ++ iName i ++ "!"
          modifyEntity p ( modifyInventory (addItem i)
                         . addEntityTime (itemUseCost p i))
          removeFloorItem (pos p) 0
        _ -> do
          mi <- pickItem is
          case mi of 
            Nothing    -> return ()
            Just (n,i) -> do
              addMessage $ "You pick up the "++ iName i ++ "!"
              modifyEntity p ( modifyInventory (addItem i)
                             . addEntityTime (itemUseCost p i))
              removeFloorItem (pos p) n
{-      
    Nothing -> 
    Just i  -> do 
          -- make a new inventory based on the type of the item
      let newinv = case iType i of
           Weapon -> cinv { equWeapon = i }
           Armor  -> cinv { equArmor  = i }
           _      -> cinv { equMisc   = i }
          -- item we're dropping from the player's inventory
          dropped = case iType i of
           Weapon -> equWeapon (inv p)
           Armor  -> equArmor  (inv p)
           _      -> equMisc   (inv p)
      -- commit new inventory, remove the item from the floor
      -- and add the dropped item to the floor at the player's feet
      put $ g { entities = p { inv = newinv } : en 
              , items    = (pos p, dropped) : filter (/= (pos p, i)) is 
              }
      -- picking stuff up takes time
      modifyEntity p (addEntityTime (itemUseCost p i))
      -- add message
-}
-- | After one turn, set messages to viewed so they don't display any more
ageMessages :: Game ()
ageMessages = do
  g <- get
  put $ g { msgs = map (\(t,_,m) -> (t, True, m)) (msgs g) }

updateVision :: Game ()
updateVision = get >>= io . updateVision' >>= put

-- | Checks if the Entity current hp <= 0.3 of max
panicEntityAI :: Entity -> Bool
panicEntityAI e = 
  let (cHp, mHp) = hp e
      hpRatio = fromIntegral cHp / fromIntegral mHp :: Double
  in hpRatio <= 0.3

processEnemies :: (Entity, [Entity]) -> Game ()
processEnemies (e, en) = do
  let p = (fst . getPC) en
  if panicEntityAI e
     -- if the entity has low hp then use its misc item
     then entityApplyItem e
     else 
       -- if the enemy is next to the player, attack, otherwise
       -- move closer to the player
       if pos e `nextTo` pos p
         then e `attackEntity` p
         else moveEntityAI en e

-- | subtract damage from defender, add time spent to the attacker, and
-- add messages 
attackEntity :: Entity -- attacker
             -> Entity -- defender
             -> Game ()
attackEntity atk def = do
  let -- inventories of the attacker and defender
      aw = equWeapon (inv atk)
      da = equArmor  (inv def)
      -- damage done from attacker to defender is attacker's weapon attack
      -- value minus defender's armor's defend value
      dmg = max 0 (iDamage aw - iDefence da)
  -- add messages
  when (isPC def) $ addMessage $ "The " ++ name atk ++ " hits you!"
  when (isPC atk) $ addMessage $ "You hit the " ++ name def ++ "!"
  unless (isPC def || isPC atk) $
    addMessage $ "The " ++ name atk ++ "hits the " ++ name def ++ "!"
  -- damage the defender
  modifyEntity def (modifyEntityHp (-dmg))
  -- add attack cost to the attacker
  modifyEntity atk (addEntityTime (itemUseCost atk aw))

-- | remove entities that have <= 0 current hitpoints and add messages about
-- their deaths
processNegativeHpEntities :: Game ()
processNegativeHpEntities = do
  g  <- get
  ls <- gets levels
  let l = Z.cursor ls
      died = filter (\e -> fst (hp e) <= 0) (entities l)
  unless (null died) $
           -- death messages
    do let dms  = map (\e -> name e ++ " dies!") died
           -- entities that are still alive
           new  = filter (`notElem` died) (entities l)
           -- create corpses to drop at the positions where
           -- the entities died
           corpses = map (\e -> (pos e, entityToCorpse e)) died
       -- add messages
       mapM_ addMessage dms
       let nl = l { entities = new, items = items l ++ corpses }
       g' <- get
       put $ g' { levels = Z.replace nl (levels g) }

entityToCorpse :: Entity -> Item
entityToCorpse e =
  Item { iType    = Corpse
       , iName    = name e ++ " corpse"
       , iSpeed   = 100
       , iDamage  = 0
       , iDefence = 0
       , iWeight  = weight e
       , iEffect  = Yuck
       }

-- | cartesian distance function
cartDistance :: Vector2 -> Vector2 -> Double
cartDistance (x1, y1) (x2, y2) =
  sqrt $ fromIntegral $ (x2-x1)^(2::Int) + (y2-y1)^(2::Int)

-- | all the directions you could ever possibly want to move in to including none
directions :: [Vector2 -> Vector2]
directions =
    [id, above, below, right, left
    ,aboveright, aboveleft
    ,belowright, belowleft]

-- | calculate the distances between two points if you were to move the
-- *first* point in all possible directions
distances :: Vector2 -> Vector2 -> [Double]
distances a b =
  map (\f -> cartDistance (f a) b) directions

-- | Check if a is next to b
nextTo :: Vector2 -> Vector2 -> Bool
nextTo a b = any (\f -> a == f b) directions

modifyEntityPos :: Vector2 -> Entity -> Entity
modifyEntityPos new e = e { pos = new }

addEntityTime :: Int -> Entity -> Entity
addEntityTime n e = e { nextMove = nextMove e + n }

moveEntityTo :: Entity -> Vector2 -> Game ()
moveEntityTo e newp =
  modifyEntity e ( modifyEntityPos newp
                 . addEntityTime (itemUseCost e (equArmor (inv e)))
                 )

-- | make a list of functions sorted by the distance that they would bring
-- the enemy to if it moved in that direction, then select the first good one
moveEntityAI :: [Entity]  -- rest of the enemies
             -> Entity    -- the entity we're moving
             -> Game () 
moveEntityAI en' e = do
  l  <- Z.cursor `fmap` gets levels
  let en = entities l
      pp = pos (fst (getPC en))
      -- entity position
      ep = pos e
      -- sort the movement functions by the distance that they would
      -- bring the enemy to if it moved into that direction
      sorted_funs = map snd $ sortBy (comparing fst)
                     (zip (distances ep pp) directions)
      -- is this tile a good move?
      goodmove ne = (tilemap l `walkableL` ne -- is the tile traversable?
                  -- is there an enemy on it?
                  && not (en' `anyEntityAt` ne))
                  -- we can also just stand still
                  && ne /= pp
      -- get the first function that's a good move. Since they're sorted
      -- by ascending distance, it's the optimal move. There is always
      -- at least one element of this list - id.
      bestfunc = head $ dropWhile (\f -> not (goodmove (f ep))) sorted_funs
      
  -- enemies have a detection radius of 10, if the player is outside
  -- it then just stand still
  e `moveEntityTo` (if cartDistance ep pp < 10 then bestfunc ep else ep)

quitCond :: Game Bool
quitCond = liftM pquit get

runGame :: Game Bool    -- ^ end condition
        -> Game ()      -- ^ update function
        -> IO GameState -- ^ end state
runGame predf logicf = do
  let loop' = do
        doquit <- predf
        if doquit
          then get
          else logicf >> loop'
  runCurses $ do
    setEcho False
    _ <- setCursorMode CursorInvisible
    runGame1 undefined $ do
      mkDungeonLevel
      updateVision
      loop'

main :: IO ()
main = do
  g <- runGame quitCond gameTurn
  mapM_ print (reverse (msgs g))
  mapM_ (print . entities) (Z.toList (levels g))

-- FOV init
ioInit :: IO Settings
ioInit = do
  settings <- newSettings
  setShape settings Circle
  setOpaqueApply settings True
  return settings

-- ncurses stuff
draw :: Update () -> Curses ()
draw ioact = do
  w <- defaultWindow
  updateWindow w ioact
  render

clear :: (Vector2, Vector2) -> Update ()
clear ((x1, y1), (x2, y2)) =
   let cs = replicate (x2-x1) ' '
    in forM_ [y1..y2+2] $ \y ->
        drawStringAt (x1, y) cs

waitForChrs :: String -> Curses Char
waitForChrs cs = loop' where
  loop' = do
    w  <- defaultWindow
    ev <- getEvent w Nothing
    case ev of
      Nothing -> loop'
      Just (EventCharacter c) -> if c `elem` cs then return c else loop'
      _ -> loop'

drawStringAt :: Vector2 -> String -> Update ()
drawStringAt (x, y) s = do
  moveCursor' (y - 1, x - 1)
  drawString s

screenSize' :: Curses Vector2
screenSize' = screenSize >>= \(y, x) ->
  return (fromIntegral y, fromIntegral x)

moveCursor' :: Vector2 -> Update ()
moveCursor' (y, x) = moveCursor (fromIntegral y) (fromIntegral x)

-- misc stuff
swap :: (a, b) -> (b, a)
swap    (a, b) =  (b, a)

replaceElem :: [a] -> a -> Int -> [a]
replaceElem es e n = take n es ++ e : drop (n + 1) es

removeElem :: [a] -> Int -> [a]
removeElem es n = take n es ++ drop (n + 1) es

randomElem :: [a] -> IO a
randomElem xs = (xs !!) `fmap` randomRIO (0, length xs - 1)

center :: Int -> String -> String
center n xs = 
  let padding = replicate ((n - length xs) `div` 2) ' '
  in padding ++ xs ++ padding
