{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

-- base, mtl, array, containers, random, monad-loops, ncurses, astar, hfov
import Control.Monad.State
import Data.Array.IArray
import Data.Array.IO
import Data.List (sortBy, find)
import Data.Ord (comparing)
import qualified Data.Set as Set
import Data.Maybe
import System.Random
import Control.Monad.Loops (iterateUntil)
import Data.Graph.AStar (aStar)
import UI.NCurses
import FOV

type Vector2    = (Int, Int)
type Room       = (Vector2, Vector2) -- xy, size
type LevelPart  = [(Vector2, Char)] -- update for array 
-- change Level to include entities, dlvl, and narr, 
-- then change dungeon to [dungeon] to allow many levels
type Level      = Array Vector2 Char
type Neighbours = Array Vector2 (Set.Set Vector2)

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

data ItemType = Weapon -- ']'
              | Armor  -- '['
              | Potion -- '!'
              | Scroll -- '?'
              | Corpse -- '%'
              deriving (Eq, Show, Read)

data ItemEffect = None
                | Healing Int
                | Death
                | Teleport
                | Yuck
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
    , weight   :: Int -- mass in grams
    , speed    :: Int
    , nextMove :: Int
    , seenL    :: Array Vector2 Bool -- ^what the player has seen of the level
    , seeingL  :: Array Vector2 Bool -- ^what the player *currently* sees
    } deriving (Eq, Show, Read)

data GameState = GameState
    { level    :: Level
    , entities :: [Entity]
    , items    :: [(Vector2, Item)]
    , msgs     :: [(Int, Bool, String)] -- ^ turn added, seen by player, message
    , turn     :: Int
    , pquit    :: Bool
    , narr     :: Neighbours
    , dlvl     :: Int
    } deriving (Eq, Show, Read)

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
emptyL :: Level
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
testArmors = [Item Armor "Tested Leather Armor" 100 0 1 5000 None
             ,Item Armor "Fancy Suit" 150 0 0 2000 None
             ,Item Armor "Power Armor mk2" 66 0 6 30000 None
             ]

testMisc :: [Item]
testMisc = [Item Scroll "Scroll of Modern Medicine" 100 0 0 100 (Healing 6)
           ,Item Potion "Potion of Alcohol" 100 0 0 700 (Healing (-1))
           ,Item Potion "Homeopathic Potion" 100 0 0 500 (Healing 0)
           ,Item Scroll "Scroll of Death" 100 0 0 100 Death
           ,Item Scroll "Book of Teleportation" 100 0 0 1000 Teleport
           ,Item Corpse "Rodent Corpse" 100 0 0 1000 Yuck
           ]

testEnemies :: [Entity]
testEnemies = 
  [Entity False (0,0) 'L' "Lamar" (5,5) undefined 30000 50 0 nullA nullA
  ,Entity False (0,0) 'A' "Giant Ant" (7,7) undefined 10000 99 0 nullA nullA
  ,Entity False (0,0) 'm' "Mom" (8,8) undefined 60000 101 0 nullA nullA
  ,Entity False (0,0) 'b' "Bear" (13,13) undefined 120000 120 0 nullA nullA
  ,Entity False (0,0) 'd' "Dog" (3,3) undefined 8000 95 0 nullA nullA
  ,Entity False (0,0) 'a' "Armadillo" (1,1) undefined 1000 85 0 nullA nullA
  ]

testBoss :: Entity
testBoss = 
  Entity False (0,0) 'G' "Dreadlord Gates" (32,32) 
   undefined 75000 135 0 nullA nullA

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
save :: Game ()
save = get >>= io . writeFile "savefile" . show

load :: Game ()
load = io (readFile "saveFile") >>= io . readIO >>= put

mkPlayer :: Vector2 -> IO Entity
mkPlayer pos' = do
  pinv <- randInv
  return Entity 
         { pc       = True
         , name     = "Player"
         , pos      = pos'
         , sym      = '@'
         , hp       = (20, 20)
         , inv      = pinv
         , weight   = 75000
         , speed    = 100
         , nextMove = 0
         , seenL    = listArray ((1,1), (80,22)) (repeat False)
         , seeingL  = listArray ((1,1), (80,22)) (repeat False)
         }

mkEnemiesOnLevel :: Int -- number of enemies
                 -> Int -- nextMove of enemies, set to the nextMove of the player
                 -- when descending the dungeon
                 -> Level 
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

mkItemsOnLevel :: Level -> IO [(Vector2, Item)]
mkItemsOnLevel l = do
  i  <- randomRIO (3,9)
  is <- replicateM i (randomElem testItems)
  ip <- randFloors l i
  return (zip ip is)

-- | Creates a brand-new, fully filled Game.
mkDungeonLevel :: Game ()
mkDungeonLevel = do
  l  <- io $ mkRandRooms (80, 22) -- dungeon, or level
  p  <- io $ randFloor l          -- player position
  ep <- io $ randFloor l          -- exit position
  is <- io $ mkItemsOnLevel l
  p' <- io $ mkPlayer p           -- player position
  entities' <- io $ mkEnemiesOnLevel 1 0 l
  put GameState
    { entities = p' : entities' 
    , items    = is
    , level    = l // [(ep, '>')]
    , msgs     = [(0, False, "Welcome to hoodie! (press ? for help)")]
    , turn     = 0
    , pquit    = False
    , narr     = mkNeighbourArray l ".⋅+#SDk"
    , dlvl     = 1
    }

-- | changes all the things that need the change between floors
descendLevel :: Game ()
descendLevel = do
  g <- get
  let (p,_) = getPC (entities g) -- get player
  l  <- io $ mkRandRooms (80, 22)
  pp <- io $ randFloor l -- new player position
  ep <- io $ randFloor l -- exit position
  is <- io $ mkItemsOnLevel l
  let dlvl' = dlvl g + 1
      p'    = p { pos     = pp
                , seenL   = listArray ((1,1), (80,22)) (repeat False)
                , seeingL = listArray ((1,1), (80,22)) (repeat False)
                }
  entities' <- io $ mkEnemiesOnLevel dlvl' (nextMove p + 1) l
  put $ g
    { entities = p' : entities' 
                 -- level 5 is the last level
    , level    = if dlvl' == 5 then l else l // [(ep, '>')]
    , narr     = mkNeighbourArray l ".⋅+#SDk"
    , items    = is
    , dlvl     = dlvl'
    }

-- | make a random pair within given bounds
randomV2 :: (Vector2, Vector2) -> IO Vector2
randomV2 ((x1, y1), (x2, y2)) = do
  x <- randomRIO (x1, x2)
  y <- randomRIO (y1, y2)
  return (x,y)

isFloor :: Level -> Vector2 -> Bool
isFloor l p = l ! p `elem` ".⋅"

-- | find a random part of the level that's walkable
randFloor :: Level -> IO Vector2
randFloor l = iterateUntil (isFloor l) (randomV2 (bounds l))

-- | find n non-repeating random positions that are walkable
randFloors :: Level -> Int -> IO [Vector2]
randFloors l n =
  flip execStateT [] $
    replicateM_ n $ do
      ps' <- get
      xy  <- iterateUntil (`notElem` ps') (liftIO $ randFloor l)
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
mkRandRooms :: Vector2 -> IO Level
mkRandRooms lbounds = do
  -- try 1000 times to create good non-overlapping rooms
  rooms <- flip execStateT [] $
    replicateM_ 1000 $ do
      rooms <- get
      r <- iterateUntil (goodRoom lbounds) (liftIO (createRoom lbounds))
      unless (r `roomOverlapAny` rooms) (modify (r:))

  when (null rooms)
    (error "dungeon generator mkRandRooms didn't make any rooms")

  let room_lps = mkRooms rooms :: LevelPart
      narrP    = mkNeighbourArray (emptyL // room_lps) " "

  paths1 <- connectRandomRoomToAll narrP rooms
  paths2 <- connectRandomRooms narrP rooms (length rooms `div` 2)

  return $ emptyL // (room_lps ++ paths1 ++ paths2)

-- | connect a random room from the list to all rooms. Cheap way to
-- ensure that all rooms are connected.
connectRandomRoomToAll :: Neighbours -> [Room] -> IO LevelPart
connectRandomRoomToAll narrP rooms = do
  rc <- randomElem rooms
  concat `fmap` catMaybes `fmap` forM rooms (\r ->
    if r /= rc
      then connectRooms narrP r rc
      else return Nothing)

-- | make n random connections between rooms
connectRandomRooms :: Neighbours -> [Room] -> Int -> IO LevelPart
connectRandomRooms narrP rooms n =
  concat `fmap` catMaybes `fmap` replicateM n
    (do r1 <- randomElem rooms
        r2 <- randomElem rooms
        if r1 /= r2 then connectRooms narrP r1 r2
                    else return Nothing)

-- | connect two rooms, using corridors generated by A* pathfinding.
-- Randomly picks a side, so all rooms must have one tile free space
-- around them.
connectRooms :: Neighbours -> Room -> Room -> IO (Maybe LevelPart)
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

maybeLookup :: Level -> Vector2 -> Maybe Char
maybeLookup l p@(x,y) =
  let ((x1, y1), (x2, y2)) = bounds l
   in if x > x2 || x < x1 || y > y2 || y < y1
       then Just (l ! p)
       else Nothing

-- | create a array of the sets of neighbours for each tile
mkNeighbourArray :: Level -> String -> Neighbours
mkNeighbourArray l tt =
  let (a@(x1,y1), b@(x2,y2)) = bounds l
   in listArray (a, b) [setOfNeighbours l tt (x,y) |
                        x <- [x1..x2],
                        y <- [y1..y2]]

-- | create a set of all neighbours of a point that are walkable
setOfNeighbours :: Level -> String -> Vector2 -> Set.Set Vector2
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
       -> Maybe LevelPart
mkPath narrP p1 p2 f1 f2 = do
  p' <- simpleAStar narrP (f1 p1) (f2 p2)
  return $ zip p' (repeat '#') ++ [(f1 p1, '#'), (p1,'+'), (p2,'+')]

-- level construction
mkHwall :: Char -> Int -> Vector2 -> LevelPart
mkHwall ch len (x,y) = zip (zip [x..(x-1+len)] (repeat y)) (repeat ch)

mkVwall :: Char -> Int -> Vector2 -> LevelPart
mkVwall ch len (x,y) = zip (zip (repeat x) [y..(y-1+len)]) (repeat ch)

-- borders:
-- ┘ ┐ ┌ └ │ ─ ├  ┬  ┤  ┴  ┼ ╭ ╮ ╯ ╰
-- ═ ║ ╒ ╓ ╔ ╕ ╖ ╗ ╘ ╙ ╚ ╛ ╜ ╝ ╞ ╟
-- ╠ ╡ ╢ ╣ ╤ ╥ ╦ ╧ ╨ ╩ ╪ ╫ ╬
mkRoom :: Room -> LevelPart
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

mkRooms :: [Room] -> LevelPart
mkRooms = concatMap mkRoom

walkableL :: Level -> Vector2 -> Bool
walkableL l p@(x,y) =
  let ((x1,y1), (x2,y2)) = bounds l
   in (x < x2 || x > x1 || y < y2 || y > y1) && (l ! p `elem` ".⋅+#SDk>")

-- TODO: this needs to be fixed to account for messages over 75 chars
displayMessageLog :: Game ()
displayMessageLog = do
  g <- get
  let ((x1,_), (x2,y2)) = bounds (level g)
  curses $ draw $ do
    clear (bounds (level g))
    drawStringAt (1,1) "Message log: ('p' to return)"
    forM_ (zip [3..] (take (y2 - 5) (msgs g))) $ \(n,(t,_,m)) ->
      drawStringAt (x1, n) $ take (x2 - 5) "[" ++ show t ++ "] " ++ m
  _ <- curses $ waitForChrs "p"
  curses $ draw (clear (bounds (level g)))

helpString :: [String]
helpString = ["Hello, hoodie here (press ? again to return)"
             ,""
             ,"Move with the Vi keys: hijkyubn"
             ,"Attack by moving into enemies"
             ,"Wait for a turn by pressing ."
             ,"i to look at your inventory"
             ,"a to apply your misc item"
             ,", to pick up items below you"
             ,"Descend to the next level with > when you are on stairs"
             ,"Press p to display the message log,"
             ,""
             ,"q to quit, coward."]

displayHelp :: Game ()
displayHelp = do
  g <- get
  curses $ draw $ do
    clear (bounds (level g))
    forM_ [1..length helpString] $ \y ->
      drawStringAt (1, y) $ center 80 (helpString !! (y-1))
  _ <- curses $ waitForChrs "?"
  curses $ draw (clear (bounds (level g)))

displayPlayerInventory :: Game ()
displayPlayerInventory = do
  p <- (fst . getPC) `fmap` gets entities
  addMessage $ "Inventory: W: " 
                      ++ iName (equWeapon (inv p)) ++
               " A: " ++ iName (equArmor (inv p)) ++
               " M: " ++ iName (equMisc (inv p)) 

withReverse :: Update () -> Update ()
withReverse upact = do
  setAttribute AttributeReverse True
  upact
  setAttribute AttributeReverse False

itemSym :: Item -> Char
itemSym i = case iType i of
  Weapon -> ']'
  Armor  -> '['
  Potion -> '!'
  Scroll -> '?'
  Corpse -> '%'

drawEverything :: Game ()
drawEverything  = do
  g  <- get
  l  <- gets level
  let (p, en') = getPC (entities g)
      seen     = seenL p
      seeing   = seeingL p

  curses $ draw $ do
    clear (bounds l)
    -- draw level
    forM_ (indices l) $ \i -> do 
      when (seen ! i) (drawStringAt i [l ! i])
      when (seeing ! i) (withReverse (drawStringAt i [l ! i]))
    -- draw items
    forM_ (items g) $ \(ip,i) -> 
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
  l  <- gets level
  en <- gets entities
  is <- gets items
  let (p,en') = getPC en
      -- position player wants to move to
      newp    = keyToDir c (pos p)
      
  -- is there someone there?
  case en' `getEntityAt` newp of
      -- we're moving into an enemy
      Just e -> attackEntity p e
      -- or not
      Nothing ->
        if l `walkableL` newp
          then do
            let i = newp `lookup` is
            when (isJust i) $ addMessage $ "You see here the "++iName (fromJust i)
            p `moveEntityTo` newp
          else addMessage "You bump into an obstacle!"

modifyEntityAt :: [Entity] -> Vector2 -> (Entity -> Entity) -> [Entity]
modifyEntityAt es p f =
  map (\e -> if e == fromJust (es `getEntityAt` p) then f e else e) es

modifyEntityHp :: Int -> Entity -> Entity
modifyEntityHp n e = 
  let (hp', maxhp') = hp e
      newhp = min maxhp' (hp' + n)
  in e { hp = (newhp, maxhp') }

removeEntityAt :: [Entity] -> Vector2 -> [Entity]
removeEntityAt es p = filter (\e -> pos e /= p) es

getEntityAt :: [Entity] -> Vector2 -> Maybe Entity
getEntityAt es p = find (\e -> pos e == p) es

anyEntityAt :: [Entity] -> Vector2 -> Bool
anyEntityAt es p = p `elem` map pos es

addMessage :: String -> Game ()
addMessage m = do
  g <- get
  put $ g { msgs = (turn g, False, m) : msgs g }

descend :: Game ()
descend = do
  g <- get
  if level g ! pos (fst (getPC (entities g))) == '>'
     then do
       curses $ draw $ clear (bounds (level g))
       descendLevel
       addMessage "You descend into the next level!"
     else addMessage "There are no stairs here!"

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
  let l       = level g
      (p,en') = getPC (entities g)
      pp      = pos p
      seen    = seenL p
  
  (seeing',seen') <- do
    s <- ioInit
    newSeeing <- newArray (bounds l) False :: IO (IOUArray Vector2 Bool)

    let lightPoint x y = writeArray newSeeing (x, y) True
        isOpaque x y   = return (l ! (x, y) `elem` " ═║╔╝╚╗")

    -- use FOV's circle to write into the newSeeing array
    circle s pp 5 lightPoint isOpaque
    newSeeing' <- freeze newSeeing :: IO (Array Vector2 Bool)
    newSeen' <- updateSeen seen newSeeing'
    return (newSeeing', newSeen')

  let np = p { seenL = seen', seeingL = seeing' }
      g' = g { entities = np:en' }

  return g'

getGoodCh :: Game Char
getGoodCh = curses $ waitForChrs "?hjklyubnqsdpria.,>" 

setQuit :: Game ()
setQuit = do 
  g <- get
  put $ g { pquit = True }

-- | Checks if the player is dead and quits if they are.
isPlayerDead :: Game ()
isPlayerDead = do
  en <- gets entities 
  let php = fst . hp .fst . getPC $ en
  when (php <= 0) $ do
    addMessage "Oh no! You are dead!"
    setQuit

-- | Turn or time is a measure of how long the player has been in the dungeon.
-- An entity with 100 speed and 100 speed armor takes 10 "time" to move one
-- square. Higher speed = less time taken.
setTurn :: Game ()
setTurn = do
  g <- get
  let p = fst . getPC $ entities g
  put $ g { turn = nextMove p }
  
-- | This is the main update function, updating one entity per call. The
-- entity to be updated is the one with the lowest nextMove value. When the
-- entity perform a task (including nothing at all), the time cost of the task
-- is added to its nextMove. "turn" is used for the lack of a better word.
gameTurn :: Game ()
gameTurn = do
  processNegativeHpEntities
  g <- get
  let en@(e, _) = entityPQ (entities g)
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
        'a' -> entityApplyItem e
        'i' -> displayPlayerInventory
        's' -> save
        'd' -> load
        'r' -> mkDungeonLevel >> updateVision
        '>' -> descend >> updateVision
        _   -> movePlayer c >> updateVision
    else processEnemies en
  isPlayerDead
  
modifyEntity :: Entity -> (Entity -> Entity) -> Game ()
modifyEntity e f = do
  g  <- get
  en <- gets entities
  put $ g { entities = map (\e' -> if e' == e then f e' else e') en }

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
  let (p, _) = getPC (entities g)
      pm     = equMisc (inv e)
      verb   = case iType pm of
                 Potion -> "drink"
                 Scroll -> "read"
                 Corpse -> "squeeze"
                 _      -> "use"
      useMsg = 
        if e == p
           then "You " ++ verb ++ " the " ++ iName pm ++ "!"
           -- don't bother using verbs for others :)
           else name e ++ " uses the " ++ iName pm ++ "!"
  case iEffect pm of
    None      -> 
      return ()
    Healing n -> do
      addMessage useMsg
      modifyEntity e ( modifyEntityHp n 
                     . addEntityTime (itemUseCost e pm))
    Death     -> do
      -- always add useMsg
      addMessage useMsg
      -- only add these when the player uses items
      when (e == p) $ addMessage "You feel stupid!"
      modifyEntity e ( modifyEntityHp (-999))
    Teleport  -> do
      addMessage useMsg
      newp <- io $ randFloor (level g)
      modifyEntity e (modifyEntityPos newp)
      addMessage "Woosh!"
      updateVision
    Yuck      -> do
      addMessage useMsg
      when (e == p) $ addMessage "Yuck!"
      
-- | Pick up an item, if there is one below the player.
playerPickupItem :: Game ()
playerPickupItem = do
  g <- get
      -- items on the floor
  let is      = items g
      -- player and friends
      (p, en) = getPC (entities g)
      -- current inventory
      cinv    = inv p
  -- is there an item below us?
  case pos p `lookup` is of
    Nothing -> 
       addMessage "There's nothing here to pick up!"
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
      addMessage $ "You pick up the " ++ iName i ++ "!"

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
  g <- get
  let died = filter (\e -> fst (hp e) <= 0) (entities g)
  unless (null died) $
           -- death messages
    do let dms  = map (\e -> name e ++ " dies!") died
           -- entities that are still alive
           new  = filter (`notElem` died) (entities g)
           -- create corpses to drop at the positions where
           -- the entities died
           corpses = map (\e -> (pos e, entityToCorpse e)) died
       -- add messages
       mapM_ addMessage dms
       g' <- get
       put $ g' { entities = new, items = corpses ++ items g' }

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
  l  <- gets level
  en <- gets entities
      -- player position
  let pp = pos (fst (getPC en))
      -- entity position
      ep = pos e
      -- sort the movement functions by the distance that they would
      -- bring the enemy to if it moved into that direction
      sorted_funs = map snd $ sortBy (comparing fst)
                     (zip (distances ep pp) directions)
      -- is this tile a good move?
      goodmove ne = (l `walkableL` ne -- is the tile traversable?
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

randomElem :: [a] -> IO a
randomElem xs = (xs !!) `fmap` randomRIO (0, length xs - 1)

center :: Int -> String -> String
center n xs = 
  let padding = replicate ((n - length xs) `div` 2) ' '
  in padding ++ xs ++ padding
