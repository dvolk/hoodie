{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

-- base, mtl, array, containers, random, lens, monad-loops, ncurses, astar, hfov
import Control.Monad.State
import Data.Array.IArray
import Data.Array.IO
import Data.List (sortBy, find, zipWith5)
import Data.Ord (comparing)
import qualified Data.Set as Set
import Data.Maybe
import System.Random
import Control.Lens hiding (Level, indices)
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
-- player can carry 1 weapon, 1 armor and 1 misc item
type Inventory  = (Item, Item, Item) 

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
              deriving (Eq, Show, Read)

data ItemEffect = None
                | Healing Int
                deriving (Eq, Show, Read)

data Item = Item
    { iType    :: ItemType
    , iName    :: String
    , iSpeed   :: Int
    , iDamage  :: Int
    , iDefence :: Int
    , iEffect  :: ItemEffect
    } deriving (Eq, Show, Read)

data Entity = Entity
    { pc       :: Bool
    , pos      :: Vector2
    , sym      :: Char
    , name     :: String
    , hp       :: (Int, Int)
    , inv      :: Inventory
    , speed    :: Int
    , nextMove :: Int
    , seenL    :: Array Vector2 Bool -- ^what the player has seen of the level
    , seeingL  :: Array Vector2 Bool -- ^what the player *currently* sees
    } deriving (Eq, Show, Read)

data GameState = GameState
    { level    :: Level
    , entities :: [Entity]
    , items    :: [(Vector2, Item)]
    , msgs     :: [(Int, Bool, String)] -- ^ turn added, string
    , turn     :: Int
    , pquit    :: Bool
    , narr     :: Neighbours
    , dlvl     :: Int
    } deriving (Eq, Show, Read)

-- | get the next actor and the rest
entityPQ :: [Entity] -> (Entity, [Entity])
entityPQ en = 
  let pq = sortBy (comparing nextMove) en
  in (head pq, tail pq)

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

testWeaponItem, testWeaponItem', testWeaponItem'' :: Item
testWeaponItem   = Item Weapon "Sword of Test +1" 100 1 0 None
testWeaponItem'  = Item Weapon "Shard of Glass" 100 1 0 None
testWeaponItem'' = Item Weapon "Staff of Skepticism" 100 1 0 None

testArmorItem, testArmorItem', testArmorItem'' :: Item
testArmorItem   = Item Armor "Tested Leather Armor" 100 0 1 None
testArmorItem'  = Item Armor "Fancy Suit" 100 0 1 None
testArmorItem'' = Item Armor "Power Armor mk2" 100 0 1 None

testMiscItem, testMiscItem', testMiscItem'' :: Item
testMiscItem   = Item Potion "Homeopathic Potion" 100 1 0 (Healing 0)
testMiscItem'  = Item Potion "Potion of Alcohol" 100 1 0 (Healing (-1))
testMiscItem'' = Item Potion "Potion" 100 1 0 None

testItems :: [Item]
testItems = [testWeaponItem, testWeaponItem', testWeaponItem''
            ,testArmorItem, testArmorItem', testArmorItem''
            ,testMiscItem, testMiscItem', testMiscItem'']

nullInv :: Inventory
nullInv = (testWeaponItem, testArmorItem, testMiscItem)

-- to save disk space, the neighbours array could be discarded here
-- and then rebuilt on load
save :: Game ()
save = get >>= io . writeFile "savefile" . show

load :: Game ()
load = io (readFile "saveFile") >>= io . readIO >>= put

mkPlayer :: Vector2 -> Entity
mkPlayer p' =
  Entity { pc       = True
         , name     = "Player"
         , pos      = p'
         , sym      = '@'
         , hp       = (12, 12)
         , inv      = nullInv
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
mkEnemiesOnLevel dl nm l' = do
  en <- randomRIO (1 * dl, 3 * dl)
  es <- randFloors l' en
  ep <- randFloor l' 
  -- add "boss" on the 5th floor
  let b=[Entity False ep 'G' "Dreadlord Gates" (32,32) 
         nullInv 135 0 nullA nullA | dl==5]
      -- make some test monsters
      syms   = cycle "LAmbda"
      names  = cycle ["Lamar", "Ant", "Mom", "Bear", "Dog", "Armadillo"]
      speeds = cycle [50, 99, 101, 120, 95, 85]
      hps    = cycle [(5,5), (7,7), (8,8), (13,13), (3,3), (1,1)]
  return $ b ++ 
    zipWith5 (\p c n s h -> Entity False p c n h nullInv s nm nullA nullA) 
      es syms names speeds hps

-- | Creates a brand-new, fully filled Game.
mkDungeonLevel :: Game ()
mkDungeonLevel = do
  l  <- io $ mkRandRooms (80, 22)
  p  <- io $ randFloor l -- player
  ep <- io $ randFloor l
  ip <- io $ randFloors l 7
  let turn' = 0
      dlvl' = 1
      p'    = mkPlayer p
      itms  = zip ip (cycle testItems)
  entities' <- io $ mkEnemiesOnLevel dlvl' 0 l
  put GameState
    { entities = p' : entities' 
    , items    = itms
    , level    = l // [(ep, '>')]
    , msgs     = [(0, False, "Welcome to hoodie! (press ? for help)")]
    , turn     = turn'
    , pquit    = False
    , narr     = mkNeighbourArray l ".⋅+#SDk"
    , dlvl     = dlvl'
    }

-- | changes all the things that need the change between floors
descendLevel :: Game ()
descendLevel = do
  g <- get
  let (p,_) = getPC (entities g)
  l      <- io $ mkRandRooms (80, 22)
  pp     <- io $ randFloor l -- player
  ep     <- io $ randFloor l -- exit
  ip <- io $ randFloors l 7
  let dlvl' = dlvl g + 1
      p'    = p { pos     = pp
                , seenL   = listArray ((1,1), (80,22)) (repeat False)
                , seeingL = listArray ((1,1), (80,22)) (repeat False)
                }
      itms  = zip ip (cycle testItems)
  entities' <- io $ mkEnemiesOnLevel dlvl' (nextMove p + 1) l
  put $ g
    { entities = p' : entities' 
                 -- level 5 is the last level
    , level    = if dlvl' == 5 then l else l // [(ep, '>')]
    , narr     = mkNeighbourArray l ".⋅+#SDk"
    , items    = itms
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
      xy <- iterateUntil (`notElem` ps') (liftIO $ randFloor l)
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
  (w,a,m) <- (inv . fst . getPC) `fmap` gets entities
  addMessage $ "Inventory: W: " ++ iName w ++
               " A: " ++ iName a ++
               " M: " ++ iName m 

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

drawEverything :: Game ()
drawEverything  = do
  (GameState l en itms ms _ _ _ dl) <- get
  let (Entity _ pp _ _ (chp,mhp) _ sp nextmove seen seeing, en') = getPC en
  curses $ draw $ do
    clear (bounds l)
    -- draw level
    forM_ (indices l) (\i -> when (seen ! i) (drawStringAt i [l ! i]))
    -- draw the currently visible part of the level reversed
    forM_ (indices l) (\i -> 
      when (seeing ! i)
        (withReverse (drawStringAt i [l ! i])))
    -- draw whole level, for debugging:
--    forM_ (indices l) $ \i -> drawStringAt i [l ! i]
    -- draw items
    forM_ itms $ \(ip,i) -> 
      when (seeing ! ip) $
        drawStringAt ip [itemSym i]
    -- draw enemies
    forM_ en' (\(Entity _ i c _ _ _ _ _ _ _) ->
      when (seeing ! i) 
        (drawStringAt i [c]))
--    forM_ en (\(Entity _ i c _ _ _ _ _ _ _) -> drawStringAt i [c])
    -- draw player
    withReverse $ drawStringAt pp "@"
    -- concat unseen messages and draw them
    let dms = concat $ reverse $ map (\(_,_,m) -> m++" ") $ 
                takeWhile (\(_,b,_) -> not b) ms
    unless (null dms) $
      withReverse $ drawStringAt (1,1) dms
    -- draw a statusbar, displaying time, speed, dungeon level, and hp
    withReverse $ drawStringAt (1, 24) $
      "Time: " ++ show nextmove ++ " Speed: " ++ show sp ++ " DL: " ++ show dl 
      ++ " HP: " ++ show chp ++ "/" ++ show mhp

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

movePlayer' :: [Entity] -> Vector2 -> [Entity]
movePlayer' en new_p_pos = 
  let (p,es) = getPC en
      p' = p { pos = new_p_pos, nextMove = nextMove p + 1000 `div` speed p }
  in p':es

-- | move the player, or attack an enemy if we're moving into one
movePlayer :: Char -> Game ()
movePlayer c = do
  g@(GameState l en _ _ _ _ _ _) <- get
  let (p,en') = getPC en
      dir = case c of
        'k' -> above
        'j' -> below
        'l' -> right
        'h' -> left
        'y' -> aboveleft
        'u' -> aboveright
        'b' -> belowleft
        'n' -> belowright
        _   -> error "movePlayer: controls misconfigured"
  case en' `getEntityAt` dir (pos p) of
      -- we're moving into an enemy
      Just (Entity _ _ _ n' _ _ _ _ _ _) -> do
        addMessage ("You hit the " ++ n' ++ "!")
        g' <- get
        let en''  = modifyEntityAt en   (dir (pos p)) (modifyEntityHp (-1))
            en''' = modifyEntityAt en'' (pos p)       idleEntity
        put g' { entities = en''' }
      -- or not
      Nothing ->
        if l `walkableL` dir (pos p)
          then put $ g { entities = movePlayer' en (dir (pos p)) }
          else addMessage "You bump into an obstacle!"

modifyEntityAt :: [Entity] -> Vector2 -> (Entity -> Entity) -> [Entity]
modifyEntityAt es p f =
  map (\e -> if e == fromJust (es `getEntityAt` p) then f e else e) es

modifyEntityHp :: Int -> Entity -> Entity
modifyEntityHp n e@(Entity _ _ _ _ (hp',maxhp') _ _ _ _ _) = 
  e { hp = (hp'+n, maxhp') }

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
updateVision' game'@(GameState l en _ _ _ _ _ _) = do
  let (pl@(Entity _ p _ _ _ _ _ _ seen _), en') = getPC en
  (seeing',seen') <- do
    s <- ioInit
    newSeeing <- newArray (bounds l) False :: IO (IOUArray Vector2 Bool)

    let lightPoint x y = writeArray newSeeing (x, y) True
        isOpaque x y   = return (l ! (x, y) `elem` " ═║╔╝╚╗")

    circle s p 5 lightPoint isOpaque
    newSeeing' <- freeze newSeeing :: IO (Array Vector2 Bool)
    newSeen' <- updateSeen seen newSeeing'
    return (newSeeing', newSeen')

  let np   = pl { seenL = seen', seeingL = seeing' }
      game = game' { entities = np:en' }

  return game

getGoodCh :: Game Char
getGoodCh = curses $ waitForChrs "?hjklyubnqsdpri.,>" 

setQuit :: Game ()
setQuit = do 
  g <- get
  put $ g { pquit = True }

playerIdle :: Game ()
playerIdle = do
  g <- get
  let (p, en) = getPC (entities g)
      p' = idleEntity p
  put $ g { entities = p':en }

isPlayerDead :: Game ()
isPlayerDead = do
  en <- gets entities 
  let php = fst . hp .fst . getPC $ en
  when (php <= 0) $ do
    addMessage "Oh no! You are dead!"
    setQuit

setTurn :: Game ()
setTurn = do
  g <- get
  let p = fst . getPC $ entities g
  put $ g { turn = nextMove p }
  
gameTurn :: Game ()
gameTurn = do
  setTurn
  pruneNegativeHpEnemies
  g <- get
  let en@(e, _) = entityPQ (entities g)
  if isPC e 
    then do
      drawEverything
      ageMessages
      c <- getGoodCh
      case c of 
        'q' -> setQuit
        'p' -> displayMessageLog
        '?' -> displayHelp
        ',' -> playerPickupItem
        'i' -> displayPlayerInventory
        's' -> save
        'd' -> load
        'r' -> mkDungeonLevel >> updateVision
        '.' -> playerIdle
        '>' -> descend >> updateVision
        _   -> movePlayer c >> updateVision
    else processEnemies en
  isPlayerDead
  
playerPickupItem :: Game ()
playerPickupItem = do
  g <- get
      -- items on the floor
  let is           = items g
      -- player and friends
      (p, en)      = getPC (entities g)
      -- current inventory
      (pw, pa, pm) = inv p
  -- is there an item below us?
  case pos p `lookup` is of
    Nothing -> 
       addMessage "There's nothing here to pick up!"
    Just i  -> do 
          -- make a new inventory based on the type of the item
      let newinv = case iType i of
           Weapon -> (i, pa, pm)
           Armor  -> (pw, i, pm)
           _      -> (pw, pa, i)
          -- item we're dropping from the player's inventory
          dropped = case iType i of
           Weapon -> pw
           Armor  -> pa
           _      -> pm
      -- commit new inventory, remove the item from the floor
      -- and add the dropped item to the floor at the player's feet
      put $ g { entities = p { inv = newinv } : en 
              , items    = (pos p, dropped) : filter (/= (pos p, i)) is 
              }
      addMessage $ "You pick up the " ++ iName i ++ "!"

ageMessages :: Game ()
ageMessages = do
  g <- get
  -- are lenses worth it?
  -- if I used lenses completely:
  -- msgs %= map (_2 .~ True) 
  -- using lenses partially:
  put $ g { msgs = map (_2 .~ True) (msgs g) }
  -- normal record update:
  -- put $ g { msgs = map (\(t,_,m) -> (t, True, m)) (msgs g) }

updateVision :: Game ()
updateVision = get >>= io . updateVision' >>= put

processEnemies :: (Entity, [Entity]) -> Game ()
processEnemies (e, en) = do
  let p = (fst . getPC) en  
  -- if the enemy is next to the player, attack, otherwise
  -- move closer to the player
  if pos e `nextTo` pos p
    then e `attackEntity` p
    else moveEntityAI en e

-- | subtract damage from defender, add time spent to the attacker, and
-- add messages 
attackEntity :: Entity -- attacker
             -> Entity -- defender
             -> Game () -- new defender
attackEntity atk def = do
  en <- gets entities
  let (Entity _ atp _ an _ _ as _ _ _) = atk
      (Entity _ dep _ dn _ _  _ _ _ _) = def
  addMessage (if isPC def
    then "The " ++ an ++ " hits you!" else an ++ " hits the " ++ dn ++ "!")
  let en'  = modifyEntityAt en  dep (modifyEntityHp (-1))
      en'' = modifyEntityAt en' atp (addEntityTime (1000 `div` as))
  g' <- get
  put $ g' { entities = en'' }

-- | remove entities that have <= 0 current hitpoints
pruneNegativeHpEnemies :: Game ()
pruneNegativeHpEnemies = do
  g <- get
  let died = filter (\e -> fst (hp e) <= 0) (entities g)
      new  = filter (`notElem` died) (entities g)
      dms  = map (\e -> name e ++ " dies!") died
  unless (null died) $
    do mapM_ addMessage dms
       g' <- get
       put $ g' { entities = new }

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

(^+^) :: Vector2 -> Vector2 -> Vector2
(x1, y1) ^+^ (x2, y2) = (x1 + x2, y1 + y2)

moveEntityTo :: Entity -> Vector2 -> Entity
moveEntityTo e new = e 
  { pos = new,
    nextMove = nextMove e + 1000 `div` speed e}

addEntityTime :: Int -> Entity -> Entity
addEntityTime n e = e { nextMove = nextMove e + n }

idleEntity :: Entity -> Entity
idleEntity e = e { nextMove = nextMove e + 1000 `div` speed e}

-- | make a list of functions sorted by the distance that they would bring
-- the enemy to if it moved in that direction, then select the first good one
moveEntityAI :: [Entity]  -- rest of the enemies
             -> Entity    -- the entity we're moving
             -> Game () 
moveEntityAI en' e = do
  g@(GameState l en _ _ _ _ _ _) <- get
  let (Entity _ pp _ _ _ _ _ _ _ _) = fst (getPC en)
      (Entity _ ep _ _ _ _ _ _ _ _) = e
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
            -- enemies have a detection radius of 10
      newp = if cartDistance ep pp < 10
                 then e `moveEntityTo` bestfunc ep
                 else idleEntity e
  put $ g { entities = modifyEntityAt en ep (const newp) }

quitCond :: Game Bool
quitCond = liftM pquit get

runGame :: (Curses GameState -> IO GameState) -- ^ Curses init & wrapper
        -> (GameState -> Bool)           -- ^ end condition
        -> (GameState -> Curses GameState)    -- ^ turn function
        -> GameState                     -- ^ starting game state
        -> IO GameState                  -- ^ end state
runGame initf predf logicf sgame = do
  let loop' game =
        if predf game
          then return game
          else logicf game >>= loop'
  initf $ loop' sgame

runGame' :: Game Bool  -- ^ end condition
         -> Game ()    -- ^ update function
         -> IO GameState -- ^ end state
runGame' predf logicf = do
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
  g <- runGame' quitCond gameTurn

  mapM_ print (reverse (msgs g))


ioInit :: IO Settings
ioInit = do
  settings <- newSettings
  setShape settings Circle
  setOpaqueApply settings True
  return settings

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
