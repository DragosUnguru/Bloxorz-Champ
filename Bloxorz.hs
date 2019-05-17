{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}


module Bloxorz where

import ProblemState

import qualified Data.Array as A

{-
    Caracterele ce vor fi printate pentru fiecare tip de obiect din joc 
    Puteți înlocui aceste caractere cu orice, în afară de '\n'.
-}

hardTile :: Char
hardTile = '▒'

softTile :: Char
softTile = '='

block :: Char
block = '▓'

switch :: Char
switch = '±'

emptySpace :: Char
emptySpace = ' '

winningTile :: Char
winningTile = '*'

{-
    Sinonim de tip de date pentru reprezetarea unei perechi (int, int)
    care va reține coordonatele de pe tabla jocului
-}

type Position = (Int, Int)

{-
    Direcțiile în care se poate mișcă blocul de pe tablă
-}

data Directions = North | South | West | East
    deriving (Show, Eq, Ord)

{-
    Tip de date care va reprezenta plăcile care alcătuiesc harta și switch-urile
-}

data Cell = HardTile | SoftTile | Block | Switch | EmptySpace | WinningTile deriving (Eq, Ord)

instance Show Cell where
    show cell = case cell of 
        HardTile -> [hardTile]
        SoftTile -> [softTile]
        Block -> [block]
        Switch -> [switch]
        EmptySpace -> [emptySpace]
        WinningTile -> [winningTile]

{-
    Tip de date pentru reprezentarea nivelului curent
-}

data Level = Lvl {
    lvl_map :: (A.Array Position Cell),
    corner :: (Position),
    player :: (Position, Position),
    switch_tile :: [(Position, [Position])],
    finish_tile :: (Position)
}   deriving (Eq, Ord)


isOverboard :: Level -> Bool
isOverboard (Lvl _ (boundx, boundy) ((x1, y1), (x2, y2)) _ _) =
    (x1 > boundx) || (x2 > boundx) || (y1 > boundy) || (y2 > boundy) ||
    (x1 < 0) || (x2 == -1) || (y1 < 0) || (y2 == -1)

lostGame :: Level -> Bool
lostGame (Lvl board _ (pos1, pos2) _ _) =
    (pos2 /= (-7, -7) && board A.! pos2 == EmptySpace) ||
    (board A.! pos1 == EmptySpace) ||
    (pos2 == (-7, -7) && board A.! pos1 == SoftTile)

wonGame :: Level -> Bool
wonGame (Lvl _ _ (bl1, bl2) _ fin_pos) =
    bl1 == fin_pos && bl2 == (-7, -7)

{-
    "Zugraveste" harta cu restul componentelor (player, bloc castigator)
-}
parseBoardHelper :: Level -> [(Position, Cell)]
parseBoardHelper (Lvl _ _ (p1, p2) _ _) =
    filter (\(x, _) -> x /= (-7, -7)) [(p1, Block), (p2, Block)]

parseBoard :: Level -> A.Array Position Cell
parseBoard lvl = let helper_list = parseBoardHelper lvl
                 in (lvl_map lvl) A.// [(pos, bl_type) | (pos, bl_type) <- helper_list]

instance Show Level where
    show lvl =
        "\n" ++ (unlines [concat [show (board A.! (y, x)) | x <- [0..endY]] | y <- [0..endX]]) ++ game_state
        where (endX, endY) = (corner lvl)
              board = parseBoard lvl
              game_state = case ((isOverboard lvl || lostGame lvl), (wonGame lvl { lvl_map = board })) of
                (_, True) -> "Congrats! You won!\n"
                (True, _) -> "Game Over\n"
                (_, _) -> ""

{-
    Primește coordonatele colțului din dreapta jos a hârtii și poziția inițială a blocului.
    Întoarce un obiect de tip Level gol.
    Implicit, colțul din stânga sus este (0, 0).
-}

emptyLevel :: Position -> Position -> Level
emptyLevel corn block_pos =
    Lvl {lvl_map=arr, corner=corn, player=(block_pos, (-7, -7)), switch_tile=[], finish_tile=(-7,-7)}
    where arr = (A.listArray ((0, 0), corn) (repeat EmptySpace)) A.// [(block_pos, HardTile)]

{-
    Adaugă o celulă de tip Tile în nivelul curent.
    Parametrul char descrie tipul de tile adăugat: 
        'H' pentru tile hard 
        'S' pentru tile soft 
        'W' pentru winning tile 
-}

addTile :: Char -> Position -> Level -> Level
addTile 'W' target_pos lvl = lvl { lvl_map=(lvl_map lvl) A.// [(target_pos, WinningTile)], finish_tile=(target_pos) }
addTile bl_type target_pos lvl = lvl { lvl_map=arr }
    where arr = (lvl_map lvl) A.// [(target_pos, req_block)]
          req_block = case bl_type of
            'H' -> HardTile
            _ -> SoftTile

{-
    Adaugă o celulă de tip Swtich în nivelul curent.
    Va primi poziția acestuia și o listă de Position
    ce vor desemna pozițiile în care vor apărea sau 
    dispărea Hard Cells în momentul activării/dezactivării
    switch-ului.
-}

addSwitch :: Position -> [Position] -> Level -> Level
addSwitch sw_pos l lvl = lvl { lvl_map=((lvl_map lvl) A.// [(sw_pos, Switch)]), switch_tile=(sw_pos, l) : (switch_tile lvl) }

{-
    Activate va verifica dacă mutarea blocului va activa vreun switch. 
    În funcție de switch, vor avea loc modificări pe hartă.
-}

isOnSwitch :: Level -> Bool
isOnSwitch lvl = not (null (filter (\(pos, _) -> pos == fst (player lvl) || pos == snd (player lvl)) (switch_tile lvl))) 

getNewBlocks :: Level -> ([Position], Bool)
getNewBlocks lvl = head (map (\(_, lst) -> (lst, (lvl_map lvl) A.! (head lst) == HardTile))
                (filter (\(pos, _) -> pos == fst (player lvl) || pos == snd (player lvl)) (switch_tile lvl)))

modifyTiles :: A.Array Position Cell -> ([Position], Bool) -> A.Array Position Cell
modifyTiles board ([], _) = board
modifyTiles board (x:xs, True) = modifyTiles (board A.// [(x, EmptySpace)]) (xs, True)
modifyTiles board (x:xs, False) = modifyTiles (board A.// [(x, HardTile)]) (xs, False) 

activate :: Level -> Level
activate lvl
    | isOnSwitch lvl == False = lvl
    | otherwise = lvl { lvl_map = board }
    where board = modifyTiles (lvl_map lvl) (getNewBlocks lvl)

{-
    Mișcarea blocului în una din cele 4 direcții 
-}

isHorizontal :: (Position, Position) -> Bool
isHorizontal ((x1, y1), (x2, y2)) = (x1 == x2) && ((y1 == y2 - 1) || (y1 == y2 + 1))

directionalOffset :: Position -> Directions -> Position
directionalOffset (x, y) South = (x + 1, y)
directionalOffset (x, y) North = (x - 1, y)
directionalOffset (x, y) West = (x, y - 1)
directionalOffset (x, y) East = (x, y + 1)

getLeadingBlock :: (Position, Position) -> Directions -> Position
getLeadingBlock ((x1, y1), (x2, y2)) South = if x1 > x2 then (x1, y1) else (x2, y2)
getLeadingBlock ((x1, y1), (x2, y2)) North = if x1 < x2 then (x1, y1) else (x2, y2)
getLeadingBlock ((x1, y1), (x2, y2)) West = if y1 < y2 then (x1, y1) else (x2, y2)
getLeadingBlock ((x1, y1), (x2, y2)) East = if y1 > y2 then (x1, y1) else (x2, y2)

moveUtil :: (Position, Position) -> Directions -> (Position, Position)
moveUtil (pos1, pos2) dir
    | pos2 == (-7, -7) = ((directionalOffset pos1 dir), (directionalOffset (directionalOffset pos1 dir) dir))
    | (((isHorizontal (pos1, pos2) == False) && (dir == South || dir == North))) ||
      (((isHorizontal (pos1, pos2)) == True) && (dir == East || dir == West)) = ((directionalOffset (getLeadingBlock (pos1, pos2) dir) dir), (-7, -7))
    | otherwise = ((directionalOffset pos1 dir), (directionalOffset pos2 dir))

move :: Directions -> Level -> Level
move dir lvl = activate (lvl { player = (moveUtil (player lvl) dir) })

{-
    Va returna True dacă jocul nu este nici câștigat, nici pierdut.
    Este folosită în cadrul Interactive.
-}

continueGame :: Level -> Bool
continueGame lvl = not (lostGame lvl || wonGame lvl)


instance ProblemState Level Directions where
    successors lvl 
        | isGoal lvl == True = []
        | otherwise = filter (\(_, lev) -> (isOverboard lev || lostGame lev) == False) [(dir, move dir lvl) | dir <- [South, North, East, West]]

    isGoal lvl = wonGame lvl

    heuristic (Lvl _ _ ((x1, y1), _) _ (winX, winY)) = abs (x1 - winX) + abs (y1 - winY)
