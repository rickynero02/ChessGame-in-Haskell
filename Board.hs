
module Chess
    (
        Piece(..),
        PieceState(..),
        Position,
        PieceColor(..),
        Player(..),
        Board(..),
        setPiece,
        parsePosition,
        getPiece,
        buildBoard,
        loadPieces,
        setEmpty,
        checkEmpty,
        checkBounds,
        checkColor,
        peekPiece,
        validMove,
        checkEat,
        updatePieceState,
        promotePiece,
        showMovement,
        underCheck,
        checkMove,
        updatePiecePosition,
        checkMate
    ) where

import Data.Char ( toLower )

data PieceType = Pawn | Rook | Bishop | Knigth | King | Queen | None
    deriving (Eq)

type Position = (Int, Int)

data PieceState = Init | TwoStep | Moved deriving(Eq)

data PieceColor = Black | White deriving(Eq)

data Piece = Piece {
    pos :: Position,
    state :: PieceState,
    color :: PieceColor,
    ptype :: PieceType
}

data Player = Player {
    name :: String,
    captured :: [Piece],
    pcolor :: PieceColor
}

data Board = Board {
    pieces :: [[Piece]]
}

instance Show PieceType where
    show Rook = "R"
    show Pawn = "P"
    show King = "K"
    show Queen = "Q"
    show Bishop = "B"
    show Knigth = "N"
    show None = " "

instance Show Piece where
    show p | color p == Black = map (toLower) $ show $ ptype p
           | otherwise = show $ ptype p


instance Show Board where
    show b = "    " ++ concat (map (\x -> show x ++ " ") [0..7])
        ++ "\n" ++ (showLine (pieces b) $ map abs [0..7])
        where showLine (x:xs) (l:ls) = 
                let line = (show l) ++ " " ++ concat (map (\x -> "|" ++ show x) x) ++ "|" in 
                    show line ++ "\n" ++ showLine xs ls
              showLine [] _ = []

parsePosition :: String -> (Int, Int)
parsePosition s =
    let (precRaw, succRaw) = break (== '-') s
        x = (read precRaw) :: Int
        y = (read succRaw) :: Int in (x, y)
 

setPiece :: Piece -> Board -> Board
setPiece p b = 
    let xpos = fst $ pos p
        ypos = snd $ pos p
        row = (pieces b) !! xpos in
            Board (insertList (pieces b) (insertList row p ypos) xpos)
                where insertList xs y pos =
                        let (prec, succ) = splitAt pos xs in
                            prec ++ [y] ++ (tail succ)


buildBoard :: Board
buildBoard = Board (genPieces 7 8)
    where genPieces (-1) _ = []
          genPieces x y = (genPieceRow x (y-1)) : genPieces (x-1) y
          genPieceRow _ (-1) = []
          genPieceRow x y = (Piece (x, y) Init White None) : genPieceRow (x) (y-1)


setPawns :: PieceColor -> Board -> Board
setPawns color b = loop b 0
    where loop b 8 = b
          loop b y =
            let p = Piece (x,y) Init color Pawn in
            setPiece p (loop b (y+1))
          x = if color == White then 6 else 1
    


setOtherPieces :: PieceColor -> Board -> Board
setOtherPieces color b = loop b 0
    where loop b 8 = b
          loop b y = 
            case y of
                0 -> setPiece (Piece (x, y) Init color Rook) (loop b (y+1))
                1 -> setPiece (Piece (x, y) Init color Knigth) (loop b (y+1))
                2 -> setPiece (Piece (x, y) Init color Bishop) (loop b (y+1))
                3 -> setPiece (Piece (x, y) Init color Queen) (loop b (y+1))
                4 -> setPiece (Piece (x, y) Init color King) (loop b (y+1))
                5 -> setPiece (Piece (x, y) Init color Bishop) (loop b (y+1))
                6 -> setPiece (Piece (x, y) Init color Knigth) (loop b (y+1))
                7 -> setPiece (Piece (x, y) Init color Rook) (loop b (y+1))
          x = if color == White then 7 else 0
  

loadPieces :: Board -> Board
loadPieces = setOtherPieces White . setOtherPieces Black . setPawns White . setPawns Black

getPiece :: Position -> Board -> Piece
getPiece p b = 
    let row = (pieces b) !! (fst p)
        piece = row !! snd p in
            piece

setEmpty :: Position -> Board -> Board
setEmpty p = setPiece (Piece p Init White None) 

checkEmpty :: Piece -> Bool
checkEmpty p = ptype p == None

checkBounds :: Position -> Bool
checkBounds p = fst p < 0 || fst p > 7 || snd p < 0 || snd p > 7

checkColor :: Player -> Piece -> Bool
checkColor pl p = pcolor pl == color p

peekPiece :: Player -> Position -> Board -> Maybe Piece
peekPiece player pos board =
    if checkBounds pos
        then let piece = getPiece pos board in
                if checkColor player piece && checkEmpty piece 
                    then Just piece
                else Nothing
    else Nothing


validMove :: Piece -> Position -> Board -> Bool
--Rook movement
validMove piece@(Piece (posX, posY) _ _ Rook) pos@(x, y) b =
    (not $ posX /= x && posY /= y) && (not $ checkMiddlePieces piece pos b)

--King movement
validMove (Piece (posX, posY) _ _ King) (x, y) _ =
    not $ (x > posX + 1 || y > posY +1) || (x < posX-1 || y < posY-1)

--Bishop movement
validMove piece@(Piece (posX, posY) _ _ Bishop) pos@(x, y) b =
    let diffX = abs $ posX - x
        diffY = abs $ posY - y in
            (diffX == diffY) && (not $ checkMiddlePieces piece pos b)

--Queen movement
validMove (Piece ppos state color Queen) pos board =
    validMove (Piece ppos state color Rook) pos board || 
    validMove (Piece ppos state color Bishop) pos board

--Knigth movement
validMove (Piece (posX, posY) _ _ Knigth) (x, y) _ =
    let diffX = abs $ posX - x
        diffY = abs $ posY - y in
            (diffX == 2 && diffY == 1) || (diffY == 2 && diffX == 1)

--Pawn movement
validMove (Piece (posX, posY) Init Black Pawn) (x, y) _ =
    not $ x > posX+2 || y /= posY || x <= posX 
validMove (Piece (posX, posY) Moved Black Pawn) (x, y) _ =
    not $ x > posX+1 || y /= posY || x <= posX
validMove (Piece (posX, posY) Init White Pawn) (x, y) _ =
    not $ x < posX-2 || y /= posY || x >= posX
validMove (Piece (posX, posY) Moved White Pawn) (x, y) _ =
    not $ x < posX-1 || y /= posY || x >= posX


checkEat :: Piece -> Position -> Bool
checkEat (Piece (posX, posY) _ White Pawn) (x, y) =
    not $ x /= posX+1 || (y /= posY+1 && y /= posY-1) || x <= posX
checkEat (Piece (posX, posY) _ Black Pawn) (x, y) =
    not $ x /= posX-1 || (y /= posY+1 && y /= posY-1) || x >= posX
checkEat _ _ = False


getPiecesList :: Board -> [Piece]
getPiecesList b = filter (\x -> ptype x /= None) $ concat (pieces b)


isBetween :: Int -> Int -> Int -> Bool
isBetween n max min = (n > min) && (n < max)

checkMiddlePieces :: Piece -> Position -> Board -> Bool
checkMiddlePieces (Piece (posX, posY) _ _ Rook) (px, py) b = 
    any (== True) $ map (checkPos) $ getPiecesList b
        where checkPos (Piece (x, y) _ _ _) = 
                let xAxis = x == px && ((isBetween y posY py) || (isBetween y py posY))
                    yAxis = y == py && ((isBetween x posX px) || (isBetween x px posX)) in
                        xAxis || yAxis 

checkMiddlePieces (Piece (posX, posY) _ _ Bishop) (_, py) b =
    any (== True) $ map (checkPos) $ getPiecesList b
        where checkPos (Piece (x, y) _ _ _) =
                let diffXposX = abs(posX - x)
                    diffYposY = abs(posY - y) in
                        (diffXposX == diffYposY) && ((isBetween y posY py) || (isBetween y py posY))


updatePieceState :: Piece -> PieceState -> Piece
updatePieceState (Piece ppos _ pcolor ptype) state =
    (Piece ppos state pcolor ptype)


promotePiece :: Piece -> PieceType -> Piece 
promotePiece (Piece ppos pstate pcolor Pawn) newPtype =
    (Piece ppos pstate pcolor newPtype)


showMovement :: Piece -> Position -> String
showMovement (Piece (posX, posY) _ _ ptype) (x, y) =
    show ptype ++ show posX ++ show posY ++ "-" ++ show x ++ show y


underCheck :: Piece -> Board -> Bool
underCheck (Piece ppos _ pcolor King) b = 
    any (== True) $ map (checkCheck) $ filter (\x -> color x /= pcolor) (getPiecesList b)
        where checkCheck piece | (validMove piece ppos b) = True
                               | otherwise = False

checkMove :: Piece -> Position -> Board -> Bool
checkMove piece pos b = 
    (validMove piece pos b) && (checkEmpty (getPiece pos b) || color (getPiece pos b) /= color piece) 

getPossiblePositions :: Piece -> Board -> [Position]
getPossiblePositions piece b = filter ((/=) (-1,-1)) $ map (checkPosition) (getAllPosition b)
    where getAllPosition b =
            let allPieceList = concat $ pieces b in
                map (pos) allPieceList
          checkPosition pos | (checkMove piece pos b) = pos
                            | otherwise = (-1, -1)

updatePiecePosition :: Piece -> Position -> Piece
updatePiecePosition (Piece _ state pcolor ptype) ppos =
    (Piece ppos state pcolor ptype)


checkMate :: Piece -> Board -> Bool
checkMate king@(Piece _ _ _ King) b =
    all (== True) $ map (checkCheck) (getPossiblePositions king b)
        where checkCheck pos = 
                underCheck (updatePiecePosition king pos) b
