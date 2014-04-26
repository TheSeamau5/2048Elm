import Keyboard


type Cell = { x : Float , y : Float , value : Maybe Float}

makeCell x y value = { x = x , y = y , value = value }

emptyCell x y = makeCell x y Nothing



drawSquare color = filled color (square 100)

mAsText : Maybe a -> Element
mAsText m = case m of
              Nothing -> plainText ""
              Just n  -> asText n

drawCell : Maybe Float -> Color -> Form
drawCell n color = toForm (collage 100 100 
                     [ drawSquare color
                     , scale 3 (toForm (mAsText n))
                     ])

drawCellAt : Cell -> Form
drawCellAt cell = move ( 100 * ( cell.x - 3 ) , 100 * cell.y ) 
                        ( drawCell cell.value (cellColor cell.value) )

cellColor n = case n of
                Just 2    -> rgb 255 0   0
                Just 4    -> rgb 125 0   0
                Just 8    -> rgb 0   125 0
                Just 16   -> rgb 0   255 0
                Just 32   -> rgb 125 125 0
                Just 64   -> rgb 255 255 0
                Just 128  -> rgb 0   0   125
                Just 256  -> rgb 0   0   255
                Just 512  -> rgb 0   125 125
                Just 1024 -> rgb 255 255 255
                Just _    -> rgb 125 125 125
                Nothing   -> rgb 125 125 125
                


render x y = collage 800 800
           [ drawCellAt (makeCell (1 + x) (2 + y) (Just 2))
           , drawCellAt (makeCell 3 1 (Just 8)) 
           ]


--main = render

hor = lift (.x) Keyboard.arrows
ver = lift (.y) Keyboard.arrows

main = lift2 render (lift toFloat hor) (lift toFloat ver)