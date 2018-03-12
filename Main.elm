import Grid exposing (..)
import Vector

import Html exposing (Html, program)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)

type alias Owner =
    Maybe Bool

type alias Board =
    Grid Owner

type alias Model =
    { board : Board
    , player : Bool
    , victor : Owner
    }

type Msg =
      NoOp
    | Move Int
    | Restart

main : Program Never Model Msg
main =
    Html.program
        { init = init ! []
        , update = update
        , subscriptions = (\_ -> Sub.none)
        , view = view
        }

init : Model
init =
    { board = newBoard
    , player = False
    , victor = Nothing
    }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []
        Move x ->
            makeMoveOn model x ! []
        Restart ->
            init ! []

view : Model -> Html Msg
view model =
    let
        header =
            Html.h1 [] [Html.text "Connect Four"]


        buttonStyle =
            style
                [ ("width", "0px") 
                , ("height", "0") 
                , ("border-left", "25px solid transparent")
                , ("border-right", "25px solid transparent")
                , ("border-top", "25px solid black")
                , ("display", "inline-block")
                ]
        button x =
            Html.div [buttonStyle, onClick (Move x)] []
        buttons =
            Html.div [] <| List.map button <| List.range 0 6


        boxStyle =
            style
                [ ("width", "50px")
                , ("height", "50px")
                , ("display", "inline-block")
                , ("background-color", "yellow")
                ]
        rowStyle =
            style
                [ ("display", "flex")
                ]
        color owner =
            case owner of
                Nothing ->
                    "grey"
                Just False ->
                    "red"
                Just True ->
                    "blue"
        circleStyle owner =
            style
                [ ("width", "50px")
                , ("height", "50px")
                , ("border-radius", "25px")
                , ("background-color", color owner)
                ]
        eltOfOwner owner =
            Html.div
                [boxStyle]
                [Html.div [circleStyle owner] []]
        combineAcross owner acc =
            eltOfOwner owner::acc
        conbineDown rowList acc =
            Html.div [rowStyle] rowList::acc
        boardElts =
            Grid.foldRightTop combineAcross conbineDown [] [] model.board
        boardElt =
            Html.div [] boardElts
    in
        Html.div
            []
            [header, buttons, boardElt]

newBoard : Board
newBoard =
    grid 7 6 Nothing

move : Board -> Bool -> Int -> Maybe Board
move board player x =
    let
        isOwnerless y =
            Grid.lookup x y board
            |> Maybe.andThen identity
            |> (==) Nothing
        minOpenHeight =
            List.head <| List.filter isOwnerless <| (List.range 0 5)
    in
        case minOpenHeight of
            Nothing ->
                Nothing
            Just y ->
                Just <| Grid.set x y (Just player) board

getWinner : Board -> Owner
getWinner board =
    let
        directions =
            [ (1,1), (0,1), (1,0), (1,-1) ]

        spaces : Int -> Int -> Vector.Vector -> List Vector.Vector
        spaces x y v =
            List.map (\i -> Vector.add (x,y) <| Vector.scale i v) <| List.range 0 3

        victoryInDirection x y v =
            let
                owner v =
                    Grid.lookupV v board |> Maybe.andThen identity
                owners = 
                    List.map owner <| spaces x y v
            in
                if List.all ((==) (owner (x, y))) owners
                then owner (x, y)
                else Nothing

        combine o1 o2 =
            case (o1, o2) of
                (Just b, _) ->
                    Just b
                (_, Just b) ->
                    Just b
                _ ->
                    Nothing

        victoryFromSpace x y =
            List.map (victoryInDirection x y) directions
            |> List.foldr combine Nothing

        victory =
            Grid.indexedMap (\v _ -> uncurry victoryFromSpace v) board
            |> Grid.foldRightTop combine combine Nothing Nothing
    in
        victory

makeMoveOn : Model -> Int -> Model
makeMoveOn model x =
    if model.victor /= Nothing then model else
    let
        newMaybeBoard =
            move model.board model.player x
    in
        case newMaybeBoard of
            Nothing ->
                model
            Just newBoard ->
                {model |
                  board = newBoard
                , victor = getWinner newBoard
                , player = not model.player
                }