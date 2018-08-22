module Main exposing (..)

import Grid exposing (..)
import Html exposing (Html, program)
import Html.Attributes exposing (class, classList, id)
import Html.Events exposing (onClick, onMouseOut, onMouseOver)
import Vector


type alias Owner =
    Maybe Bool


type alias Board =
    Grid Owner


type alias Model =
    { board : Board
    , player : Bool
    , victor : Maybe ( Bool, List Vector.Vector )
    , highlighted : Maybe Int
    }


type Msg
    = NoOp
    | Move Int
    | Enter Int
    | Leave
    | Restart


main : Program Never Model Msg
main =
    Html.program
        { init = init ! []
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


init : Model
init =
    { board = newBoard
    , player = False
    , victor = Nothing
    , highlighted = Nothing
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        Move x ->
            makeMoveOn model x ! []

        Enter x ->
            { model | highlighted = Just x } ! []

        Leave ->
            { model | highlighted = Nothing } ! []

        Restart ->
            init ! []


view : Model -> Html Msg
view model =
    let
        eltOfOwner x y owner =
            Html.td
                [ class "box", onClick (Move x), onMouseOver (Enter x), onMouseOut Leave ]
                [ Html.div
                    [ classList
                        [ ( "circle", True )
                        , ( "red", owner == Just False )
                        , ( "yellow", owner == Just True )
                        , ( "highlighted", Just x == model.highlighted )
                        , ( "not-winning", Maybe.map (\( _, lst ) -> List.member ( x, y ) lst) model.victor == Just False )
                        ]
                    ]
                    []
                ]

        combineAcross ( x, y, owner ) acc =
            eltOfOwner x y owner :: acc

        conbineDown rowList acc =
            Html.tr [] rowList :: acc

        boardElts =
            model.board
                |> Grid.indexedMap (\( x, y ) owner -> ( x, y, owner ))
                |> Grid.foldRightTop combineAcross conbineDown [] []

        restartButton =
            Html.button
                [ onClick Restart, id "reset-button" ]
                [ Html.text "Restart" ]
    in
    Html.div
        [ id "content" ]
        [ Html.div
            [ id "content-widget" ]
            [ Html.table [ id "board" ] boardElts
            , restartButton
            ]
        ]


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
            List.head <| List.filter isOwnerless <| List.range 0 5
    in
    case minOpenHeight of
        Nothing ->
            Nothing

        Just y ->
            Just <| Grid.set x y (Just player) board


getWinner : Board -> Maybe ( Bool, List Vector.Vector )
getWinner board =
    let
        directions =
            [ ( 1, 1 ), ( 0, 1 ), ( 1, 0 ), ( 1, -1 ) ]

        spaces : Int -> Int -> Vector.Vector -> List Vector.Vector
        spaces x y v =
            List.map (\i -> Vector.add ( x, y ) <| Vector.scale i v) <| List.range 0 3

        victoryInDirection x y v =
            let
                owner v =
                    Grid.lookupV v board |> Maybe.andThen identity

                owners =
                    List.map owner <| spaces x y v
            in
            if List.all ((==) (owner ( x, y ))) owners then
                owner ( x, y )
                    |> Maybe.andThen (\b -> Just ( b, spaces x y v ))

            else
                Nothing

        combine o1 o2 =
            case ( o1, o2 ) of
                ( Just b, _ ) ->
                    Just b

                ( _, Just b ) ->
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
    if model.victor /= Nothing then
        model

    else
        let
            newMaybeBoard =
                move model.board model.player x
        in
        case newMaybeBoard of
            Nothing ->
                model

            Just newBoard ->
                { model
                    | board = newBoard
                    , victor = getWinner newBoard
                    , player = not model.player
                }
