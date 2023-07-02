module Year2022.Day2.Main exposing (main, part1, part2)

import Browser
import Dict exposing (Dict)
import Html exposing (Attribute, Html, a, br, button, div, h1, h2, li, p, span, text, textarea, ul)
import Html.Attributes exposing (cols, href, placeholder, rows, style, value)
import Html.Events exposing (onClick, onInput)
import List exposing (head, sortWith, sum, take)
import Tuple
import Year2022.Day2.Input exposing (testInp)


unwrapList : List (Maybe Int) -> Maybe (List Int)
unwrapList list =
    let
        validInts =
            List.filterMap identity list
    in
    if List.length validInts == List.length list then
        Just validInts

    else
        Nothing


playOpts : Dict String Int
playOpts =
    Dict.fromList
        [ ( "A", 1 ) -- rock
        , ( "X", 1 )
        , ( "B", 2 ) -- paper
        , ( "Y", 2 )
        , ( "C", 3 ) -- scissors
        , ( "Z", 3 )
        ]


getGamePts : ( Int, Int ) -> Result String Int
getGamePts ( a, b ) =
    if a == b then
        Ok <| 3 + b

    else if b < a || (b == 1 && a == 3) then
        Ok <| 6 + b

    else if b > a || (b == 3 && a == 1) then
        Ok <| 0 + b

    else
        Err "Unreachable comparison"



-- takes the row (A-C and X-Z separated by space) and returns the score


evalGame : String -> Result String Int
evalGame row_ =
    let
        row =
            case String.split " " row_ of
                [] ->
                    Err "List's length is smaller than 2"

                [ _ ] ->
                    Err "List's length is smaller than 2"

                a_ :: b_ :: _ ->
                    case ( Dict.get a_ playOpts, Dict.get b_ playOpts ) of
                        ( Nothing, Nothing ) ->
                            Err "The key is not inside the dictionary"

                        ( Just _, Nothing ) ->
                            Err "The key is not inside the dictionary"

                        ( Nothing, Just _ ) ->
                            Err "The key is not inside the dictionary"

                        ( Just a, Just b ) ->
                            Ok ( a, b )
    in
    Result.andThen getGamePts row


part1 : String -> Result String Int
part1 inp_ =
    let
        inp =
            String.replace "\u{000D}" "" inp_
                |> String.trim
                |> String.split "\n"

        pts =
            List.map evalGame inp

        filteredPts =
            List.filterMap Result.toMaybe pts
    in
    if List.length filteredPts == List.length pts then
        Ok <| sum filteredPts

    else
        Err "Error when evaluating some games"


part2 : String -> Result String Int
part2 inp_ =
    Ok 0



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { input : String
    , part1 : Result String Int
    , part2 : Result String Int
    }


init : Model
init =
    let
        part1Res =
            part1 testInp

        part2Res =
            part2 testInp
    in
    Model testInp part1Res part2Res



-- UPDATE


type Msg
    = Input String
    | TestInp
    | Run


update : Msg -> Model -> Model
update msg model =
    case msg of
        Input val ->
            { model | input = val }

        TestInp ->
            { model | input = testInp }

        Run ->
            let
                part1Res =
                    part1 model.input

                part2Res =
                    part2 model.input
            in
            { model | part1 = part1Res, part2 = part2Res }



-- VIEW


view : Model -> Html Msg
view model =
    div
        []
        [ h1 [] [ text "Day 1" ]
        , a [ href "https://adventofcode.com/2022/day/1" ] [ text "Link" ]
        , h2 [] [ text "Input" ]
        , textarea [ value model.input, placeholder "Enter input...", onInput Input, rows 20, cols 50 ] []
        , br [] []
        , button [ onClick TestInp ] [ text "Use test input" ]
        , button [ onClick Run ] [ text "Run" ]
        , h2 [] [ text "Part 1" ]
        , viewResult <| Result.map (text << String.fromInt) model.part1
        , h2 [] [ text "Part 2" ]
        , viewResult <| Result.map (text << String.fromInt) model.part2
        ]


viewResult : Result String (Html msg) -> Html msg
viewResult val =
    case val of
        Ok c ->
            c

        Err err ->
            p
                []
                [ span [ style "font-weight" "bold" ] [ text "Error: " ]
                , span [ style "color" "red" ] [ text err ]
                ]
