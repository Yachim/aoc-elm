module Year2022.Day2.Main exposing (main, part1, part2)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, a, br, button, div, h1, h2, p, span, text, textarea)
import Html.Attributes exposing (cols, href, placeholder, rows, style, value)
import Html.Events exposing (onClick, onInput)
import List exposing (sum)
import Year2022.Day2.Input exposing (testInp)


xyz : Dict String Int
xyz =
    Dict.fromList
        [ ( "X", 1 ) -- rock
        , ( "Y", 2 ) -- paper
        , ( "Z", 3 ) -- scissors
        ]


getPts : ( String, String ) -> Int
getPts ( a, b ) =
    if
        (a == "A" && b == "X")
            || (a == "B" && b == "Y")
            || (a == "C" && b == "Z")
    then
        3

    else if
        (a == "A" && b == "Y")
            || (a == "B" && b == "Z")
            || (a == "C" && b == "X")
    then
        6

    else
        0



-- takes the row (A-C and X-Z separated by space) and returns the score


evalGame : String -> Result String Int
evalGame row =
    case String.split " " row of
        [] ->
            Err "List's length is smaller than 2"

        [ _ ] ->
            Err "List's length is smaller than 2"

        a :: b :: _ ->
            case Dict.get b xyz of
                Nothing ->
                    Err "The key is not inside the dictionary"

                Just pts ->
                    Ok <| pts + getPts ( a, b )


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



-- takes the row (A-C and X-Z separated by space) and returns the score


getPtsPt2 : ( String, String ) -> Int
getPtsPt2 ( a, b ) =
    if
        (a == "A" && b == "X")
            || (a == "B" && b == "Z")
            || (a == "C" && b == "Y")
    then
        3

    else if
        (a == "A" && b == "Z")
            || (a == "B" && b == "Y")
            || (a == "C" && b == "X")
    then
        2

    else
        1


xyzPt2 : Dict String Int
xyzPt2 =
    Dict.fromList
        [ ( "X", 0 ) -- lose
        , ( "Y", 3 ) -- draw
        , ( "Z", 6 ) -- win
        ]


evalGamePt2 : String -> Result String Int
evalGamePt2 row =
    case String.split " " row of
        [] ->
            Err "List's length is smaller than 2"

        [ _ ] ->
            Err "List's length is smaller than 2"

        a :: b :: _ ->
            case Dict.get b xyzPt2 of
                Nothing ->
                    Err "The key is not inside the dictionary"

                Just pts ->
                    Ok <| pts + getPtsPt2 ( a, b )


part2 : String -> Result String Int
part2 inp_ =
    let
        inp =
            String.replace "\u{000D}" "" inp_
                |> String.trim
                |> String.split "\n"

        pts =
            List.map evalGamePt2 inp

        filteredPts =
            List.filterMap Result.toMaybe pts
    in
    if List.length filteredPts == List.length pts then
        Ok <| sum filteredPts

    else
        Err "Error when evaluating some games"



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
