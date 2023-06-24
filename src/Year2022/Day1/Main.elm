module Year2022.Day1.Main exposing (main, part1, part2)

import Browser
import Html exposing (Attribute, Html, a, br, button, div, h1, h2, li, p, span, text, textarea, ul)
import Html.Attributes exposing (cols, href, placeholder, rows, style, value)
import Html.Events exposing (onClick, onInput)
import List exposing (head, sortWith, sum, take)
import Year2022.Day1.Input exposing (testInp)


descending : Int -> Int -> Order
descending a b =
    case compare a b of
        LT ->
            GT

        EQ ->
            EQ

        GT ->
            LT


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


elfCalories : String -> Result String (List Int)
elfCalories inp_ =
    let
        inp =
            String.replace "\u{000D}" "" inp_
                |> String.trim
    in
    String.split "\n\n" inp
        |> List.map
            (Maybe.map sum << unwrapList << List.map String.toInt << String.split "\n")
        |> unwrapList
        |> Result.fromMaybe "Not all Ints are valid"
        |> Result.map (sortWith descending)


part1 : List Int -> Result String Int
part1 =
    Result.fromMaybe "Error taking the first element of list" << head


part2 : List Int -> Int
part2 =
    sum << take 3



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { input : String
    , cals : Result String (List Int)
    , part1 : Result String Int
    , part2 : Result String Int
    }


init : Model
init =
    let
        cals =
            elfCalories testInp

        part1Res =
            Result.andThen part1 cals

        part2Res =
            Result.map part2 cals
    in
    Model testInp cals part1Res part2Res



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
                calsRes =
                    elfCalories model.input

                part1Res =
                    Result.andThen part1 calsRes

                part2Res =
                    Result.map part2 calsRes
            in
            { model | cals = calsRes, part1 = part1Res, part2 = part2Res }



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
        , h2 [] [ text "Calories" ]
        , viewResult <| Result.map (ul [] << List.map (li [] << List.singleton << text << String.fromInt)) model.cals
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
