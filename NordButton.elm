module NordButton exposing (..)

-- where

import Html exposing (..)
import Html.Events exposing (..)
import Html.App exposing (map)
import Html.Attributes exposing (..)
import Lazy.List exposing (..)
import Maybe exposing (..)


-- MODEL


type alias Model a =
    { elems : List a
    , labels : List String
    , currentElem : a
    }


create : List ( String, a ) -> Model a
create elems =
    let
        current =
            snd
                <| case List.head elems of
                    Just elem ->
                        elem

                    Nothing ->
                        Debug.crash "empty list on button creation!"
    in
        { elems = List.map snd elems
        , labels = List.map fst elems
        , currentElem = current
        }


type Msg
    = Click (String -> Cmd Msg)


btnStyle : List ( String, String )
btnStyle =
    [ ( "list-style-type", "none" ) ]


redStyle =
    [ ( "color", "red" ) ]



-- VIEW


view : (String -> Cmd Msg) -> Model a -> Html Msg
view cmdEmmiter model =
    div [ class "nord-btn" ]
        [ ul [] <| options model
        , button [ onClick <| Click cmdEmmiter ]
            [ text "a" ]
        ]


options : Model a -> List (Html b)
options model =
    List.map2
        (\elem label ->
            option model elem label
        )
        model.elems
        model.labels


option model elem label =
    li
        [ style
            <| if elem == model.currentElem then
                redStyle
               else
                btnStyle
        ]
        [ text label ]


nordButton : (Msg -> b) -> (String -> Cmd Msg) -> Model c -> Html b
nordButton knobMsg cmdEmmiter model =
    Html.App.map knobMsg
        <| view (\value -> value |> cmdEmmiter)
            model



-- UPDATE


update : Msg -> Model a -> ( Model a, Cmd Msg )
update message model =
    case message of
        Click cmdEmmiter ->
            let
                elems =
                    Lazy.List.cycle
                        <| Lazy.List.fromList model.elems

                maybeNextElem =
                    List.head
                        <| Lazy.List.toList
                        <| Lazy.List.take 1 elems

                nextElem =
                    case maybeNextElem of
                        Just elem ->
                            elem

                        Nothing ->
                            Debug.crash "no values provided"

                elems' =
                    Lazy.List.toList
                        <| Lazy.List.take (List.length model.elems)
                        <| Lazy.List.drop 1 elems
            in
                ( { model | elems = elems' }, nextElem |> toString |> cmdEmmiter )
