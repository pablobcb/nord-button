port module Example exposing (..)

-- where

import Html exposing (Html)
import Html.App
import NordButton


-- MODEL


type alias AppModel =
    { buttonModel : NordButton.Model OscillatorWaveform }


initialModel : AppModel
initialModel =
    { buttonModel =
        NordButton.create
            [ ( "sin", Sine )
            , ( "tri", Triangle )
            , ( "saw", Sawtooth )
            , ( "sqr", Square )
            ]
    }


init : ( AppModel, Cmd Msg )
init =
    ( initialModel, Cmd.none )


type Msg
    = ButtonMsg NordButton.Msg
    | NoOp


type OscillatorWaveform
    = Sawtooth
    | Triangle
    | Sine
    | Square



-- VIEW


view : AppModel -> Html Msg
view model =
    Html.div []
        [ NordButton.nordButton ButtonMsg examplePort model.buttonModel ]



-- UPDATE


updateButton : NordButton.Model OscillatorWaveform -> AppModel -> AppModel
updateButton buttonModel model =
    { model | buttonModel = buttonModel }


updateMap :
    AppModel
    -> (b -> childrenModel -> ( d, Cmd e ))
    -> b
    -> (AppModel -> childrenModel)
    -> (d -> AppModel -> AppModel)
    -> (e -> g)
    -> ( AppModel, Cmd g )
updateMap model childUpdate childMsg getChild reduxor msg =
    let
        ( updatedChildModel, childCmd ) =
            childUpdate childMsg (getChild model)
    in
        ( reduxor updatedChildModel model
        , Cmd.map msg childCmd
        )


update : Msg -> AppModel -> ( AppModel, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        ButtonMsg subMsg ->
            updateMap model NordButton.update subMsg .buttonModel updateButton ButtonMsg



-- SUBSCIPTIONS


subscriptions : AppModel -> Sub Msg
subscriptions model =
    Sub.none


port examplePort : String -> Cmd msg



-- APP


main : Program Never
main =
    Html.App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
