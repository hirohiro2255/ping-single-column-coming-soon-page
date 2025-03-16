module Main exposing (main)

import Browser
import Html exposing (Html, button, div, form, h1, h2, input, main_, p, section, small, text)
import Html.Attributes exposing (class, classList, placeholder, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Regex


type alias Model =
    { email : String
    , error : String
    }


init : Model
init =
    { email = ""
    , error = ""
    }


type Msg
    = Email String
    | Submit


update : Msg -> Model -> Model
update msg model =
    case msg of
        Email result ->
            { model | email = result }

        Submit ->
            { model
                | error = validateEmail model.email
            }


view : Model -> Html Msg
view model =
    main_ []
        [ section []
            [ div []
                [ h1 [ class "title" ] [ text "PING" ]
                , h2 [ class "sub-heading" ] [ text "We are launching soon!" ]
                , p [ class "paragraph" ] [ text "Subscribe and get notified" ]
                ]
            , div []
                [ form [ onSubmit Submit ]
                    [ div []
                        [ input
                            [ type_ "text"
                            , placeholder "example@example.com"
                            , onInput Email
                            , value model.email
                            ]
                            []
                        , viewErrorMsg model.error
                        ]
                    , button
                        [ type_ "submit" ]
                        [ text "Notify Me" ]
                    ]
                ]
            ]
        , text model.email
        ]


viewErrorMsg : String -> Html msg
viewErrorMsg errorMsg =
    small [] [ text errorMsg ]


pattern =
    "^[\\w-\\.]+@([\\w-]+\\.)+[\\w-]{2,4}$"


isValidEmail : Regex.Regex
isValidEmail =
    Maybe.withDefault Regex.never <| Regex.fromString pattern


validateEmail : String -> String
validateEmail email =
    if String.length email == 0 then
        "Whoops! It looks like you forgot to add your email"

    else if Regex.contains isValidEmail email == False then
        "Please provide a valid email address"

    else
        ""


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
