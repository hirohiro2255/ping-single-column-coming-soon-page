module Main exposing (main)

import Browser
import Html exposing (Html, button, div, form, h1, h2, img, input, main_, p, section, small, text)
import Html.Attributes exposing (class, classList, placeholder, src, style, type_, value)
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
    main_ [ class "container" ]
        [ div [ class "container-wrapper" ]
            [ section []
                [ viewHeaderSection
                , viewFormSection model
                ]
            , section []
                [ img
                    [ class "image"
                    , src "src/assets/illustration-dashboard.png"
                    ]
                    []
                ]
            ]
        ]


viewHeaderSection : Html msg
viewHeaderSection =
    div []
        [ h1 [ class "title" ] [ text "PING" ]
        , h2 [ class "sub-heading" ] [ text "We are launching " ]
        , p [ class "paragraph" ] [ text "Subscribe and get notified" ]
        ]


viewFormSection : Model -> Html Msg
viewFormSection model =
    div [ class "form-container" ]
        [ form [ class "form", onSubmit Submit ]
            [ div [ class "email-input-wrapper" ]
                [ input
                    [ type_ "text"
                    , placeholder "example@example.com"
                    , onInput Email
                    , value model.email
                    , class "email"
                    , classList
                        [ ( "email", True )
                        , ( "input-error", not (String.length model.error == 0) )
                        ]
                    ]
                    []
                , viewErrorMsg model.error
                ]
            , button
                [ type_ "submit", class "button" ]
                [ text "Notify Me" ]
            ]
        ]


viewErrorMsg : String -> Html msg
viewErrorMsg errorMsg =
    let
        result =
            if String.length errorMsg == 0 then
                "hidden"

            else
                "visible"
    in
    small [ class "error-detail", style "visibility" result ] [ text errorMsg ]


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
