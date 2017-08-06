
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Random


--main =
--  Html.beginnerProgram { model = model, view = view, update = update }
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
    

left_quotes  : List String
left_quotes = 
    [ "left1"
    , "left2"
    , "left3"
    ]

right_quotes : List String
right_quotes = 
    [ "right1"
    , "right2"
    , "right3"
    ]

type Selection = Left | Right


-- MODEL

type alias Model = 
    { total:   Int
    , correct: Int
    , remain:  Int
    , l_pool:  List String
    , r_pool:  List String
    , quote:   Maybe String
    , answer:  Maybe Selection
    , previous:Maybe Bool
    }


init : (Model, Cmd Msg)
init =
    (
        { total   = 0
        , correct = 0 
        , remain  = 10
        , l_pool  = left_quotes
        , r_pool  = right_quotes
        , quote   = Nothing
        , answer  = Nothing
        , previous= Nothing
        }
        , Cmd.none
    )

-- UPDATE

type Msg = Correct | Incorrect | Reset | NewPrompt Bool

update2 : Msg -> Model -> Model
update2 msg model = 
    model


update : Msg -> Model -> (Model, Cmd Msg)

update msg model = 
    let (new_total, new_remain) =
        (model.total + 1, model.remain - 1)
    in
        case msg of 
            Correct -> 
                (
                    { model
                    | total   = new_total
                    , remain  = new_remain
                    , correct = model.correct + 1
                    }
                , Random.generate NewPrompt Random.bool
                )
            Incorrect -> 
                (
                    { model
                    | total   = new_total
                    , remain  = new_remain
                    }
                , Cmd.none
                )
            Reset -> 
                init 
            NewPrompt next -> 
                if next then
                    case model.l_pool of
                        [] -> init  -- should never happen
                        l_quote :: l_rest -> 
                            ( 
                                { model
                                | quote  = Just l_quote
                                , answer = Just Left
                                , l_pool = l_rest
                                }
                            , Cmd.none
                            )
                else 
                    case model.r_pool of
                        [] -> init  -- should never happen
                        r_quote :: r_rest ->
                            (
                                { model
                                | quote  = Just r_quote
                                , answer = Just Right
                                , r_pool = r_rest
                                }
                            , Cmd.none
                            )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ button [ onClick Incorrect ] [ text "Incorrect" ]
    , div [] [ text (toString model) ]
    , button [ onClick Correct ] [ text "Correct" ]
    , button [ onClick Reset ] [ text "Reset" ]
    ]


