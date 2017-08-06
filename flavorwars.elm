
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Random


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
type Result = Correct | Incorrect

num_questions : Int
num_questions = 10

left_name : String
left_name = "LEFT"

right_name : String
right_name = "RIGHT"

-- MODEL

type alias Model = 
    { total:   Int
    , correct: Int
    , remain:  Int
    , l_pool:  List String
    , r_pool:  List String
    , quote:   Maybe String
    , answer:  Maybe Selection
    , previous:Maybe Result
    }


init : (Model, Cmd Msg)
init =
    (
        { total   = 0
        , correct = 0 
        , remain  = num_questions
        , l_pool  = left_quotes
        , r_pool  = right_quotes
        , quote   = Nothing
        , answer  = Nothing
        , previous= Nothing
        }
        , Random.generate NewPrompt Random.bool
    )

-- UPDATE

type Msg = Guess Selection | Reset | NewPrompt Bool

update2 : Msg -> Model -> Model
update2 msg model = 
    model


update : Msg -> Model -> (Model, Cmd Msg)

update msg model = 
    case msg of 
        Guess guess -> 
            if Just guess == model.answer then
                (   { model
                    | total   = model.total + 1
                    , remain  = model.remain - 1
                    , correct = model.correct + 1
                    , previous= Just Correct
                    }
                    , Random.generate NewPrompt Random.bool
                    )
            else
                (   { model
                    | total   = model.total + 1
                    , remain  = model.remain - 1
                    , previous= Just Incorrect
                    }
                    , Random.generate NewPrompt Random.bool
                    )
        Reset -> 
            init 
        NewPrompt next -> 
            if next then
                case model.l_pool of
                    [] -> init  -- should never happen
                    l_quote :: l_rest -> 
                        (   { model
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
                        (   { model
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
    [ button [ onClick (Guess Left)] [ text left_name ]
    , div [] [ text (case model.previous of
        Nothing -> ""
        Just Incorrect -> "Wrong"
        Just Correct -> "Correct") ]
    , div [] [ text ("Who said it? '" ++ toString model.quote ++ "'") ]
    , div [] [ text (toString model) ]
    , button [ onClick (Guess Right)] [ text right_name ]
    , button [ onClick Reset ] [ text "Reset" ]
    ]


