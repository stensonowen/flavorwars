
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Random
import Random.List


main :
    Program
        Never
        Model
        Msg

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


type alias Quote = 
    { text:   String
    , source: Selection
    }

all_quotes : List Quote
all_quotes = List.map (\q -> { text = q, source = Left }) left_quotes
    ++ List.map (\q -> { text = q, source = Right}) right_quotes



-- MODEL

type alias Model = 
    { total:   Int
    , correct: Int
    , remain:  Int
    , queue:   List Quote
    , current: Quote
    , answer:  Maybe Selection
    , previous:Maybe Result
    }


init : (Model, Cmd Msg)
init =
    (
        { total   = 0
        , correct = 0 
        , remain  = min num_questions (List.length all_quotes)
        , queue   = all_quotes
        , current = { text = "", source = Left }
        , answer  = Nothing
        , previous= Nothing
        }
        , Random.generate Setup (Random.List.shuffle all_quotes)
    )



-- UPDATE

type Msg = Guess Selection | Reset | Setup (List Quote)

update2 : Msg -> Model -> Model
update2 msg model = 
    model


update : Msg -> Model -> (Model, Cmd Msg)

update msg model = 
    case msg of 
        Guess guess -> 
            case model.queue of 
                [] -> 
                    init
                next :: rest -> 
                    if Just guess == model.answer then
                        (   { model
                            | total   = model.total + 1
                            , remain  = model.remain - 1
                            , correct = model.correct + 1
                            , previous= Just Correct
                            , current = next
                            , queue   = rest
                            }
                            , Cmd.none
                            )
                    else
                        (   { model
                            | total   = model.total + 1
                            , remain  = model.remain - 1
                            , previous= Just Incorrect
                            , current = next
                            , queue   = rest
                            }
                            , Cmd.none
                            )
        Setup quotes -> 
            case quotes of 
                [] -> (model, Cmd.none)
                first :: rest -> 
                    (   { model
                        | current = first
                        , queue   = rest
                        }
                        , Cmd.none
                        )
        Reset -> 
            init 




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
    , div [] [ text ("Who said it? '" ++ toString model.current.text ++ "'") ]
    , div [] [ text (toString model) ]
    , button [ onClick (Guess Right)] [ text right_name ]
    , button [ onClick Reset ] [ text "Reset" ]
    ]


