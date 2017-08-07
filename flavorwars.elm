
import Html exposing (Html, Attribute, button, div, text)
import Html.Attributes exposing (..)
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
left_name = "Alex Jones"

right_name : String
right_name = "Guy Fieri"


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
    , current: Maybe Quote
    , previous:Maybe Result
    }


init : (Model, Cmd Msg)
init =
    (
        { total   = 0
        , correct = 0 
        , remain  = (Basics.min num_questions (List.length all_quotes))
        , queue   = all_quotes
        , current = Nothing
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
            let queue = case model.queue of
                _ :: tail -> tail
                [] -> []
            in
                case model.current of 
                    Just current -> 
                        if guess == current.source then
                            (   { model
                                | total   = model.total + 1
                                , remain  = model.remain - 1
                                , correct = model.correct + 1
                                , previous= Just Correct
                                , current = List.head model.queue
                                , queue   = queue
                                }
                                , Cmd.none
                                )
                        else
                            (   { model
                                | total   = model.total + 1
                                , remain  = model.remain - 1
                                , previous= Just Incorrect
                                , current = List.head model.queue
                                , queue   = queue
                                }
                                , Cmd.none
                                )
                    _ -> 
                        init
        Setup quotes -> 
            case quotes of 
                [] -> (model, Cmd.none)
                first :: rest -> 
                    (   { model
                        | current = Just first
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

imgStyle : Attribute msg
imgStyle = 
    style
        [ ("height", "10") 
        , ("text-align", "center")
        ]

image_src : String
image_src = "https://itszn.com/u/b9d7e94f67b47d813eb91d2af92bc177f1d5608a.png"

view : Model -> Html Msg
view model =
  div [ (align "center"), (height 100) ]
    ( 
        [ Html.img [ src image_src
        , style [("width", "65%")]
        ] []
        ]
      ++
    [
    let 
        (color, message) = 
            case model.previous of 
                Nothing -> ("white", " ")
                Just Incorrect -> ("red", "Wrong")
                Just Correct -> ("green", "Correct")
    in 
          div [ style [("color", color)] ] [ text message ]
    ] 
    ++ 
    case model.current of 
        Just quote -> 
            [ div [] [ text (toString quote.text) ] 
            , button [ onClick (Guess Left)] [ text left_name ]
            , button [ onClick (Guess Right)] [ text right_name ]
            ]
        Nothing ->
            [ div [] [ text ("You got " ++ toString(model.correct) ++ " right out of " ++ toString(model.total)) ] ]
    ++
    [ div [] []
    , button [ onClick Reset ] [ text "Start over" ]
    ]
    ++
    [ Html.footer [] [ text "foooooooo" ] ]
    )


{-
imageStyle : Attribute msg
imageStyle = 
    style
        [ ("height", "10") 
        , ("text-align", "center")
        ]
-}
  {-
    [ ("width", "100%")
    , ("height", "40px")
    , ("padding", "10px 0")
    , ("font-size", "2em")
    , ("text-align", "center")
    ]
  -}
