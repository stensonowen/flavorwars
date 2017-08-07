
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
    

-- ALEX JONES 

left_quotes  : List String
left_quotes = 
    [ "Hello everybody. It’s now time for NPR, funded by the Rockefeller foundation. We’re going to talk real soft while we drag this knife across your carotid arteries."
    , "Gravity is bleeding into this universe. That's what they call Dark Matter"
    , "I DON'T LIKE 'EM PUTTIN' CHEMICALS IN THE WATER THAT TURN THE FRICKIN' FROGS GAY"
    , "We don't even notice hell itself rising up against us"
    , "I grew up in Dallas, Texas, drinking sodium fluoridated water. All the scientific studies show my IQ has been reduced by at least 20 points. The shadow of who I would have been calls out from the grave."
    , "I start thinking about Bill Gates, that little chicken neck, hopping around, little murdering eugenicist. 'You know I walk like a demonic elf. I'm Bill Gates! I'm gonna shoot you up with something that's gonna kill you down in a hammer. How's a 30 year death from gut disease sound, African children? Roll up the sleeves!"
    , "It is surreal to talk about issues, here on air, and then word-for-word hear Trump say it two days later."
    , "I like to eat. I like to have children."
    , "This is a human! This is what we look like! This is what we act like!"
    , "I regret that we mischaracterized Chobani, its employees, and the people of Twin Falls, Idaho the way we did."
    , "Alexa, do you work for the CIA?"
    , "Just like the Bible says. It's basically an intergalactic invasion into this space through people."
    , "They're frickin' interdimensional invaders, okay? I'll just say it. Make fun of me all you want."
    , "HILLARY CLINTON IS A GODDAMN DEMON"
    , "*in demon voice* 'Have the Pepsi taste testing systems be based on fetal tissue' ... That's Hillary"
    , "I came knocking on your door a million times and you laughed at me." 
    , "What is Venezuela?"
    , "Evil ain't cool and evil ain't fun."
    , "I am extremely proud that, in the Meme Wars that are going on in this country and worldwide, my show is probably the dominant material being used in all of this."
    , "The censors are letting [a meme] get through, because they think it's hurting me, when it's doing the opposite."
    , "We will take this piece of art, and we will basically take in the poison, and then, through alchemy, spit it back out in a way that will empower humanity"
    , "You will see people with green-colored skin. You will see people saying they kill their babies and they love Satan and they love being ugly and that they love stinking and howling and screaming all over the country, literally posessed by the modern spirit of anti-human garbage."
    ]



-- GUY FIERI

right_quotes : List String
right_quotes = 
    [ "... my brother from another mother -- Steve from Smash Mouth"
    , "If it tastes really good, and it's funky, it's funkalicous. If the guy making it is funky, he's funkintacious."
    , "Cooking is like snow skiing: If you don't fall at least 10 times, then you're not skiing hard enough."
    , "Cooking is all about people. Food is maybe the only universal thing that really has the power to bring everyone together. No matter what culture, everywhere around the world, people get together to eat."
    , "If you looked in my fridge, you'd see maybe 12 different mustards."
    , "There are three people you need in life: an accountant, a fishmonger, and a bail bondsman."
    , "I wake up in the morning thinking about food."
    , "I'm a culinary gangsta with a very spiritual side"
    , "Amateurs, losers, and idiots use lighter fluid"
    , "I wanna be the ambassador to Chimichanga Flavour Town."
    , "I'm a five-seasons griller... I don't care what the weather is like. My hair is impervious to any kind of dampness, so I don't have too much to worry about."
    , "Anybody that pays attention to hate really is wasting their time. I don't subscribe. I don't buy in."
    , "My spiked hair goes back about 15 years ago. I had long, curly rocker hair then. The woman who cuts my hair thought I needed a new style, so I let her surprise me. I flipped when I first saw it, but I soon realized the look was really me. I've always been a little crazy."
    , "I've always been an eccentric, a rocker at heart. I can't play the guitar, but I can play the griddle." 
    , "I would ride my horse to school."
    , "We’re Riding the Bus to Flavortown!"
    , "Holy [food item], Batman!"
    , "Welcome to the culinary dojo"
    , "If you're feeling adventurous, grill up some marinated octopus. It's so healthy."
    , "I'm not an actor. Lord knows, I'd be acting if I were an actor. What you see is what you get."
    , "No one likes rubbery chicken."

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

view : Model -> Html Msg
view model =
  div [ (align "center"), (height 100) ]
    ( 
        [ Html.h1 [] [ text "Who said it?" ]
        , Html.h2 [] [ text "Guy Fieri or Alex Jones?" ]
        , Html.img 
            [ src "https://itszn.com/u/b9d7e94f67b47d813eb91d2af92bc177f1d5608a.png"
            , alt "Credit to https://itszn.com"
            , style [("width", "50%")]
            ] 
        []
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
    if model.remain > 0 then
        case model.current of 
            Just quote -> 
                [ div [ style [("width", "50%")]  ] [ text ("\"" ++ quote.text ++ "\"") ] 
                , button [ onClick (Guess Left)] [ text left_name ]
                , Html.span [] [ text " or " ]
                , button [ onClick (Guess Right)] [ text right_name ]
                ]
            Nothing ->
                [ div [] [ text ("You got " ++ toString(model.correct) ++ " right out of " ++ toString(model.total)) ] ]
    else
        [ div [] [ text ("You got " ++ toString(model.correct) ++ " right out of " ++ toString(model.total)) ] ]
    ++
    [ div [] []
    , button [ onClick Reset ] [ text "Start over" ]
    ]
    )
