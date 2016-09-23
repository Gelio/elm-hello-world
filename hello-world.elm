import Html exposing (Html, button, div, text, h1, input, br)
import Html.Attributes exposing (..)
import Html.App as App
import Html.Events exposing (onClick, onInput)
import String

main : Program Never
main = App.beginnerProgram { model = model, view = view, update = update }

-- MODEL

type alias Model = 
  {
    counter : Int,
    name : String
  }

model : Model
model = 
  {
    counter = 0,
    name = ""
  }



-- UPDATE

type Msg = Increment | Decrement | Reset | ChangeName String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment ->
      { model | counter = model.counter + 1 }
    
    Decrement ->
      { model | counter = model.counter - 1 }
    
    Reset ->
      { model | counter = 0 }
    
    ChangeName newName ->
      { model | name = newName }


-- VIEW
view : Model -> Html Msg
view model =
  let
    reversedName = "Your name reversed is " ++ (String.reverse model.name)
  in
    div []
      [
        button [ onClick Decrement ] [ text "-" ],
        div [] [ text (toString model.counter) ],
        button [ onClick Increment ] [ text "+" ],
        button [ onClick Reset ] [ text "Reset" ],

        div [] [
          h1 [] [ text "Enter your name" ],
          input [ placeholder "Your name", onInput ChangeName ] [],
          br [] [],
          text reversedName
        ]

      ]