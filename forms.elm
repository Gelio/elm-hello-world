import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as App
import Html.Events exposing (onInput, onClick)
import String exposing (isEmpty, length, toInt)
import Regex exposing (regex, contains)

main : Program Never
main = App.beginnerProgram { model = model, update = update, view = view }


type alias Model =
  { name : String
  , password : String
  , passwordRepeated : String
  , age : String
  , showValidation : Bool
  }

model : Model
model =
  { name = ""
  , password = ""
  , passwordRepeated = ""
  , age = ""
  , showValidation = False
  }

type Action
  = NameChange String
  | PasswordChange String
  | PasswordRepeatedChange String
  | AgeChange String
  | Submit



update : Action -> Model -> Model
update msg model =
  case msg of
    NameChange newName ->
      switchOffValidation { model | name = newName }

    PasswordChange newPassword ->
      switchOffValidation { model | password = newPassword }

    PasswordRepeatedChange newPassword ->
      switchOffValidation { model | passwordRepeated = newPassword }

    AgeChange newAge ->
      switchOffValidation { model | age = newAge }

    Submit ->
      { model | showValidation = True }

switchOffValidation : Model -> Model
switchOffValidation model = { model | showValidation = False }

view : Model -> Html Action
view model =
  let
    containerStyle = style
      [ ("margin-bottom", "0.5em")
      , ("padding-bottom", "0.5em")
      , ("border-bottom", "solid 1px #333")
      ]
  in
    div []
    [
      div [ containerStyle ] [
        label [ for "name" ] [ text "Name" ],
        br [] [],
        input [ id "name", type' "text", onInput NameChange ] []
      ],
      div [ containerStyle ] [
        label [ for "password" ] [ text "Password" ],
        br [] [],
        input [ id "password", type' "password", onInput PasswordChange ] []
      ],
      div [ containerStyle ] [
        label [ for "passwordRepeated" ] [ text "Repeat password" ],
        br [] [],
        input [ id "passwordRepeated", type' "password", onInput PasswordRepeatedChange ] []
      ],
      div [ containerStyle ] [
        label [ for "age" ] [ text "Age" ],
        br [] [],
        input [ id "age", type' "text", onInput AgeChange ] []
      ],
      div [ containerStyle ] [
        button [ onClick Submit ] [ text "Submit" ]
      ],
      if model.showValidation then
        viewValidation model
      else
        br [] []
    ]

passwordPatternValid : String -> Bool
passwordPatternValid password
   = contains (regex "[A-Z]") password
  && contains (regex "[a-z]") password
  && contains (regex "\\d") password

ageValid : String -> Bool
ageValid age =
  case toInt age of
    Err msg ->
      False
    Ok ageInt ->
      ageInt > 0 && ageInt < 130


viewValidation : Model -> Html Action
viewValidation model =
  let
    (color, message) =
      if not (ageValid model.age) then
        ("red", "Age is invalid")
      else if isEmpty model.password then
        ("red", "You must provide a password")
      else if length model.password < 8 then
        ("orangered", "Your password must be at least 8 characters long")
      else if not (passwordPatternValid model.password) then
        ("orangered", "Your password must contain at least one upper case letter, lower case letter and a number")
      else if model.password /= model.passwordRepeated then
        ("red", "Passwords must match")
      else
        ("green", "OK")
  in
    div [ style [ ("color", color) ] ] [
      text message
    ]
