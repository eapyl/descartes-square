module Main exposing (Msg(..), main, update)

import Browser
import Browser.Dom as Dom
import Element
    exposing
        ( Element
        , alignBottom
        , alignLeft
        , alignRight
        , centerX
        , centerY
        , column
        , el
        , fill
        , focusStyle
        , focused
        , height
        , inFront
        , maximum
        , newTabLink
        , none
        , padding
        , paddingEach
        , paddingXY
        , rgb255
        , row
        , shrink
        , spacing
        , text
        , width
        , wrappedRow
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (button, labelHidden)
import Html
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Task


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { route : Route
    , questions : List Question
    , uid : Int
    , language : Translation
    }


type Route
    = QuestionView QuestionType
    | Summary


type alias Question =
    { questionType : QuestionType
    , field : String
    , answers : List Answer
    }


type alias Answer =
    { id : Int
    , text : String
    }


type QuestionType
    = YesYes
    | YesNo
    | NoYes
    | NoNo


type Translation
    = En
    | Ru


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        Summary
        [ Question YesYes "" []
        , Question YesNo "" []
        , Question NoYes "" []
        , Question NoNo "" []
        ]
        0
        En
    , Cmd.none
    )


type Msg
    = ChangeAnswer Int String
    | AddAnswer Question
    | ChangeNewAnswer QuestionType String
    | ChangeLanguage Translation
    | ChangeView Route
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeAnswer id newText ->
            ( { model
                | questions = List.map (updateQuestionAnswer id newText) model.questions
              }
            , if String.isEmpty newText then
                changeFocusToNewAnswer

              else
                Cmd.none
            )

        AddAnswer question ->
            if String.isEmpty question.field then
                ( model, Cmd.none )

            else
                ( { model
                    | uid = model.uid + 1
                    , questions =
                        List.map
                            (\q ->
                                if q.questionType == question.questionType then
                                    newAnswerForQuestion model.uid q

                                else
                                    q
                            )
                            model.questions
                  }
                , changeFocusToNewAnswer
                )

        ChangeNewAnswer questionType newText ->
            ( { model
                | questions =
                    List.map
                        (\q ->
                            if q.questionType == questionType then
                                updateField newText q

                            else
                                q
                        )
                        model.questions
              }
            , Cmd.none
            )

        ChangeLanguage translation ->
            ( { model | language = translation }
            , Cmd.none
            )

        ChangeView route ->
            ( { model | route = route }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


changeFocusToNewAnswer : Cmd Msg
changeFocusToNewAnswer =
    Task.attempt (\_ -> NoOp) (Dom.focus newAnswerIdValue)


updateQuestionAnswer : Int -> String -> Question -> Question
updateQuestionAnswer id newText question =
    { question
        | answers =
            if String.isEmpty newText then
                List.filter (\a -> a.id /= id) question.answers

            else
                List.map
                    (\a ->
                        if a.id == id then
                            updateAnswer newText a

                        else
                            a
                    )
                    question.answers
    }


updateAnswer : a -> { b | text : a } -> { b | text : a }
updateAnswer newText answer =
    { answer | text = newText }


updateField : a -> { b | field : a } -> { b | field : a }
updateField txt quesiton =
    { quesiton | field = txt }


newAnswerForQuestion : Int -> Question -> Question
newAnswerForQuestion id question =
    { question | field = "", answers = question.answers ++ [ Answer id question.field ] }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Browser.Document Msg
view model =
    { title = "Descartes Square"
    , body =
        [ mainElement model
            |> Element.layoutWith
                { options = [ focusStyle { borderColor = Nothing, backgroundColor = Nothing, shadow = Nothing } ] }
                [ inFront info ]
        ]
    }


info : Element msg
info =
    newTabLink [ alignRight, alignBottom, Font.color blue, Font.size 32, padding 10 ]
        { url = "https://github.com/eapyl/descartes-square"
        , label = text "ℹ"
        }


languageChange : Element Msg
languageChange =
    row [ alignRight, spacing 5, padding 10 ]
        [ button [ Font.color white ] { onPress = Just (ChangeLanguage En), label = text "🇬🇧" }
        , button [ Font.color white ] { onPress = Just (ChangeLanguage Ru), label = text "🇷🇺" }
        ]


mainElement : Model -> Element Msg
mainElement model =
    let
        summaryView =
            column (inFront languageChange :: fillStyle)
                [ column fillStyle
                    [ row fillStyle
                        [ model.questions |> getQuestionByType YesNo |> getSquare model.language
                        , model.questions |> getQuestionByType YesYes |> getSquare model.language
                        ]
                    , row fillStyle
                        [ model.questions |> getQuestionByType NoNo |> getSquare model.language
                        , model.questions |> getQuestionByType NoYes |> getSquare model.language
                        ]
                    ]
                ]
    in
    case model.route of
        QuestionView questionType ->
            case getQuestionByType questionType model.questions of
                Just selectedQuestion ->
                    questionView model.language selectedQuestion

                Nothing ->
                    summaryView

        Summary ->
            summaryView


getQuestionByType : QuestionType -> List Question -> Maybe Question
getQuestionByType questionType questions =
    questions |> List.filter (\q -> q.questionType == questionType) |> List.head


getSquare : Translation -> Maybe Question -> Element Msg
getSquare language quesiton =
    case quesiton of
        Just { questionType, answers } ->
            let
                linkElement =
                    getTranslatedText language questionType
                        |> questionViewElement answers
                        |> wrapQuestionViewIntoButton questionType
            in
            case questionType of
                YesYes ->
                    el [ width fill, height fill, paddingEach { top = 0, left = 2, right = 0, bottom = 2 } ]
                        linkElement

                YesNo ->
                    el [ width fill, height fill, paddingEach { top = 0, left = 0, right = 2, bottom = 2 } ]
                        linkElement

                NoYes ->
                    el [ width fill, height fill, paddingEach { top = 2, left = 2, right = 0, bottom = 0 } ]
                        linkElement

                NoNo ->
                    el [ width fill, height fill, paddingEach { top = 2, left = 0, right = 2, bottom = 0 } ]
                        linkElement

        Nothing ->
            none


wrapQuestionViewIntoButton : QuestionType -> Element Msg -> Element Msg
wrapQuestionViewIntoButton questionType questionElement =
    button
        [ width fill
        , height fill
        , Background.color (questionSquareColor questionType)
        ]
        { label = questionElement
        , onPress = Just (ChangeView (QuestionView questionType))
        }


questionViewElement : List { a | text : String } -> Element msg -> Element msg
questionViewElement answers questionText =
    column fillStyle
        [ el [ centerX, styleQuestionTitle (List.isEmpty answers), Font.color white ] questionText
        , column [ paddingXY 20 0, spacing 5 ]
            (answers
                |> List.map .text
                |> List.map showAnswer
            )
        ]


styleQuestionTitle : Bool -> Element.Attribute msg
styleQuestionTitle center =
    if center then
        centerY

    else
        paddingXY 0 20


showAnswer : String -> Element msg
showAnswer answerText =
    el [ Font.color white ] (text answerText)


questionView : Translation -> Question -> Element Msg
questionView language question =
    column [ width fill, height fill, paddingXY 0 10, spacing 20, Background.color (questionColor question.questionType) ]
        [ row [ width fill ]
            [ el [ alignLeft ] backLink
            , el [ width fill ] (el [ centerX ] (getTranslatedText language question.questionType))
            ]
        , column [ centerX, width (fill |> maximum 800), spacing 10 ]
            (List.append
                (question.answers
                    |> List.map answerView
                )
                [ Input.text [ width fill, focused [], idAttr newAnswerIdValue, onEnter (AddAnswer question) ]
                    { text = question.field
                    , onChange = ChangeNewAnswer question.questionType
                    , placeholder = Nothing
                    , label = labelHidden ""
                    }
                ]
            )
        ]


backLink : Element Msg
backLink =
    button
        [ alignLeft
        , paddingXY 5 0
        , Font.size fontSizeQuestionText
        ]
        { label = el [ centerX, centerY ] (text "🔙")
        , onPress = Just (ChangeView Summary)
        }


answerView : Answer -> Element Msg
answerView answer =
    Input.text [ width fill ]
        { text = answer.text
        , onChange = ChangeAnswer answer.id
        , placeholder = Nothing
        , label = labelHidden ""
        }


onEnter : msg -> Element.Attribute msg
onEnter msg =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Decode.succeed msg

                        else
                            Decode.fail "Not the enter key"
                    )
            )
        )


newAnswerIdValue : String
newAnswerIdValue =
    "new-answer"


questionColor : QuestionType -> Element.Color
questionColor questionType =
    case questionType of
        YesYes ->
            rgb255 236 253 245

        YesNo ->
            rgb255 239 246 255

        NoYes ->
            rgb255 255 251 235

        NoNo ->
            rgb255 254 242 242


questionSquareColor : QuestionType -> Element.Color
questionSquareColor questionType =
    case questionType of
        YesYes ->
            green

        YesNo ->
            blue

        NoYes ->
            yellow

        NoNo ->
            red


getTranslatedText : Translation -> QuestionType -> Element msg
getTranslatedText translation questionType =
    case ( translation, questionType ) of
        ( En, YesYes ) ->
            Element.paragraph
                [ Font.size fontSizeQuestionText, Font.center ]
                [ text "What happens if this happens?"
                ]

        ( En, YesNo ) ->
            Element.paragraph
                [ Font.size fontSizeQuestionText, Font.center ]
                [ text "What if it "
                , el [ Font.bold ] <| text "doesn’t"
                , text " happen?"
                ]

        ( En, NoYes ) ->
            Element.paragraph
                [ Font.size fontSizeQuestionText, Font.center ]
                [ text "What "
                , el [ Font.bold ] <| text "will not"
                , text " happen if this happens?"
                ]

        ( En, NoNo ) ->
            Element.paragraph
                [ Font.size fontSizeQuestionText, Font.center ]
                [ text "What "
                , el [ Font.bold ] <| text "won’t"
                , text " happen if it "
                , el [ Font.bold ] <| text "doesn’t"
                , text "?"
                ]

        ( Ru, YesYes ) ->
            Element.paragraph
                [ Font.size fontSizeQuestionText, Font.center ]
                [ text "Что я "
                , el [ Font.bold ] <| text "получу"
                , text ", если "
                , el [ Font.bold ] <| text "приму"
                , text " это решение?"
                ]

        ( Ru, YesNo ) ->
            Element.paragraph
                [ Font.size fontSizeQuestionText, Font.center ]
                [ text "Что я "
                , el [ Font.bold ] <| text "получу"
                , text ", если "
                , el [ Font.bold ] <| text "не приму"
                , text " это решение?"
                ]

        ( Ru, NoYes ) ->
            Element.paragraph
                [ Font.size fontSizeQuestionText, Font.center ]
                [ text "Что я "
                , el [ Font.bold ] <| text "потеряю"
                , text ", если "
                , el [ Font.bold ] <| text "приму"
                , text " это решение?"
                ]

        ( Ru, NoNo ) ->
            Element.paragraph
                [ Font.size fontSizeQuestionText, Font.center ]
                [ text "Что я "
                , el [ Font.bold ] <| text "потеряю"
                , text ", если "
                , el [ Font.bold ] <| text "не приму"
                , text " это решение?"
                ]


fontSizeQuestionText : number
fontSizeQuestionText =
    24


idAttr : String -> Element.Attribute msg
idAttr idValue =
    Element.htmlAttribute (Html.Attributes.id idValue)


fillStyle : List (Element.Attribute msg)
fillStyle =
    [ width fill, height fill ]


white : Element.Color
white =
    rgb255 255 255 255


black : Element.Color
black =
    rgb255 0 0 0


green : Element.Color
green =
    rgb255 16 185 129


blue : Element.Color
blue =
    rgb255 59 130 246


yellow : Element.Color
yellow =
    rgb255 245 158 11


red : Element.Color
red =
    rgb255 239 68 68
