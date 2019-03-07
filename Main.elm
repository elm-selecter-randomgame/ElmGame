module Main exposing (Action(..), Model, getPanel, init, main, update, view)

-- import List.Extra exposing (getAt, removeAt)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Random
import Random.List exposing (..)
import Task
import Time


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscription }


subscription : Model -> Sub Action
subscription model =
    Time.every 1000 Tick


type alias Word =
    String


type alias Words =
    List Word


type alias Model =
    { -- color:List String,
      targetWord : Words --目标单词，定义列表String类型
    , originalWord : Words --基础单词

    -- 目标单词是否显示
    , tick : Int --记号
    , clickItems : Words
    , fullWord : Words --txt文件里的所有单词
    }



-- type clickItems=List


init : () -> ( Model, Cmd Action )
init _ =
    ( { targetWord = []
      , originalWord = []
      , tick = 10000
      , clickItems = []
      , fullWord = []
      }
    , Http.get
        { url = "/source/word.txt" --本地txt文件
        , expect = Http.expectString GotWords
        }
    )


type Action
    = ChangeColor String --给这个事件传个String类型的参数
    | Tick Time.Posix
      -- | Reset
    | GotWords (Result Http.Error String) --发起请求，获得本地的txt文件
    | Twenty Words
    | Three Words


update : Action -> Model -> ( Model, Cmd Action )
update msg model =
    case msg of
        ChangeColor word ->
            ( { model | clickItems = word :: model.clickItems }, Cmd.none )

        --：：表示append
        Tick newtime ->
            ( { model | tick = model.tick - 1000 }, Cmd.none )

        -- Reset ->
        --     init ()
        GotWords result ->
            case result of
                Ok fullText ->
                    let
                        fullWords =
                            getWords fullText
                    in
                    ( { model | fullWord = fullWords }
                    , Random.generate Twenty (Random.List.shuffle fullWords)
                    )

                Err _ ->
                    ( model, Cmd.none )

        Twenty words ->
            -- let                originalWords=List.take 20 indexs
            let
                originalWords =
                    List.take 20 words
            in
            ( { model | originalWord = originalWords }, Random.generate Three (Random.List.shuffle originalWords) )

        Three words ->
            -- let               targetWords=List.take 3 indexs
            ( { model | targetWord = List.take 3 words }, Cmd.none )


getWords : String -> List String
getWords fullText =
    String.split "," fullText



--基础单词


getPanel : Model -> List (Html Action)
getPanel model =
    List.map
        (\x ->
            --\参数 ->返回值表达式 \a -> b
            button
                [ onClick (ChangeColor x)
                , style "color"
                    (getColor model x)

                --调用
                ]
                [ text x ]
        )
        model.originalWord


getColor model x =
    if List.member x model.clickItems then
        if List.member x model.targetWord then
            "green"

        else
            "red"

    else
        "black"



--left
--目标单词


getPanel_ : Model -> List (Html Action)
getPanel_ model =
    List.map (\x -> button [ class "targetWord" ] [ text x ]) model.targetWord


view : Model -> Html Action
view model =
    div [ class "contain" ]
        [ --left side
          div [ class "left-side" ]
            -- (getPanel_ model)
            (if model.tick > 0 then
                getPanel_ model

             else
                []
            )
        , --right side
          div [ class "right-side" ] (getPanel model)
        ]
