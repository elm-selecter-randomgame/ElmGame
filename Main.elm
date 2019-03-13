module Main exposing (Action(..), Model, getPanel, init, main, update, view)

-- import List.Extra exposing (getAt, removeAt)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Random
import Random.List exposing (..)
import Task
import Time


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscription }


subscription : Model -> Sub Action
subscription model =
    if model.tick == 0 then
        if model.clickTrue == 3 then
            Time.every 1 OnClickTick

        else
            Sub.none

    else
        Time.every 1 Tick


type alias Word =
    String


type alias Words =
    List Word


type alias Model =
    { -- color:List String,
      targetWord : Words --目标单词，定义列表String类型
    , originalWord : Words --基础单词

    -- 目标单词是否显示
    , tick : Float --记号
    , clickItems : Words
    , fullWord : Words --txt文件里的所有单词
    , clickTrue : Int --点击次数
    , clickTick : Int --点击三次之后执行ClickTick当clickTick为0的时候重置
    , ifBigin : Bool -- 当为True的时候是准备界面，False的时候游戏界面
    , setTime : Float --onInput时先把时间储存起来
    }



-- type clickItems=List


init : () -> ( Model, Cmd Action )
init _ =
    ( { targetWord = []
      , originalWord = []
      , tick = 3 * 1000
      , clickItems = []
      , fullWord = []
      , clickTrue = 0
      , clickTick = 1000
      , ifBigin = True
      , setTime = 0
      }
    , Http.get
        { url = "source/word.txt" --本地txt文件
        , expect = Http.expectString GotWords
        }
    )


type Action
    = ChangeColor String Int --给这个事件传个String类型的参数
    | Tick Time.Posix
      -- | Reset
    | GotWords (Result Http.Error String) --发起请求，获得本地的txt文件
    | Twenty Words
    | Three Words
    | OnClickTick Time.Posix
    | IfBigin
    | SetTime String


update : Action -> Model -> ( Model, Cmd Action )
update msg model =
    case msg of
        ChangeColor word number ->
            --点击3次后重置
            if model.clickTrue /= 3 then
                -- if model.clickTick < 0 then
                --     init ()
                --  else (model, Cmd.none)
                ( { model | clickItems = word :: model.clickItems, clickTrue = model.clickTrue + number }, Cmd.none )

            else
                ( model, Cmd.none )

        --：：表示append
        Tick newtime ->
            ( { model | tick = model.tick - 1 }, Cmd.none )

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

        OnClickTick newtime ->
            if model.clickTick == 0 then
                ( { model
                    | targetWord = []
                    , originalWord = []
                    , tick = model.setTime
                    , clickItems = []
                    , fullWord = []
                    , clickTrue = 0
                    , clickTick = 1000
                    , ifBigin = False
                  }
                , Http.get
                    { url = "https://elm-selecter-randomgame.github.io/WordMemory/source/word.txt" --本地txt文件
                    , expect = Http.expectString GotWords
                    }
                )

            else
                ( { model | clickTick = model.clickTick - 1 }, Cmd.none )

        IfBigin ->
            ( { model | ifBigin = False, tick = model.setTime}, Cmd.none )

        SetTime newTick ->
            ({model | setTime = ( (Maybe.withDefault 100.0 (String.toFloat newTick)) * 1000)},Cmd.none)


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
                [ class "btn btn-default col-md-3"
                , onClick (ChangeColor x 1)
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
    List.map (\x -> button [ class "btn btn-default col-md-4" ] [ text x ]) model.targetWord


view : Model -> Html Action
view model =
    div [ class "container" ]
        [ div [ class "row" ]
            [ if model.ifBigin then
                div [ class "input-group col-md-12" ]
                    [ input [ placeholder "输入单词显示的时间 /s", onInput SetTime, class "form-control" ] []
                    , span [ class "input-group-btn" ]
                        [ button [ onClick IfBigin, class "btn btn-default" ] [ text "Begin" ]
                        ]
                    ]

              else
                div [ class "col-md-12" ]
                    [ --left side
                      div [ class "row" ]
                        -- (getPanel_ model)
                        (if model.tick > 0 then
                            getPanel_ model

                         else
                            []
                        )
                    , --right side
                      if model.tick > 0 then
                        div [ class "row" ] []

                      else
                        div [ class "row" ] (getPanel model)
                    ]
            ]
        ]
