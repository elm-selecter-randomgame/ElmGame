module Main exposing (Action(..), Model, getPanel, init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Task
import Time



-- import Time exposing (Time)


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscription }


subscription : Model -> Sub Action
subscription model =
  if model.tick == 0  then
    if  model.clickTrue == 3 then
      Time.every 1000 OnClickTick
      else Sub.none
    else
    Time.every 1000 Tick


type alias Model =
    { -- color:List String,
      targetWord : List String --目标单词，定义列表String类型
    , originalWord : OriginalWord --基础单词

    -- 目标单词是否显示
    , tick : Int --记号
    , clickItems : List String
    , clickTrue : Int
    , clickTick : Int
    }


type alias OriginalWord =
    List Words



--定义列表Words类型


type alias Words =
    { index : String --单词索引
    , word : String --单词
    }



-- type clickItems=List


init : () -> ( Model, Cmd Action )
init _ =
    ({ targetWord = [ "plan", "car", "bus" ]
      , originalWord = [ { index = "1", word = "plan" }, { index = "2", word = "cat" }, { index = "3", word = "bus" }, { index = "4", word = "car" }, { index = "5", word = "dog" }, { index = "6", word = "rabbit" } ]
      , tick = 3000
      , clickItems = []
      , clickTrue = 0
      , clickTick = 1000
      }
    , Cmd.none

    )



--事件


type Action
    = ChangeColor String Int--给这个事件传个String类型的参数
    | Tick Time.Posix
    | OnClickTick Time.Posix
    -- | Reset Int
    -- | IfReset


update : Action -> Model -> ( Model, Cmd Action )
update msg model =
    case msg of
        ChangeColor word number->
          --点击3次后重置
          if model.clickTrue /= 3 then
              -- if model.clickTick < 0 then
              --     init ()
              --  else (model, Cmd.none)
              ( { model | clickItems = word :: model.clickItems, clickTrue = model.clickTrue + number }, Cmd.none )
            else
            (model, Cmd.none)

        --：：表示append
        Tick newtime ->
            ( { model | tick = model.tick - 1000 }, Cmd.none )

        OnClickTick newtime ->
          if model.clickTick == 0 then init ()
            else
          ({model | clickTick = model.clickTick - 1000 }, Cmd.none)

        -- Reset  number ->
        --   if model.clickTrue >= 3 then IfReset
        --     else
        --     ( {model | clickTrue = model.clickTrue + number},Cmd.none )

        -- IfReset  ->
        --   init ()



--点击三次正确之后重置 init()
-- reset : Model -> Model
-- reset model =
--   if List.member x.index model.clickItems then
--     if List.member x.word model.targetWord then
--       {model | clickTrue = model.clickTrue + 1}
--       if model.clickTrue >= 3 then
--         init ()

-- reset model =
--   if model.clickTrue >= 3 then
--     init ()
--     else model

getPanel : Model -> List (Html Action)
getPanel model =
    List.map
        (\x ->
            --\参数 ->返回值表达式 \a -> b
            button
                -- TODO 点击判断
                [ onClick (ChangeColor x.index 1)
                , style "color"
                    (setColor model x)

                --调用
                ]
                [ text x.word ]
        )
        model.originalWord


setColor model x =
    if List.member x.index model.clickItems then
        if List.member x.word model.targetWord then
            "green"

        else
            "red"

    else
        "black"
--重置
-- reset model x =
--     if List.member x.word model.targetWord then
--        1
--       else  1
-- reset ： Model -> Action
-- reset model  =
--   if model.clickTrue == 3 then ChangeColor

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
            (if model.tick > 0 then
                getPanel_ model

             else
                []
            )
        , --right side
          div [ class "right-side"  ] (getPanel model)
        ]
