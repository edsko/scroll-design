module Main where

import CmdLine
import ScrollDesign

scroll :: Scroll
scroll = Div "scroll" [
      -- 13cm
      Div "focalPoint" [
            SetSize 97
          $ Text "我成功的原因"
        ]

      -- 11cm
    , Div "greenText" [
            SetSize 83
          $ Text "也正在于此"]

    , Div "smallPrintTopRight" [
            SetSize 15
          $ Text "我的职业生涯中投失了九千球输掉了"
        ]

    , Div "smallPrintBottomRight" [
            SetSize 15
          $ Text "差不多三百场比赛还有二十六"
        ]

    , Div "smallPrintTopLeft" [
            SetSize 15
          $ Text "次队友把决胜球传给我结果我没投中"
        ]

    , Div "smallPrintBottomLeft" [
            SetSize 15
          $ Text "我的人生中失败过一次又一次"
        ]

      -- 2cm
    , Div "signature" [
            SetSize 10
          $ Text "德安书"
        ]

    , Div "stamp" []
    ]

main :: IO ()
main = do
    Options{..} <- getOptions
    writeFile optionsOutput $ renderHtml scroll
