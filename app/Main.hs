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

      -- 2cm
    , Div "smallPrint" [
            SetSize 15
          $ SetLimit [13 + 11, 6 + 9, 6, 13]
          $ Text $ concat [ "我的职业生涯中投失了九千球"
                          , "输掉了差不多三百场比赛"
                          , "还有二十六次"
                          , "队友把决胜球传给我"
                          , "结果我没投中"
                          , "我的人生中失败过一次又一次"
                          ]
        ]

      -- 2cm
    , Div "signature" [
            SetSize 15
          $ Text "德安书"
        ]

    , Div "stamp" []
    ]

main :: IO ()
main = do
    Options{..} <- getOptions
    writeFile optionsOutput $ renderHtml scroll
