module Main where

import CmdLine
import ScrollDesign

scroll :: Scroll
scroll = Elements [
        SetPos (Pos 125 300)
      $ SetSize 120
      $ SetAttr "font-size" ("90" `WithUnit` "pt")
      $ SetAttr "font-weight" (Unitless "bold")
      $ Text "我成功的原因"

    ,

        SetPos (Pos 55 270)
      $ SetSize 100
      $ SetAttr "font-size" ("70" `WithUnit` "pt")
      $ SetAttr "font-weight" (Unitless "bold")
      $ SetAttr "color" (Unitless "#CEDCC7")
      $ Text "也正在于此"

    ,

        SetPos (Pos 210 150)
      $ SetSize 11
      $ SetAttr "font-size" ("6" `WithUnit` "pt")
      $ SetLimit [13, 11, 6, 9, 6, 13]
      $ Text ( concat [ "我的职业生涯中投失了九千球"
                      , "输掉了差不多三百场比赛"
                      , "还有二十六次"
                      , "队友把决胜球传给我"
                      , "结果我没投中"
                      , "我的人生中失败过一次又一次"
                      ] )
    ]

main :: IO ()
main = do
    Options{..} <- getOptions
    writeFile optionsOutput $ renderHtml scroll
