module Lib
    ( someFunc
    ) where

import Text.XML.HXT.Core

someFunc :: IO ()
someFunc = do
  runX $ root [] [helloWorld] >>> writeDocument [withIndent yes] "hello.xml"
  return ()

helloWorld :: ArrowXml a => a XmlTree XmlTree
helloWorld
    = mkelem "html" []
        [ mkelem "head" []
          [ mkelem "title" []
            [ txt "Hello World" ]     
          ]
        , mkelem "body"
            [ sattr "class" "haskell" ]
            [ mkelem "h1" [] [ txt "Hello World" ] ]
        ]  
