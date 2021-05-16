module Lib
    ( someFunc
    ) where

import Text.XML.HXT.Core

someFunc :: IO ()
someFunc = do
  runX $ root [] [makeAds] >>> writeDocument [withIndent yes] "Ads.xml"
  return ()

makeAds :: ArrowXml a => a XmlTree XmlTree
makeAds
    = mkelem "Ads" [ sattr "formatVersion" "haskell", sattr "target" "Avito.ru" ]
        [ mkelem "Ad" []
          [ mkelem "Id" [] [ txt "Sayding_001" ]
          , mkelem "DateBegin" [] [ txt "16.05.2021 10:00" ]
          , mkelem "DateEnd" [] [ txt "18.05.2021 10:00" ]
          , mkelem "AdStatus" [] [ txt "Free" ]
          , mkelem "AllowEmail" [] [ txt "Да" ]
          , mkelem "ManagerName" [] [ txt "Илья" ]
          , mkelem "ContactPhone" [] [ txt "8 929 301-42-00" ]
          , mkelem "Address" [] [ txt "Омская область, Омск, ул. Осминина, 16" ]
          , mkelem "Category" [] [ txt "Ремонт и строительство" ]
          , mkelem "Condition" [] [ txt "Новое" ]
          , mkelem "GoodsType" [] [ txt "Стройматериалы" ]
          , mkelem "GoodsSubType" [] [ txt "Отделка" ]
          , mkelem "AdType" [] [ txt "Товар приобретен на продажу" ]
          , mkelem "Title" [] [ txt "Сайдинг" ]
          , mkelem "Description" [] [ constA description >>> mkCdata ]
          , mkelem "Price" [] [ txt "450" ]
          , mkelem "VideoURL" [] [ txt "http://www.youtube.com/watch?v=YKmDXNrDdBI" ]
          , mkelem "Images" [] $
              [ mkelem "Image" [ sattr "url" "http://img.test.ru/8F7B-4A4F3A0F2BA1.jpg" ] []
              , mkelem "Image" [ sattr "url" "http://img.test.ru/8F7B-4A4F3A0F2XA3.jpg" ] []
              ]
          ]
        ]  
  where description = " \
          \ <p>Описание:</p> \
          \ <ul> \
          \ <li>Сайдинг металлический, виниловый! \
          \ <li>Работаем без посредников! \
          \ <li>Свой цех! \
          \ <li>Монтаж в короткие сроки! \
          \ <li>Качество гарантируем! \
          \ <li>Скидки всем! \
          \ <li>Рассрочка до 20 мес, без банка! \
          \ <li>Замер и консультация специалиста бесплатно! \
          \ </ul>"
