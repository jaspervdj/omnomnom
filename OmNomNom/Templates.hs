-- | Module providing different templates for the site
--
{-# LANGUAGE OverloadedStrings #-}
module OmNomNom.Templates
    ( warning
    , blaze
    , root
    , OmNomNom.Templates.header
    , login
    , shop
    , cart
    ) where

import Control.Monad (forM_)
import Data.Monoid (mempty)

import Snap.Types
import Text.Blaze (Html)
import Text.Blaze.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes as A

import OmNomNom.Types

-- | A javascript element
--
javaScript :: Html -> Html
javaScript = script ! type_ "text/javascript"

-- | A warning
--
warning :: Html -> Html
warning = H.div ! class_ "warning"

-- | Send a blazehtml response to the browser
--
blaze :: Html -> Snap ()
blaze html = do
    modifyResponse $ addHeader "Content-Type" "text/html; charset=UTF-8"
    writeLBS $ renderHtml html

-- | Main template
--
root :: Html
root = docTypeHtml $ do
    H.head $ do
        H.title "omnomnom"
        javaScript ! src "/jquery-1.4.2.min.js" $ mempty
        javaScript ! src "/omnomnom.js" $ mempty
    H.body $ do
        javaScript "appendBody('/header');"
        javaScript "appendBody('/content');"

-- | Site header
--
header :: Html
header = h1 "omnomnom"

-- | Show a login box
--
login :: Html
login = H.div ! A.id "login" $ do
    H.form ! name "login-form" ! action "/login" ! A.method "post" $ do

        H.label ! for "login-form-name" $ "Name:"
        input ! type_ "text" ! name "login-form-name" ! A.id "login-form-name"
        br

        H.label ! for "login-form-password" $ "Password:"
        input ! type_ "password" ! name "login-form-password"
                                 ! A.id "login-form-password"
        br

        input ! type_ "submit" ! value "Create new user or login"

-- | Show the shop
--
shop :: [Product] -> Html
shop products = H.div ! A.id "shop" $ do
    a ! href "/logout" $ "Logout"
    ul $ forM_ products $ \product -> li $ do
        let name' = unProduct product
        p $ string $ name'
        H.form ! name "shop-product"
               ! onsubmit (stringValue $ "return order('" ++ name' ++ "');") $
            input ! type_ "submit" ! value "Get me one!"

-- | The cart with the user's products in
--
cart :: User -> Html
cart user = H.div ! A.id "cart" $ do
    p $ string $ "Cart for " ++ userName user
    ul $ forM_ (productCount $ userProducts user) $ \(product, count) -> li $ do
        p $ string $ unProduct product
        p $ string $ "Quantity: " ++ show count
