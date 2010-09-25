-- | Module providing different templates for the site
--
{-# LANGUAGE OverloadedStrings #-}
module OmNomNom.Templates
    ( blaze
    , root
    , login
    , _shop
    , _cart
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

-- | Send a blazehtml response to the browser
--
blaze :: Html -> Snap ()
blaze html = do
    modifyResponse $ addHeader "Content-Type" "text/html; charset=UTF-8"
    writeLBS $ renderHtml html

-- | Main template
--
root :: Html -> Html
root inner = docTypeHtml $ do
    H.head $ do
        H.title "omnomnom"
        javaScript ! src "/jquery-1.4.2.min.js" $ mempty
        javaScript ! src "/omnomnom.js" $ mempty
    H.body $ do
        h1 "omnomnom"
        inner
        p "Footer here"

-- | Show a login box
--
login :: String -> Html
login message = root $ H.div ! A.id "login" $ do
    p $ string message

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
_shop :: [Product] -> Html
_shop products = H.div ! A.id "shop" $ do
    a ! href "/logout" $ "Logout"
    ul $ forM_ products $ \product -> li $ do
        let name' = unProduct product
        p $ string $ name'
        H.form ! name "shop-product"
               ! onsubmit (stringValue $ "return _cart('" ++ name' ++ "');") $
            input ! type_ "submit" ! value "Get me one!"

-- | The cart with the user's products in
--
_cart :: User -> Html
_cart user = H.div ! A.id "cart" $ do
    p $ string $ "Cart for " ++ userName user
    ul $ forM_ (productCount $ userProducts user) $ \(product, count) -> li $ do
        p $ string $ unProduct product
        p $ string $ "Quantity: " ++ show count
