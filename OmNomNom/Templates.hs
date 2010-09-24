-- | Module providing different templates for the site
--
{-# LANGUAGE OverloadedStrings #-}
module OmNomNom.Templates
    ( warning
    , blaze
    , root
    , OmNomNom.Templates.header
    , users
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

-- | Show the list of users
--
users :: [User] -> Html
users users = H.div ! A.id "users" $ do
    p "Select your name or add yourself to the list."
    ul $ forM_ users $ \user -> li $ do
        let name' = userName user
        a ! onclick (stringValue $ "login('" ++ name' ++ "');")
          ! href "#"
          $ string name'
    H.form ! name "add-user" ! onsubmit "return addUser();" $ do
        input ! type_ "text" ! name "add-user-name" ! A.id "add-user-name"
        input ! type_ "submit" ! value "Create new user"

-- | Show the shop
--
shop :: [Product] -> Html
shop products = H.div ! A.id "shop" $ do
    a ! onclick "logout();" ! href "#" $ "Logout"
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
    ul $ forM_ (userProducts user) $ \product -> li $
        p $ string $ unProduct product
