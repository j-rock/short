{-# LANGUAGE OverloadedStrings #-}

module Short.Pages
    (
      homePage
    , createdURL
    , couldntCreateURL
    , cantResolveURL
    , uhOh
    ) where


import           Data.Monoid                   ((<>))

import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Blaze.Html5              hiding (html, map, param)
import           Text.Blaze.Html5.Attributes   hiding (form, span, title)
import           Web.Scotty                    hiding (body, text)

import           Prelude                       hiding (div, head, id, lookup,
                                                span)

import           Short.State


-- let's you write the class cleaner
-- e.g. div ! class "blah" ===> cdiv "blah"
c :: (Html -> Html) -> AttributeValue -> Html -> Html
c h x = h ! class_ x

cdiv    = c div
cform   = c form
cspan   = c span
cbutton = c button

{-
   Turns nestedDivClasses ["c1", ..., "cn"] subHTML
   into <div class="c1">
          ...
            <div class="cn">
              subHTML
            </div>
          ...
        </div>
-}
nestedDivClasses :: [AttributeValue] -> Html -> Html
nestedDivClasses as h = foldl (flip ($)) h $ map cdiv as


actionHtml :: Html -> ActionM ()
actionHtml = html . renderHtml


headerFooter :: Html -> ActionM ()
headerFooter bodyHtml =
    actionHtml $ do docType

                    head $ do
                      meta ! charset "utf-8"
                      meta ! httpEquiv "X-UA-Compatible" ! content "IE=edge"
                      title "URL shortening service"
                      link ! rel "stylesheet" ! href "/css/bootstrap.min.css"

                    body $ do
                      nestedDivClasses [ "navbar navbar-default navbar-fixed-top"
                                       , "container"
                                       , "col-lg-6"
                                       , "navbar-header"
                                       ] "URL Shortener"
                      cdiv "container" $ bodyHtml


homePage :: ActionM ()
homePage = headerFooter $ do
             cdiv "bs-docs-section" $ do
               nestedDivClasses [ "row"
                                , "col-lg-12"
                                , "page-header"
                                ] $
                  h1 ! id "forms" $ "Give it a shot"

               nestedDivClasses [ "row"
                                , "col-lg-6"
                                , "well bs-component"
                                ] $ do
                 cform "form-horizontal" ! action "/s" ! method "post" $
                   fieldset $ do
                     cdiv "form-group" $
                       input ! class_ "form-control" ! type_ "text" ! name "url"

                     nestedDivClasses [ "form-group"
                                      , "col-lg-10 col-lg-offset-2"
                                      ] $ do
                       cbutton "btn btn-primary" ! type_ "submit" $ "Shorten it!"


jumbo :: Html -> Html -> Html -> Html
jumbo bigText medText buttonMsg =
    do nestedDivClasses ["row", "col-lg-6", "jumbotron"] $ do
         h1 bigText
         p  medText
         p $ a ! class_ "btn btn-primary btn-lg"
               ! href "/" $ buttonMsg


createdURL :: OriginalURL -> ShortenedURL -> ActionM ()
createdURL _ (URL su) =
    let shortHref = lazyTextValue su

        medText = do
           h4 "Your URL was successfully shortened"
           p $ do
             lazyText "You can reach it at:"
             a ! href shortHref $ toHtml su

    in headerFooter $ do
         jumbo "Congratulations" medText "Shorten another"


errorPage :: Html -> ActionM ()
errorPage explanation =
    headerFooter $ jumbo "Apologies" explanation "Go home"


uhOh :: ActionM ()
uhOh = errorPage "You are trying to reach a page that doesn't exist."


urlError :: Html -> URL a -> ActionM ()
urlError message (URL u) = errorPage $ message <> toHtml u

cantResolveURL :: ShortenedURL -> ActionM ()
cantResolveURL = urlError "Couldn't resolve this url: "


couldntCreateURL :: OriginalURL -> ActionM ()
couldntCreateURL = urlError "Couldn't shorten URL: "

