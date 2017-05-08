{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty

main :: IO ()
main = scotty 3000 $ do
  get "/" $ do
    html $ mconcat
      [ "<h1>Hello</h1>"
      , "<p>Try going to <a href=\"/welcome\">Welcome</a>.</p>"
      , "<p>Try going to <a href=\"/beam\">Trekkie land</a>.</p>"
      ]
  get "/welcome" $ do
    html $ "<h1>Welcome Barney</h1>Go back <a href=\"/\">Home</a>."
  get "/:word" $ do
    beam <- param "word"
    html $ mconcat
      [ "<h1>Scotty, ", beam, " me up!</h1>"
      , "<a href=\"/\">Go back home</a>"
      ]
