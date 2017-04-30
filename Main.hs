{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty

main :: IO ()
main = scotty 3000 $ do
  get "/" $ do
    html $ mconcat
      [ "<h1>Home</h1>"
      , "<p>Try going <a href=\"/hello\">Hello</a>.</p>"
      , "<p>Try going <a href=\"/beam\">To Star Trek land</a>.</p>"
      ]
  get "/hello" $ do
    html $ "<h1>Hello</h1>Go back <a href=\"/\">Home</a>."
  get "/:word" $ do
    beam <- param "word"
    html $ mconcat
      [ "<h1>Scotty, ", beam, " me up!</h1>"
      , "<a href=\"/\">Go back home</a>"
      ]
