module Main (main) where

import Api

import Network.Wai.Middleware.Cors

main :: IO ()
main = run 8081 $ websocketsOr defaultConnectionOptions appWS appHTTP

