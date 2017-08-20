module Main where

import Pwn

main :: IO ()
main = do
  putStrLn "- Pwn.Log ----------"
  status  "status"
  success "success"
  failure "failure"
  debug   "debug"
  info    "info"
  warning "warning"
