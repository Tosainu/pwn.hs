import           Pwn

main :: IO ()
main = pwn $ do
  status  "status"
  success "success"
  failure "failure"
  debug   "debug"
  info    "info"
  warning "warning"
