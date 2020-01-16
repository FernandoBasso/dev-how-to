--
-- Pattern Matching against data constructors, p 230.
--

module RegisteredUser where

newtype Username =
  Username String

newtype AccountNumber =
  AccountNumber Integer

data User =
    UnregisteredUser
  | RegisteredUser Username AccountNumber


printUser :: User -> IO ()
printUser UnregisteredUser =
  putStrLn "UnregisteredUser"
printUser (RegisteredUser -- <1>
           (Username name)
           (AccountNumber acctNum)) =
  putStrLn $ name ++ " " ++ show acctNum

main :: IO ()
main = do
  printUser UnregisteredUser

  printUser ru where
    u = Username "Leia Organa"
    a = AccountNumber 2001
    ru = RegisteredUser u a

--
-- <1>: Pattern-match on RegisteredUser and unpack its values into the
-- variables name and acctNum.
--

