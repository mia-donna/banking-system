module Main where

import Control.Concurrent
import System.Random

-- first define the types
type Winner = String
type Name = String
type AccountBalance =  Int

-- Customer Data type - record syntax
data Customer = Customer {
  name :: Name,
  accountBalance :: AccountBalance,
  accountNumber :: AccountNumber
} deriving (Eq, Show) 

--  DATATYPE FOR ACCOUNT NUMBER : algebraic datatype is where we just have a list of constants, an enumerated type
data AccountNumber = One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten  deriving (Show, Eq)

-- Function to assign (by mapping) each customer account a number
mapIntToCustomer :: Int -> AccountNumber
mapIntToCustomer n = case r of
      0 -> One
      1 -> Two
      2 -> Three
      3 -> Four
      4 -> Five
      5 -> Six
      6 -> Seven
      7 -> Eight
      8 -> Nine
      9 -> Ten 
    where r = mod n 10 

-- Randomly choose an account
randomGenerator :: IO AccountNumber
randomGenerator = do
    n <- randomIO :: IO Int
    let chooseRandomAccountNumber = mapIntToCustomer n
    return chooseRandomAccountNumber    

----TEST
---functions

deposit :: Customer -> Int -> IO (Customer)
withdraw :: Customer -> Int -> IO (Customer)
transfer :: Customer -> Customer -> Int -> IO (Customer, Customer)

{-
  Function Declarations
-}
-- amount must be greater than 0
deposit account amount
  | amount > 0 = return account { accountBalance =  (bal + amount)}
  | otherwise = return account
  where bal = accountBalance account

-- amount must be greater than 0
withdraw account amount
  | bal < amount = return account
  | amount > 0 = return account { accountBalance =  (bal - amount)}
  | otherwise = return account
  where bal = accountBalance account

-- transfer from an acount to another account a desired amount
transfer from to amount
  | amount <= 0 = return (from, to)
  | accountBalance from < amount = return (from, to)
  | otherwise = return ((from { accountBalance =  ((accountBalance from) - amount)}),(to { accountBalance =  ((accountBalance to) + amount)}))

-- WIPcreate an IO action for forkIO
process :: Customer -> MVar () -> MVar (Customer) -> IO () 
process customer free box = do
    f <- takeMVar free
   -- d1 <- diceThrow
    putMVar box (customer)
    threadDelay 100
    process customer free box

main :: IO ()
main = do
   -- Create an account named C1 with a 50 balance
  let c1 = Customer {name = "C1", accountBalance = 50, accountNumber = One }
  c1 <- deposit c1 10 -- Deposit $10 into C1
  print c1

  -- Create an account named C2 with a 20 balance
  let c2 = Customer {name = "C2", accountBalance = 20, accountNumber = Two }
  c2 <- deposit c2 20 -- Deposit $20 into C2
  print c2
  c2 <- withdraw c2 10 -- Withdraw $10 into C2
  print c2

  (c1, c2) <- transfer c1 c2 10 -- Transfer $10 from C1 into C2
  print c1
  print c2
  putStrLn $ "CREATING 10 CUSTOMERS... " 
-- Create an account named C3 with a 0 balance
  let c3 = Customer {name = "C3", accountBalance = 20, accountNumber = Three}
-- Create an account named C4 with a 0 balance
  let c4 = Customer {name = "C4", accountBalance = 20, accountNumber = Four}
-- Create an account named C5 with a 0 balance
  let c5 = Customer {name = "C5", accountBalance = 20, accountNumber = Five}
-- Create an account named C6 with a 0 balance
  let c6 = Customer {name = "C6", accountBalance = 20, accountNumber = Six}
-- Create an account named C7 with a 0 balance
  let c7 = Customer {name = "C3", accountBalance = 20, accountNumber = Seven}
-- Create an account named C8 with a 0 balance
  let c8 = Customer {name = "C8", accountBalance = 20, accountNumber = Eight}
-- Create an account named C9 with a 0 balance
  let c9 = Customer {name = "C9", accountBalance = 20, accountNumber = Nine}
-- Create an account named C10 with a 0 balance
  let c10 = Customer {name = "C10", accountBalance = 20, accountNumber = Ten}
  print c3
  print c4
  print c5
  print c6
  print c7
  print c8
  print c9
  print c10
  
  free <- newMVar ()
  random <- randomGenerator
  initial_random <- newMVar random
  winner <- newEmptyMVar
  putStrLn "************************************"
  putStrLn $ "Random account that will perform the transfer is: " ++ (show random)
  putStrLn "************************************"
  box <- newEmptyMVar
  
  forkIO (process c1 free box)


  w <- takeMVar winner
  putStrLn $ "THE WINNER IS " ++ w


  --free <- newMVar ()
  --box <- newEmptyMVar
  --forkIO (process c1 free box)


{-

-- Maps Int to Customer Account Numbers
mapIntToCustomer :: Int -> AccountNumber
mapIntToCustomer n = case r of
      0 -> One
      1 -> Two
      2 -> Three
      3 -> Four
      4 -> Five
      5 -> Six
      6 -> Seven
      7 -> Eight
      8 -> Nine
      9 -> Ten 
    where r = mod n 10 

-- RE diceThrow : randomly choose which account to deposit into
randomGenerator :: IO AccountNumber
randomGenerator = do
    n <- randomIO :: IO Int
    let chooseRandomAccountNumber = mapIntToCustomer n
    return chooseRandomAccountNumber

-- trying to get a function to get a random number between 10 - 50 for the transfer
    

process :: Name -> MVar () -> MVar (AccountNumber, Name) -> IO () 
process name free box = do
    f <- takeMVar free
    r1 <- randomGenerator
    putMVar box (r1, name)
    threadDelay 100
    process name free box

judge :: MVar AccountNumber -> MVar () -> MVar (AccountNumber, Name) -> MVar String -> IO () 
judge initial_random free box winner = do
    r1 <- takeMVar initial_random
    (r2, name) <- takeMVar box
    print $ "Account Name: " ++ name ++ " got " ++ show(r2)
    if r1 == r2 then do
        putStrLn "************************************"
        putStrLn $ " Account Name: " ++ name ++ " will be transfered funds from " ++ show(r2)
        putStrLn "************************************"
        putMVar winner name
    else do
        putMVar initial_random r1
        putMVar free ()
        judge initial_random free box winner

main :: IO ()
main = do
    free <- newMVar ()
    random <- randomGenerator
    initial_random <- newMVar random
    winner <- newEmptyMVar
    putStrLn "************************************"
    putStrLn $ "Random account that will perform the transfer is: " ++ (show random)
    -- putStrLn $ "That account belongs to:  " ++ name
    -- here can add "Random amount they will transfer is:" " and add a function to genrate a number between 10-50
    putStrLn "************************************"
    box <- newEmptyMVar

    forkIO (process "Mia" free box)
    forkIO (process "Tom" free box)
    forkIO (process "Sonia" free box)
    forkIO (process "Dudley" free box)
    forkIO (process "Massimo" free box)
    forkIO (process "Lily" free box)
    forkIO (process "Elsa" free box)
    forkIO (process "Donna" free box)
    forkIO (process "Franco" free box)
    forkIO (process "Luca" free box)

    forkIO (judge initial_random free box winner)

    w <- takeMVar winner
    putStrLn $ "THE WINNER IS " ++ w



-}



-- in MAIN could
-- 1. Add   10 Customers
-- 2. Create the Customer threads 
-- 3. 

-- for reading balance at the end can look something like this:
{-balance :: Account -> IO Int
balance account = readTVarIO account
-}


{-
To complete this exercise you need to implement the following functions:

openAccount - Called at the start of each test. Returns a BankAccount.
closeAccount - Called at the end of each test.
getBalance - Get the balance of the bank account.
updateBalance - Increment the balance of the bank account by the given amount.

-}