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

----TEST
---functions

deposit :: Customer -> Int -> IO (Customer)
withdraw :: Customer -> Int -> IO (Customer)
transfer :: Customer -> Customer -> Int -> IO (Customer, Customer)
printbal :: Customer -> Int

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


-- Print the Balance
printbal account = accountBalance account

main :: IO ()
main = do
   -- Create an account named B1 with a 0 balance
  let b1 = Customer {name = "B1", accountBalance = 50, accountNumber = One }
  b1 <- deposit b1 10 -- Deposit $10 into B1
  print b1

  -- Create an account named B2 with a 0 balance
  let b2 = Customer {name = "B2", accountBalance = 20, accountNumber = Two }
  b2 <- deposit b2 20 -- Deposit $20 into B2
  print b2
  b2 <- withdraw b2 10 -- Withdraw $10 into B2
  print b2

  (b1, b2) <- transfer b1 b2 10 -- Transfer $10 from B1 into B2
  print b1
  print b2


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