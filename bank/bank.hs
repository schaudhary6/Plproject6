module Bank where

-- Import
import Control.Monad.State

-- BankOp monad 
newtype BankOp a = BankOp (State Float a)
    deriving (Functor, Applicative, Monad)

-- Deposit operation
deposit :: Float -> BankOp ()
deposit amount = BankOp $ modify (\balance -> balance + amount)

-- Withdraw operation
withdraw :: Float -> BankOp Float
withdraw amount = BankOp $ state (\balance ->
    let actualWithdraw = min amount (balance + 100) -- Allow overdrawing up to $100
    in (actualWithdraw, balance - actualWithdraw))

-- Get the current balance
getBalance :: BankOp Float
getBalance = BankOp get

-- Run a BankOp operation with an initial balance = 0
runBankOp :: BankOp a -> a
runBankOp (BankOp op) = evalState op 0
