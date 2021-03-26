import System.Random (randomRIO, randomR, StdGen, getStdGen)
import Control.Monad (when)
import Control.Exception (catch, throwIO, AsyncException(UserInterrupt))

data Op a = Minus a | Plus a deriving (Show)

main :: IO ()
main = do
    first <- getRandomNumber
    print first

    mainLoop first []

getRandomNumber :: IO Int
getRandomNumber = randomRIO (1, 9)

getRandomOp :: Int -> StdGen -> Op Int
getRandomOp x g = op x
    where
        (n, _) = randomR (0, 1) g :: (Int, StdGen)
        op = if n == 1
                then Minus
                else Plus

mainLoop :: Int -> [Op Int] -> IO ()
mainLoop s os = do
    number <- getRandomNumber
    gen <- getStdGen
    let op = getRandomOp number gen
        ns = op:os
    print op
    answer <- getLine

    catch (mainLoop s ns)
          (\e -> if e == UserInterrupt
                    then print $ calculate s ns
                    else throwIO e)

calculate :: Int -> [Op Int] -> Int
calculate = foldr applyOp
    where
        applyOp op acc = getOpFunc op acc
        getOpFunc op = case op of
            Plus x -> (+ x)
            Minus x -> (+ (-x))
