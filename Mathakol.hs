import System.Random (randomRIO, randomR, StdGen, getStdGen)

data Op a = Minus a | Plus a deriving (Show)

main :: IO ()
main = do
    first <- getRandomNumber
    print first
    answer <- getLine

    if answer /= "s"
        then mainLoop first []
        else return ()

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

    if answer /= "s"
        then mainLoop s ns
        else print $ calculate s ns

calculate :: Int -> [Op Int] -> Int
calculate n os = foldr applyOp n os
    where
        applyOp op acc = getOpFunc op $ acc
        getOpFunc op = case op of
            Plus x -> (+ x)
            Minus x -> (+ (-x))
