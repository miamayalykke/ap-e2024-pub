import Data.Char (isDigit, ord)
import Control.Monad(guard, liftM, ap)

digitsToInt :: String -> Int
digitsToInt s = loop 1 $ reverse s
    where 
        loop _ [] = 0
        loop w (c: cs) =
            (ord c - ord '0') * w
                + loop (w * 10) cs


digitsToIntMaybe :: String -> Maybe Int
digitsToIntMaybe s = 
    digitsToInt . reverse <$> check [] s
    where 
        check acc (c : cs) =
            if isDigit c 
                then check (c : acc) cs
                else Nothing
        check acc [] = Just acc


readTwoInts :: String -> Maybe(Int, Int)
readTwoInts s = loop1 [] s
    where 
        loop1 acc (c:cs) = 
            if isDigit c
                then loop1 (c:acc) cs
                else
                    if c == ' '
                        then Just (digitsToInt $ reverse acc, 
                        0
                        )
                        else Nothing
        loop1 _ [] =
            Nothing
        loop2 acc (c:cs) =
            if isDigit c
                then loop2 (c:acc) cs
                else Nothing
        loop2 acc [] = Just $ digitsToInt $ reverse acc



                
readInt :: String -> Maybe(Int, String)
readInt s = do
    (digits, residual) <- check [] s
    guard $ not $ null digits
    pure
        (
            digitsToInt $ reverse digits,
            residual
        )
    where
        check acc (c : cs) =
            if isDigit c
                then check (c :acc) cs
                else Just (acc, (c : cs))
        check acc [] = Just (acc, [])

readTwoInts2 ::
    String ->
    Maybe ((Int, Int), String)

readTwoInts2 s = 
    case readInt s of 
        Nothing -> Nothing
        Just (x, s') ->
            case s' of
                ' ' : s'' ->
                    case readInt s'' of
                        Nothing -> Nothing
                        Just (y, s''') ->
                            Just ((x,y), s''')
                _ -> Nothing

readManyInts :: String -> Maybe ([Int], String)
readManyInts s =
    case readInt s of
        Nothing -> Nothing 
        Just (x,s') ->
            case s' of
                ' ' : s'' ->
                    case readManyInts s'' of
                        Nothing -> Just ([x], s'')
                        Just (xs, s''') ->
                            Just (x : xs, s''')
                _ -> Just ([x], s')

type Error = String 
data Parser a
    = Parser ((Int, String) -> Either Error (a,String))

instance Functor Parser where
    fmap = liftM

instance Applicative Parser where
    --pure :: a -> Parser a
    --
    -- x :: a
    --s :: String
    -- undefined :: Maybe (a, String)
    pure x =  Parser $ \s ->
        Just (x,s)
    (<*>) = ap

instance Monad Parser where 
    -- bind: composes two monadic operations
    -- the left hand type of bind is of type parser
    -- the right hand type of bind is of type
    -- m :: (String -> Maybe (a, String))
    -- f :: a -> Parser b
    -- f' == (String -> Maybe (b, String))
    -- undefined 
    Parser m >>= f =
        Parser $ \s ->
            case m s of 
                Nothing -> Nothing
                Just (x, s') -> 
                    let Parser f' = f x 
                    in f' s'


next :: Parser Char
next = Parser $ \s ->
    case s of 
        [] -> Nothing
        c:s' -> Just(c,s')

parserFailure :: Parser a
parserFailure = Parser $ \_ -> Nothing

choice :: [Parser a] -> Parser a
choice [] = Parser $ \_ -> Nothing
choice (Parser p:ps) = Parser $ \s ->
    case p s of 
        Nothing -> 
            let Parser g = choice ps
            in g s
        Just (x, s') -> Just (x,s')


--eof :: Parser()
--eof = Parser $ \s ->
--    case


runParser :: String -> Parser a -> Maybe a
runParser s (Parser f) =
    case f s of
        Nothing -> Nothing
        Just (x,_) -> Just x

parseDigit :: Parser Int
parseDigit = do
    c <- next
    if isDigit c
        then pure (ord c - ord '0')
        else parserFailure "expected digit"


parseTwoDigits :: Parser (Int, Int)
parseTwoDigits = do
    x <- parseDigit
    y <- parseDigit
    pure (x,y)


many :: Parser a -> Parser [a]
many p = 
    choice 
    [ do 
        x <- p
        xs <- many p
        pure $ x : xs,
    pure []
    ]

parseInt :: Parser Int
parseInt = do
    digits <- many parseInt
    pure $ loop 1 $ reverse digits
    where
        loop _ [] = 0
        loop w (c : cs) =
            c * w + loop (w * 10) cs

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
    c <- next
    if p c
        then pure c
        else parserFailure

spaces :: Parser ()
spaces = do
    _ <- many $ satisfy (== ' ')
    pure ()

parseTwoInts :: Parser (Int, Int)
parseTwoInts = do
    x <- parseInt
    spaces
    y <- parseInt
    pure (x,y)