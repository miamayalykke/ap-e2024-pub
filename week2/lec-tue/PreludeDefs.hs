data Maybe a = Nothing | Just a

class Functor f where
    -- f has kind * -> *
    fmap :: (a -> b) -> f a -> f b

{-
Functor laws:
    Identity:    fmap id = id
    Composition: fmap (f . g) = fmap f . fmap g
 -}

(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap

class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
    -- <*> is left-associative
    --ordered <$> Just 1 <*> Just 2 <*> Just 3 -> Just True    

{-
Applicative laws:
    Identity:       pure id <*> x = x
    Composition:    pure (.) <*> f <*> g <*> x = f <*> (g <*> x)
    Homomorphism:   pure f <*> pure x = pure (f x)
    Interchange:    f <*> pure x = pure (\g -> g x) <*> f
 -}

class Applicative m => Monad m where
    (>>=) :: m a -> (a -> m b) -> m b -- >>= means bind
    -- >>= is left-associative

{-
Monad laws:
    Left identity:  pure x >>= f = f x
    Right identity: m >>= pure = m
    Associativity:  m >>= (\x -> f x >>= g) = (m >>= f) >>= g
 -}
