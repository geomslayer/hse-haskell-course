module RWS where

{-|
    __# 9 Монадический стек RWS__
    Все подпункты оцениваются одинаково, то есть за каждую задачу вида @9.a.b@
    даётся @n/6@ балла, где @n@ --- число баллов за всю задачу @9@.
-}

-- | __# 9.1 Reader__

-- | Зафиксируем некоторый тип @r@, заметим, что функции вида @r -> a@ являются
-- функтором, действительно:
--
-- @
-- instance Functor ((->) r) where
--   fmap = (.)
-- @
--
-- Поскольку инстанс @Functor@ для @((->) r@ определен
-- в "GHC.Base", воспользуемся типом-обёрткой:
--
-- @
-- newtype Reader r a = Reader { runReader :: r -> a }
-- @

-- | Семантика этого типа такова: вычисления, которые происходят в некотором
-- общем окружении @r@, которое можно локально изменять.

-- | При работе с монадой @'Reader' r@ удобно использовать следующие функции:
--
-- 1. @ask@ --- возвращает окружение,
--
-- 2. @local@ --- выполняет вычисление в модифицированном окружении.

newtype Reader r a = Reader { runReader :: r -> a }

-- | __Задача #9.1.1__: реализуйте инстансы @Functor@, @Applicative@ и @Monad@
-- для @'Reader' r@. Использование @deriving@ в каком-либо виде запрещено.

-- | __Задача #9.1.2__: реализуйте функции-помощники @ask@, @local@.

ask :: Reader r r
ask = Reader id

local
  :: (r -> r)
  -> Reader r a
  -> Reader r a
local f m = Reader $ runReader m . f

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader $ f . ra

instance Applicative (Reader r) where
  pure a = Reader $ \r -> a
  (<*>) (Reader rab) (Reader ra) = Reader $ \r ->
    rab r (ra r)

instance Monad (Reader r) where
  (>>=) (Reader ra) arb = Reader $ \r ->
    let (Reader rb) = arb (ra r)
    in rb r

-- | __#9.2 Writer__

-- | Семантика этого типа такова: Writer является оберткой над типом упорядоченной пары,
-- первым элементом которой является некоторый результат вычисления,
-- а вторым -- лог для актуального результата вычислений.

-- | __Задача #9.2.1__: реализуйте инстансы @Functor@, @Applicative@ и @Monad@
-- для @'Writer' w@. Использование @deriving@ в каком-либо виде запрещено.

-- | При работе с монадой @'Writer' w@ удобно использовать следующие функции:
--
-- 1. @tell@ --- записывает значение в @'Writer'@.
--
-- 2. @listen@ --- функция, заменяющая внутреннее состояние.
--
-- 3. @pass@ --- функция, изменяющая лог, но при этом сохраняет значение.

-- | __Задача #9.2.2__: реализуйте функции-помощники @tell@, @listen@ и @pass@.

newtype Writer w a
  = Writer { runWriter :: (a, w) }

tell
  :: Monoid w
  => w
  -> Writer w ()
tell w = Writer ((), w)

listen
  :: Monoid w
  => Writer w a
  -> Writer w (w, a)
listen m = Writer ((w, a), w) where
  (a, w) = runWriter m

pass
  :: Monoid w
  => Writer w (a, w -> w)
  -> Writer w a
pass m = Writer (a, f w) where
    ((a, f), w) = runWriter m

instance Functor (Writer w) where
  fmap f (Writer (a, w)) = Writer (f a, w)

instance Monoid w => Applicative (Writer w) where
  pure x = Writer (x, mempty)
  (Writer (f, a)) <*> (Writer (g, b)) = Writer (f g, mappend a b)

instance Monoid w => Monad (Writer w) where
  (Writer (a, w)) >>= f = Writer (a1, mappend w  w1) where
    (Writer (a1, w1)) = f a

-- | __#9.3 State__

-- | Часто бывает так, что нужно использовать состояние, которых, как известно,
-- в Haskell нет.

-- Для эмуляции состояния принято использовать монаду @'State' s@.

-- | Монада State является комбинацией монад Reader и Writer.

-- | __Задача #9.3.1__: реализуйте инстансы @Functor@, @Applicative@ и @Monad@
-- для @'State' s@. Использование @deriving@ в каком-либо виде запрещено.

-- | При работе с монадой @'State' s@ удобно использовать следующие функции:
--
-- 1. @get :: 'State' s a@ --- функция, возвращающая внутреннее состояние,
--
-- 2. @put :: s -> 'State' s ()@ --- функция, заменяющая внутреннее состояние.

-- | __Задача #9.3.2__: реализуйте функции-помощники @get@, @put@.

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ const ((), s)

newtype State s a
  = State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap g (State f) = State $ \s ->
    let (a, s1) = f s
    in  (g a, s1)

instance Applicative (State s) where
  pure a = State $ \s -> (a, s)
  (State f) <*> (State g) = State $ \s ->
    let (a, s1) = g s
        (h, s2) = f s1
    in  (h a, s2)

instance Monad (State s) where
  (State f) >>= g = State $ \s ->
    let (a, s1)   = f s
        (State h) = g a
    in  h s1
