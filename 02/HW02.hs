-- Собирать надо без -Wredundant-constraints (возникают проблемы с monadInMonad)

module HW02 where

import Control.Applicative (liftA2)
import Data.Char (toLower, isSymbol)
import Data.Functor (($>))

-- # 1 Инстансы полугрупп и моноидов

-- # 1.1  Показать, что тип ValueOpEx v является полугруппой
-- | (достаточно реализовать инстанс)

-- Рассмотрим тип данных ValueOp, параметризованный типом v.
-- | Подобные типы часто используются для алгебраического представления изменения значения по его ключу,
-- | например, в состоянии блокчейн-системы.
-- | Во-первых, значение может быть новым, для нового значения используется конструктор New.
-- | Во-вторых, мы можем изменить значение, соответствующее заданному ключу.
-- | В-третих, искомого значения может и не быть, для этого существует маркер NotExisted.
-- | Для удаления значения используется маркер Rem.

-- Бинарная операция над данным типом должна соотвествовать композиции двух изменений с состоянии по данному ключу.

data ValueOp v
  = New v       -- ^ новое значение
  | Upd v       -- ^ изменить значение
  | Rem         -- ^ удаление значение
  | NotExisted  -- ^ несуществущее значение

-- Проблема в том, что композиция некоторых конструктором немного ошибочка, например,
-- | композиция Upd и NotExisted не очень осмысленно, так как несуществующее значение
-- | не может быть обновлено.

-- Определим тип ValueOpEx v, данный тип изоморфен Maybe (ValueOpEx v).
-- Конструктор Op хранит значение типа (ValueOp v).
-- Err необходимо возвращать в тех случаях, когда композиция двух изменений не осмыслена,
-- как в примере выше.


-- Задача: реализовать инстанс полугруппы для типа (ValueOpEx v), где v - переменная типа.

-- Реализаций данного инстанса может быть несколько,
-- поэтому стоит в комментариях описывать, почему при тех или иных значениях на входе,
-- вы возвращаете тот или иной результат.

data ValueOpEx v
  = Op (ValueOp v)
  | Err

instance Semigroup (ValueOpEx v) where
-- Ошибка с любым выражением - ошибка
  Err             <> _               = Err
  _               <> Err             = Err
-- Создание элемента, а затем его обновление аналогично созданию уже обновленного элемента
  Op (New _)      <> Op (Upd v)      = Op (New v)
-- Создание элемента, а затем его удалению аналогично отсутствию действий
  Op (New _)      <> Op (Rem)        = Op (NotExisted)
-- Остальные случаи невалидны
  Op (New _)      <> _               = Err
-- Из двойного обновления оставляем последнее
  Op (Upd _)      <> Op (Upd v)      = Op (Upd v)
-- Обновление элемента, а затем удаление аналогично удалению
  Op (Upd _)      <> Op (Rem)        = Op (Rem)
-- Остальные случаи невалидны
  Op (Upd _)      <> _               = Err
-- Удаление и создание аналогично обновлению
  Op (Rem)        <> Op (New v)      = Op (Upd v)
-- После удаления может следовать NotExisted
  Op (Rem)        <> Op (NotExisted) = Op (Rem)
-- Остальные случаи невалидны
  Op (Rem)        <> _               = Err
-- NotExisted может следовать создание
  Op (NotExisted) <> Op (New v)      = Op (New v)
-- или NotExisted
  Op (NotExisted) <> Op (NotExisted) = Op (NotExisted)
-- Остальные случаи невалидны
  Op (NotExisted) <> _               = Err

-- # 1.2* Показать, что предложенная операция ассоциативна

-- (New a <> New b) <> New c = Err
-- New a <> (New b <> New c) = Err

-- (New a <> New b) <> Upd c = Err
-- New a <> (New b <> Upd c) = Err

-- (New a <> New b) <> Rem = Err
-- New a <> (New b <> Rem) = Err

-- (New a <> New b) <> NotExisted = Err
-- New a <> (New b <> NotExisted) = Err

-- (New a <> Upd b) <> New c = Err
-- New a <> (Upd b <> New c) = Err

-- (New a <> Upd b) <> Upd c = Upd c
-- New a <> (Upd b <> Upd c) = Upd c

-- (New a <> Upd b) <> Rem = NotExisted
-- New a <> (Upd b <> Rem) = NotExisted

-- (New a <> Upd b) <> NotExisted = Err
-- New a <> (Upd b <> NotExisted) = Err

-- (New a <> Rem) <> New c = New c
-- New a <> (Rem <> New c) = New c

-- (New a <> Rem) <> Upd c = Err
-- New a <> (Rem <> Upd c) = Err

-- (New a <> Rem) <> Rem = Err
-- New a <> (Rem <> Rem) = Err

-- (New a <> Rem) <> NotExisted = NotExisted
-- New a <> (Rem <> NotExisted) = NotExisted

-- (New a <> NotExisted) <> New c = Err
-- New a <> (NotExisted <> New c) = Err

-- (New a <> NotExisted) <> Upd c = Err
-- New a <> (NotExisted <> Upd c) = Err

-- (New a <> NotExisted) <> Rem = Err
-- New a <> (NotExisted <> Rem) = Err

-- (New a <> NotExisted) <> NotExisted = Err
-- New a <> (NotExisted <> NotExisted) = Err

-- (Upd a <> New b) <> New c = Err
-- Upd a <> (New b <> New c) = Err

-- (Upd a <> New b) <> Upd c = Err
-- Upd a <> (New b <> Upd c) = Err

-- (Upd a <> New b) <> Rem = Err
-- Upd a <> (New b <> Rem) = Err

-- (Upd a <> New b) <> NotExisted = Err
-- Upd a <> (New b <> NotExisted) = Err

-- (Upd a <> Upd b) <> New c = Err
-- Upd a <> (Upd b <> New c) = Err

-- (Upd a <> Upd b) <> Upd c = Upd c
-- Upd a <> (Upd b <> Upd c) = Upd c

-- (Upd a <> Upd b) <> Rem = Rem
-- Upd a <> (Upd b <> Rem) = Rem

-- (Upd a <> Upd b) <> NotExisted = Err
-- Upd a <> (Upd b <> NotExisted) = Err

-- (Upd a <> Rem) <> New c = Upd c
-- Upd a <> (Rem <> New c) = Upd c

-- (Upd a <> Rem) <> Upd c = Err
-- Upd a <> (Rem <> Upd c) = Err

-- (Upd a <> Rem) <> Rem = Err
-- Upd a <> (Rem <> Rem) = Err

-- (Upd a <> Rem) <> NotExisted = Rem
-- Upd a <> (Rem <> NotExisted) = Rem

-- (Upd a <> NotExisted) <> New c = Err
-- Upd a <> (NotExisted <> New c) = Err

-- (Upd a <> NotExisted) <> Upd c = Err
-- Upd a <> (NotExisted <> Upd c) = Err

-- (Upd a <> NotExisted) <> Rem = Err
-- Upd a <> (NotExisted <> Rem) = Err

-- (Upd a <> NotExisted) <> NotExisted = Err
-- Upd a <> (NotExisted <> NotExisted) = Err

-- (Rem <> New b) <> New c = Err
-- Rem <> (New b <> New c) = Err

-- (Rem <> New b) <> Upd c = Upd c
-- Rem <> (New b <> Upd c) = Upd c

-- (Rem <> New b) <> Rem = Rem
-- Rem <> (New b <> Rem) = Rem

-- (Rem <> New b) <> NotExisted = Err
-- Rem <> (New b <> NotExisted) = Err

-- (Rem <> Upd b) <> New c = Err
-- Rem <> (Upd b <> New c) = Err

-- (Rem <> Upd b) <> Upd c = Err
-- Rem <> (Upd b <> Upd c) = Err

-- (Rem <> Upd b) <> Rem = Err
-- Rem <> (Upd b <> Rem) = Err

-- (Rem <> Upd b) <> NotExisted = Err
-- Rem <> (Upd b <> NotExisted) = Err

-- (Rem <> Rem) <> New c = Err
-- Rem <> (Rem <> New c) = Err

-- (Rem <> Rem) <> Upd c = Err
-- Rem <> (Rem <> Upd c) = Err

-- (Rem <> Rem) <> Rem = Err
-- Rem <> (Rem <> Rem) = Err

-- (Rem <> Rem) <> NotExisted = Err
-- Rem <> (Rem <> NotExisted) = Err

-- (Rem <> NotExisted) <> New c = Upd c
-- Rem <> (NotExisted <> New c) = Upd c

-- (Rem <> NotExisted) <> Upd c = Err
-- Rem <> (NotExisted <> Upd c) = Err

-- (Rem <> NotExisted) <> Rem = Err
-- Rem <> (NotExisted <> Rem) = Err

-- (Rem <> NotExisted) <> NotExisted = Rem
-- Rem <> (NotExisted <> NotExisted) = Rem

-- (NotExisted <> New b) <> New c = Err
-- NotExisted <> (New b <> New c) = Err

-- (NotExisted <> New b) <> Upd c = New c
-- NotExisted <> (New b <> Upd c) = New c

-- (NotExisted <> New b) <> Rem = NotExisted
-- NotExisted <> (New b <> Rem) = NotExisted

-- (NotExisted <> New b) <> NotExisted = Err
-- NotExisted <> (New b <> NotExisted) = Err

-- (NotExisted <> Upd b) <> New c = Err
-- NotExisted <> (Upd b <> New c) = Err

-- (NotExisted <> Upd b) <> Upd c = Err
-- NotExisted <> (Upd b <> Upd c) = Err

-- (NotExisted <> Upd b) <> Rem = Err
-- NotExisted <> (Upd b <> Rem) = Err

-- (NotExisted <> Upd b) <> NotExisted = Err
-- NotExisted <> (Upd b <> NotExisted) = Err

-- (NotExisted <> Rem) <> New c = Err
-- NotExisted <> (Rem <> New c) = Err

-- (NotExisted <> Rem) <> Upd c = Err
-- NotExisted <> (Rem <> Upd c) = Err

-- (NotExisted <> Rem) <> Rem = Err
-- NotExisted <> (Rem <> Rem) = Err

-- (NotExisted <> Rem) <> NotExisted = Err
-- NotExisted <> (Rem <> NotExisted) = Err

-- (NotExisted <> NotExisted) <> New c = New c
-- NotExisted <> (NotExisted <> New c) = New c

-- (NotExisted <> NotExisted) <> Upd c = Err
-- NotExisted <> (NotExisted <> Upd c) = Err

-- (NotExisted <> NotExisted) <> Rem = Err
-- NotExisted <> (NotExisted <> Rem) = Err

-- (NotExisted <> NotExisted) <> NotExisted = NotExisted
-- NotExisted <> (NotExisted <> NotExisted) = NotExisted

-- #2. Еще немного моноидов и полугрупп

-- # 2.1.

-- Тип данных VerRes e a, параметризованный типами e и a изоморфен типу Either.
-- VErr хранит значение типа e, семантика -- возвращение ошибки в случае неудачного вычисления
-- VRes хранит значение типа a, успешный результат.

-- Показать, что тип (VerRes e a) является полугруппой,
-- | Доопределить полученную полугруппу до моноида,
-- | проверить, что полученная единица действительно является единицей

data VerRes e a
  = VErr e
  | VRes a
  deriving (Show, Eq)

instance Semigroup a => Semigroup (VerRes e a) where
  VErr a <> _      = VErr a
  _      <> VErr a = VErr a
  VRes a <> VRes b = VRes (a <> b)

instance Monoid a => Monoid (VerRes e a) where
  mempty = VRes mempty

testVerRes :: Bool
testVerRes = and
  [ verRes1 <> mempty == verRes2
  , VErr "some log" <> VRes ['a'..'z'] == VErr "some log"
  ]
  where
    verRes1 :: VerRes () [Char]
    verRes1 = VRes ['a'..'z'] <> VRes ['0'..'1']

    verRes2 :: VerRes () [Char]
    verRes2 = VRes $ ['a'..'z'] ++ ['0'..'1']

-- # 2.3.

-- Тип (BlockIntegrityVerifier block) -- это тип, параметризованный типом block.
-- | Данный тип является оберткой над типом функции из абстрактного типа block в VerRes e (),
-- где e -- это тип ошибки, а () (одноэлементный тип) -- тип успешной проверки,
-- в данном случае, проверки сходимости некоторых абстрактных блоков.

-- Задача: реализовать инстансы классов Semigroup и Monoid для типа (BlockIntegrityVerifier block)

-- Подсказка: пользоваться тем фактом, что VerRes a b - полугруппа (моноид).

newtype BlockIntegrityVerifier block
  = BIV { unBIV :: block -> VerRes String () }

instance Semigroup (BlockIntegrityVerifier block) where
  BIV a <> BIV b = BIV $ \x -> (a x) <> (b x)

instance Monoid (BlockIntegrityVerifier block) where
  mempty = BIV $ \x -> mempty

-- # 2.5. Реализовать инстансы моноида и полугруппы для типа Endo a,
-- | который является оберткой над типом функции из a в a.

newtype Endo a =
  Endo { runEndo :: a -> a }

instance Semigroup (Endo a) where
  Endo a <> Endo b = Endo $ a . b

instance Monoid (Endo a) where
  mempty = Endo id

-- # 3.

-- Рассмотрим тип MyCont r a, параметризованный типами r и a.
-- Данный тип - обертка над типов функции (a -> r) -> r.
-- Что в функциональном программировании еще часто называется монадой Cont.
-- | предназначенной для так называемых функций с продолжением.

newtype MyCont r a
  = MyCont { runCont :: (a -> r) -> r }

-- # 3.1 Реализовать инстанс функтора для типа MyCont r

instance Functor (MyCont r) where
  fmap f m = MyCont $ \x -> runCont m (x . f)

-- # 3.2 Реализовать инстанс аппликатива для типа MyCont r

instance Applicative (MyCont r) where
  pure v = MyCont $ \x -> x v
  (<*>) f m = MyCont $ \x -> runCont f (\g -> runCont m (\y -> x (g y)))

-- # 3.3 Реализовать инстанс монады для типа MyCont r

instance Monad (MyCont r) where
  (>>=) m f = MyCont $ \x -> runCont m (\y -> runCont (f y) x)




-- # 4.

-- Рассмотрим класс типов Monoidal f, который, на самом деле, изоморфен классу Applicative:

class Functor f => Monoidal f where
  munit :: f ()
  (<#>) :: f a -> f b -> f (a, b)

-- Например:

instance Monoidal Maybe where
  munit             = Just ()
  _ <#> Nothing     = Nothing
  Nothing <#> _     = Nothing
  Just a <#> Just b = Just (a, b)

instance Monoidal [] where
  munit     = [()]
  xs <#> ys = do
    x <- xs
    y <- ys
    return (x,y)

-- # 4.1. Выразить методы Applicative через Monoidal

pure'
  :: Monoidal f
  => a
  -> f a
pure' f = munit $> f

(<**>)
  :: Monoidal f
  => f (a -> b)
  -> f a
  -> f b
(<**>) f g = uncurry ($) <$> (f <#> g)

-- # 4.2. Выразить методы Monoidal через Applicative

munit'
  :: Applicative f
  => f ()
munit' = pure ()

(<##>)
  :: Applicative f
  => f a
  -> f b
  -> f (a, b)
(<##>) f g = (,) <$> f <*> g

testMonoidal :: Bool
testMonoidal =
  and [ ([succ, pred] <*> testList) == [succ, pred] <**> testList
      , (Just (4 :: Int) <##> Just (6 :: Int)) == (Just (4 :: Int) `appPair` Just (6 :: Int))
      , (Just (4 :: Int) `appPair` Just (6 :: Int)) == (Just (4 :: Int) <#> Just (6 :: Int))
      ]
  where
    testList :: [Int]
    testList = [1..10]
    appPair = liftA2 (,)

-- Если бы миром правили алгебраисты-теоретики,
-- | то монада в хаскелле вводилась бы следующим образом:

class Applicative m => AnotherMonad m where
  join :: m (m a) -> m a

-- # 4.3. Выразить AnotherMonad через Monad, иными словами,
-- | реализовать join методами класса типов Monad:

join'
  :: Monad m
  => m (m a)
  -> m a
join' m = m >>= id

-- # 4.4. Выразить монадический bind через AnotherMonad

anotherBind ::
  AnotherMonad m
  => m a
  -> (a -> m b)
  -> m b
anotherBind m f = join (f <$> m)

-- # 4.5. Реализовать альтернативную монаду списка:

instance AnotherMonad [] where
  join m = concat m

-- # 4.6. Реализовать альтернативую монаду Maybe:

instance AnotherMonad Maybe where
  join Nothing         = Nothing
  join (Just Nothing)  = Nothing
  join (Just (Just a)) = Just a

-- # 5 Реализовать функции через do-нотацию

-- # 5.1

foldM
  :: Monad m
  => (a -> b -> m a)
  -> a
  -> [b]
  -> m a
foldM _ a []     = return a
foldM f a (x:xs) = do r <- foldM f a xs
                      f r x

-- # 5.2

bothM
  :: Monad m
  => (a -> m b)
  -> (a -> m c)
  -> m a
  -> m (b, c)
bothM f g m = do a <- m
                 b <- f a
                 c <- g a
                 return (b, c)

-- Дальше ничего интересного.

newtype Sum
  = Sum { runSum :: Int }
  deriving (Show, Eq, Ord)

plusSum
  :: Sum
  -> Sum
  -> Sum
plusSum (Sum a) (Sum b) = Sum $ a + b

instance Semigroup Sum where
  (<>) = plusSum

instance Monoid Sum where
  mempty = Sum 0

testM :: IO Bool
testM = do
  result1 <- foldM mappendM (Sum 0) (Sum <$> [1..10])
  result2 <- bothM fun1 fun2 (pure 1303)
  return $ result1 == Sum 55 && result2 == (2606, 1697809)
  where
    mappendM :: Sum -> Sum -> IO Sum
    mappendM = \x y -> return $ x <> y

    fun1, fun2 :: Int -> IO Int
    fun1 = \x -> return $ x + x
    fun2 = \x -> return $ x * x

-- # 5.3.

newtype ListT m a
  = ListT { runListT :: m [a] }

monadInMonad
  :: (Monad m, Monad n, Monoid b)
  => (m b -> n b)
  -> (a -> b)
  -> ListT m a
  -> n b
monadInMonad trans mor xsT = trans m
  where m = do as <- runListT xsT
               return $ foldr (<>) mempty (map mor as)

-- # 6

-- Рассмотрим класс MonadTrans.
-- MonadTrans позволяет делать новую монаду из существующей монады,
-- вкладывая в новую монаду все вычисления и действия из предыдущей монады.
-- Такой способ формирования новых монад называется трансформером монад,
-- и задается классом MonadTrans:

class MonadTrans n where
  lift :: Monad m => m a -> n m a

-- MonadTrans -- это класс с единственным методом, который берет значение в монаде m
-- и посылает его в новую монаду n.

-- Реализовать инстанс MonadTrans для следующих типов

-- # 6.1. MaybeT

newtype MaybeT m a
  = MaybeT { runMaybeT :: m (Maybe a) }

instance MonadTrans MaybeT where
  lift m = MaybeT $ m >>= return . Just

-- # 6.2. ContT

newtype ContT r m a
  = ContT { runContT :: (a -> m r) -> m r }

instance MonadTrans (ContT r) where
  lift m = ContT (m >>=)

-- # 6.3. ListT

instance MonadTrans ListT where
  lift m = ListT $ do a <- m
                      return [a]

-- # 7 Рассахарить do-нотацию

-- # 7.1.

prodM
  :: Monad m
  => (a -> m b)
  -> (c -> m d)
  -> m (a, c)
  -> m (b, d)
-- prodM f g mac = do
--   (a, c) <- mac
--   b <- f a
--   d <- g c
--   return (b, d)
prodM f g mac =
  mac >>= \(a, c) ->
    f a >>= \b ->
      g c >>= \d ->
        return (b, d)

-- # 7.2.

compose
  :: Monad m
  => (b -> m c)
  -> (a -> m b)
  -> m a
  -> m c
-- compose fm gm xm = do
--   x <- xm
--   gx <- gm x
--   fm gx
compose fm gm xm =
  xm >>= \x ->
    gm x >>= \gx ->
      fm gx

-- # 7.3. Рассахарить list comprehension в do-нотацию

listFunction
  :: [a -> b -> c]
  -> [a -> b]
  -> [a]
  -> [c]
-- listFunction fxs gxs xs =
--   [ f x (g x) | f <- fxs, g <- gxs, x <- xs]
listFunction fxs gxs xs = do
  f <- fxs
  g <- gxs
  x <- xs
  return $ f x (g x)

-- # 7.4. Рассахарить do-нотацию через методы классы типа Monad

listFunction'
  :: [a -> b -> c]
  -> [a -> b]
  -> [a]
  -> [c]
listFunction' fxs gxs xs =
  fxs >>= \f ->
    gxs >>= \g ->
      xs >>= \x ->
        return $ f x (g x)

-- # 7.5. Реализовать ту же функцию, раскрыв использованные методы класса типов Monad
-- | в соотвествии с тем, как реализован представитель класса типов Monad для списков.

listFunction''
  :: [a -> b -> c]
  -> [a -> b]
  -> [a]
  -> [c]
listFunction'' fxs gxs xs =
  concat (map (\f -> concat (map (\g -> concat (map (\x -> return $ f x (g x)) xs)) gxs)) fxs)

listFunctionTest :: Bool
listFunctionTest =
  and [ listFunction fs gs vals  == listFunction' fs gs vals
      , listFunction' fs gs vals == listFunction'' fs gs vals
      ]
  where
    fs :: [Int -> Int -> Int]
    fs = [(+), (*), (-)]

    gs :: [Int -> Int]
    gs = [succ, pred]

    vals :: [Int]
    vals = [1..100]

-- # 8. Рассмотрим класс типов Contravariant, который является двойственным классу типов Functor

class Contravariant f where
  contramap :: (a -> b) -> f b -> f a

-- # 8.1
-- Реализовать инстанс класса типов Contravariant для однопараметрического типа Predicate a, который
-- является оберткой над одноместным предикатом, определенным на типе a

newtype Predicate a
  = Predicate { runPredicate :: a -> Bool }

instance Contravariant Predicate where
  contramap f (Predicate p) = Predicate $ p . f

predicateTest :: Bool
predicateTest =
  and [ (runPredicate $ contramap toLower (Predicate isSymbol)) '$' == True
      , (runPredicate $ contramap (`div` (49 :: Int)) (Predicate even)) 95 == False
      ]

-- # 8.2.

newtype Const a b
  = Const { runConst :: a }

instance Contravariant (Const a) where
  contramap _ (Const a) = Const a

-- # 8.3

newtype Compare a
  = Compare { runCompare :: a -> a -> Ordering }

instance Contravariant Compare where
  contramap f a = Compare $ \x y ->
    runCompare a (f x) (f y)

compareTest :: Bool
compareTest =
  and
    [ (runCompare $ contramap length (Compare compare)) numbers1 numbers2 == LT
    , (runCompare $ contramap mconcat (Compare compare)) listString1 listString2 == GT
    ]
  where
    numbers1 = [1..10] :: [Int]
    numbers2 = [11..29]
    listString1 = ["harold", " hide "]
    listString2 = [" the ", "pain"]

-- использовать функцию main для прогона тестов для ваших решений.
-- Тест устроен просто: елси тесты пройдены, то main вернет поздравление.
-- В противном случае, main попросит перепроверить решения.

main :: IO ()
main = do
  fourResult <- testM
  let hwTest = and
                [ testVerRes
                , fourResult
                , testMonoidal
                , listFunctionTest
                , predicateTest
                , compareTest
                ]
  case hwTest of
    True  -> putStrLn "Success! Good job!"
    False -> putStrLn "Something went wrong! Check your solutions, please."
  putStrLn ""
