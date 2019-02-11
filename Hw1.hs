module HW01 where

import System.IO
import Data.Maybe

{-
1. Задача на лямбда-исчисление
1.1. Уберите скобки в следующем лямбда-терме, произвести редукцию. Расписать пошагово:
((λ p. (λ q. ((q (p r)) s))) ((q ((λ p. p) r)) s))

Solution:

((λ p. (λ q. ((q (p r)) s))) ((q ((λ p. p) r)) s))
((λ p. (λ q. ((q (p r)) s))) ((q (r)) s))
((λ p. (λ q. ((q (p r)) s))) ((q (r)) s))
((λ p. (λ q. ((q (p r)) s))) ((q r) s))
((λ p. (λ q. ((q (p r)) s))) (q r s))
(λ q. ((q ((q r s) r)) s))
λ q. ((q (q r s r)) s)
λ q. (q (q r s r) s)
λ q. q (q r s r) s

1.2. Аналогично:
((λ a. λ b. (λ x. x) b a (a b x) ((λ a. (λ b. a)) x)) (λ b. b)) [x := b]

Solution:

((λ a. λ b. (λ x. x) b a (a b x) ((λ a. (λ b. a)) x)) (λ b. b)) [x := b]
((λ a. λ b. b a (a b x) ((λ a. (λ b. a)) x)) (λ b. b)) [x := b]
((λ a. λ b. b a (a b x) (λ b. x)) (λ b. b)) [x := b]
((λ a. λ b. b a (a b x) (λ c. x)) (λ d. d)) [x := b]
(λ a. λ b. b a (a b x) (λ c. x)) (λ d. d) [x := b]
λ b. b (λ d. d) ((λ d. d) b x) (λ c. x) [x := b]
λ b. b (λ d. d) (b x) (λ c. x) [x := b]
λ e. e (λ d. d) (e x) (λ c. x) [x := b]
λ e. e (λ d. d) (e b) (λ c. b)

-}

{-
Правило редукции:
(λ x. M) N -> M [x := N]
Правила избавления от скобок:
1. M N P = (M N) P
2. λ x. λ y. M = λ x. (λ y. M)
3. λ x y. M = λ x. λ y. M
-}

-- 2

euclid :: Integer -> Integer -> Integer
euclid a 0 = a
euclid a b = euclid b (mod a b)

-- 3

eulerTotientIter :: Integer -> Integer -> Integer
eulerTotientIter n 0 = 0
eulerTotientIter n m = (if euclid n m == 1 then 1 else 0) + eulerTotientIter n (m - 1)

eulerTotient :: Integer -> Integer
eulerTotient n = eulerTotientIter n n

-- 4

exp :: Integer -> Integer -> Integer
exp n p =
    if p == 0
        then 1
    else if mod p 2 == 0
        then
            let a = Main.exp n (div p 2)
            in a * a
    else n * Main.exp n (p - 1)

-- 5

integrate
    :: (Double -> Double)
    -> Double
    -> Double
    -> Double
integrate func a b = integrateIter func a b 0
    -- if b - a > 0.001 then (integrate func a ((a + b) / 2)) + (integrate func ((a + b) / 2) b)
    -- else ((func a) + (func b)) * (b - a) / 2

integrateIter
    :: (Double -> Double)
    -> Double
    -> Double
    -> Double
    -> Double
integrateIter func a b n =
    let
        w = (b - a) / 100
        l = a + w * n
        r = a + w * (n + 1)
    in (func l + func r) * w / 2 + if n < 99 then integrateIter func a b (n + 1) else 0

{- 6. Заселить следующие типы термами: -}

-- # 6.1:

permute :: (a -> b -> c) -> b -> a -> c
permute func b a = func a b

-- # 6.2:

pairProd :: (a -> b) -> (c -> d) -> (a,c) -> (b,d)
pairProd funcOne funcTwo (a, c) = (funcOne a, funcTwo c)

-- # 6.3:

fix :: (a -> a) -> a
fix func = func (fix func)
-- подсказка к # 6.3: вспомнить про комбинатор неподвижной точки, о котором говорилось на второй лекции:

-- # 6.4

weirdFunction
    :: (d -> d -> b)
    -> (a -> b -> c)
    -> (d -> b)
    -> d -> b
weirdFunction funcDdb funcAbc funcDb d = funcDdb d d
    
-- 7

data CoList a = Nil | Snoc (CoList a) a
    deriving (Show, Eq)

{-7.1 Реализовать функцию, которая по ко-списку возвращает список -}

coListToList :: CoList a -> [a]
coListToList Nil = []
coListToList (Snoc a last) = (coListToList a) ++ [last]

{-7.2 Реализовать конкатенацию ко-списков.
Реализация функции должна удовлетворять следующему равенству:
listToCoList (coListConcat a b) = (listToColist a) ++ (listToColist b),
-}

coListConcat :: CoList a -> CoList a -> CoList a
coListConcat a Nil = a
coListConcat a (Snoc b bLast) = Snoc (coListConcat a b) bLast

{-
8. Определим тип деревьев с двоичным ветвлением
-}

data Tree a = Leaf | Node (Tree a) a (Tree a)
    deriving (Show, Eq)

-- # 8.1 Реализовать instance класса типов Functor для деревьев

instance Functor Tree where
    fmap func Leaf = Leaf
    fmap func (Node left value right) = Node (fmap func left) (func value) (fmap func right)

-- # 8.2. Реализовать функцию, которая возвращает список элементов дерева

treeToList :: Tree a -> [a]
treeToList Leaf = []
treeToList (Node left value right) = (treeToList left) ++ [value] ++ (treeToList right)

-- # 8.3 Аналогично для ко-списков

treeToCoList :: Tree a -> CoList a
treeToCoList Leaf = Nil
treeToCoList (Node left value right) = coListConcat (Snoc (treeToCoList left) value) (treeToCoList right)

{- # 8.4 Реализовать проверку на пустоту -}

isEmpty :: Tree a -> Bool
isEmpty Leaf = True
isEmpty (Node left value right) = False

{- # 9. В стандартной библиотеке языка Haskell определен двухпараметрический тип Either,
data Either a b = Left a | Right b, семантика которого похожа на семантику Maybe, который обсуждался на семинаре.
Если нужное нам вычисление закончилось хорошо, то мы кладем результат в Right (правое вложение), а если
вычисление закончилось не очень хорошо, то мы применяем Left, который, в отличие от Nothing, еще требует объекта
некоторого типа a
Пример:
divideEither :: (Fractional a, Eq a) => a -> a -> Either String a
divideEither a b =
    if b == 0 then (Left "cho ti delash, ne deli na nol' ples") else Right (a / b)
> divideEither 5 0
Left "cho ti delash, ne deli na nol' ples"
> divideEither 5 6
Right 0.8333333333333334
 -}

-- # 9.1 Заселить данный тип

eitherCommute :: Either a b -> Either b a
eitherCommute (Left a) = Right a
eitherCommute (Right b) = Left b

-- # 9.2 Аналогично

eitherAssoc :: Either a (Either b c) -> Either (Either a b) c
eitherAssoc (Left a) = Left (Left a)
eitherAssoc (Right (Left b)) = Left (Right b)
eitherAssoc (Right (Right c)) = Right c

{- 10. В Haskell определена также конструкция case of, которая позволяет делать паттерн-матчинг
внутри реализации функции.
Примеры:
caseOfListLength :: [a] -> Int
caseOfListLength xs = case xs of
    [] -> 0
    (x:xs) -> 1 + caseOfListLength xs
booleanImplication :: Bool -> Bool -> Bool
booleanImplication x y = case (not x || y) of
    True -> True
    False -> False
Реализовать через case of следующие функции -}

-- # 10.1

listSum :: Num a => [a] -> a
listSum xs = case xs of
    [] -> 0
    (x:xs) -> x + listSum xs

-- # 10.2

filterList :: (a -> Bool) -> [a] -> [a]
filterList predicate x = case x of
    [] -> []
    (x:xs) -> case (predicate x) of
        True -> (x : filterList predicate xs)
        False -> filterList predicate xs

-- # 10.3

safeHead :: [a] -> Maybe a
safeHead x = case x of
    [] -> Nothing
    (x:xs) -> Just x

-- # 10.4

distributivity :: (a, Either b c) -> Either (a, b) (a, c)
distributivity val = case val of
    (x, Left y) -> Left (x, y)
    (x, Right y) -> Right (x, y)

