module Lecture05 where

{-
  05: Ленивость

  - Пример ленивых вычислений
  - Ленивость в других языках (yield, ||, &&)
  - Что такое ленивость
    - thunk
    - lazy evaluation
    - whnf, hnf, nf
  - Почему ленивость это хорошо
    - бесконечные списки
    - ленивость помогает компилятору оптимизировать код
  - Почему ленивость это плохо
    - space leaks
  - Как с этим бороться
    - (!, seq)
    - foldl, foldl', foldr
    - records strict fields
    - LANGUAGE strict, LANGUAGE strictdata
  - newtype lifting https://wiki.haskell.org/Newtype

  Подробнее:
    - https://www.fpcomplete.com/blog/2017/09/all-about-strictness
    - https://wiki.haskell.org/Foldr_Foldl_Foldl'
    - https://takenobu-hs.github.io/downloads/haskell_lazy_evaluation.pdf
-}


-- <Задачи для самостоятельного решения>
{-
    Напишите функцию, вычисляющую n-ое простое число с помощью
    решета Эротосфена (https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes).
    Используйте бесконечные списки.

    Функция `sieve` "просеивает" список, переданный в качестве аргумента,
    и возвращает другой список, но уже из простых чисел. Работает она так:
    берёт очередное число из списка и выкидывает из него все числа, которые делятся на это число.

    Чтобы с помощью `sieve` реализовать `nthPrime`, `sieve` должна работать на бесконечных списках.

    https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes#/media/File:Sieve_of_Eratosthenes_animation.gif
-}
sieve :: [Integer] -> [Integer]
sieve []       = []
sieve (x : xs) = x : sieve [ y | y <- xs, mod y x /= 0 ]

-- Функция, возвращающая n-ое простое число. Для её реализации используйте функцию sieve
nthPrime :: Int -> Integer
nthPrime n = last (take n (sieve [2..]))

{-
    Недавно в интервью Forbes с Сергеем Гуриевым Андрей Мовчан решил показать, что он
    тоже в некотором смысле математик, но немного запутался в рассуждениях о ВВП Китая и США:

        30 секунд с привязкой по времени: https://youtu.be/bTnnTeXHp8w?t=1782

    Помогите математику Андрею как программисты. Используя бесконечные списки, напишите функцию,
    которая вычислит через сколько лет ВВП на душу населения Китая догонит ВВП США.

    ВВП Китая $10к на душу населения, растёт на 6% в год
    ВВП США   $66к на душу населения, растёт на 2% в год.

    Можете воспользоваться функцией [iterate](https://hoogle.haskell.org/?hoogle=iterate).
    Она возвращает бесконечный список, каждый элемент которого получен из предыдущего
    применением первого аргумента ко второму:

        iterate f x = [x, f(x), f(f(x)), f(f(f(f))), ...]
-}

-- Возвращает бесконечный список ВВП на годы и годы вперёд
-- yearGDP 100 0.1 ~> [100, 100.1, 100.20009(9), 100.3003.., ...]
yearGDP :: Double -> Double -> [Double]
yearGDP now percent = now : yearGDP (now * (1 + percent / 100)) percent

-- Возвращает количество лет, которые нужны Китаю, чтобы догнать США в текущих условиях
inHowManyYearsChinaWins :: Int
inHowManyYearsChinaWins = error "not implemented"

{-
  Пусть у нас есть некоторая лента событий, каждое сообщение в которой говорит,
  сколько людей заболело в очередной стране. Нужно посчитать, сколько больных
  в каждой из перечисленных стран:

    stat [(China, 1000), (Italy, 47), (Russia, 14), (Italy, 98), (China, 107)] ~>
      [(China, 1107), (Russia, 14), (Italy, 145), (USA, 0), (GreatBritain, 0)]

    stat [(China, 80026)] ~>
      [(China, 80026), (Russia, 0), (Italy, 0), (USA, 0), (GreatBritain, 0)]

    В тестах есть случай с большими списками. Поэтому обычное решение будет
    неэффективным и нужно придумать как его улучшить с помощью строгих вычислений.
-}

data Country = Country String Integer deriving (Eq, Show)

allCountries :: [Country]
allCountries =
  [ Country "China" 0
  , Country "Russia" 0
  , Country "Italy" 0
  , Country "USA" 0
  , Country "GreatBritain" 0 ]

stat :: [Country] -> [Country]
stat []     = allCountries
stat events = [ Country "China" china
  , Country "Russia" rus
  , Country "Italy" italy
  , Country "USA" usa
  , Country "GreatBritain" gb ] where 
                                china = sum $ map(\y@(Country name c) -> if name == "China" then c else 0) events
                                rus   = sum $ map(\y@(Country name c) -> if name == "Russia" then c else 0) events
                                italy = sum $ map(\y@(Country name c) -> if name == "Italy" then c else 0) events
                                usa   = sum $ map(\y@(Country name c) -> if name == "USA" then c else 0) events 
                                gb    = sum $ map(\y@(Country name c) -> if name == "GreatBritain" then c else 0) events

--map(\y@(Country country_name _) -> (Country country_name (sum $ map (\other@(Country name count) -> if name == country_name then count else 0) events ))) allCountries
 
-- </Задачи для самостоятельного решения>
