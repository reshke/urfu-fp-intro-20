module Lecture10.Reader where

import Data.List
import Data.Maybe
import Control.Monad.Reader
import Prelude hiding (id)

-- <Задачи для самостоятельного решения>

{-
  Задача: по имеющейся базе данных сфомировать набор рекламных писем.
  Для неженатого(-ой) персоны письмо должно иметь вид:

  Для мужчины:

"""
  Уважаемый Имя Отчество!
  Разрешите предложить Вам наши услуги.
"""

  Для женщины:

"""
  Уважаемая Имя Отчество!
  Разрешите предложить Вам наши услуги.
"""

  Семейным парам шлется одно письмо вида

"""
  Уважаемые Имя_мужа Отчество_мужа и Имя_жены Отчество_жены!
  Разрешите предложить вам наши услуги.
"""

-}

data Sex = Male | Female deriving (Show, Eq, Ord)

type PersonId = Int

data Person = Person
  { id :: Int
  , family :: String
  , name :: String
  , surname :: String
  , sex :: Sex
  , marriedBy :: Maybe Int
  } deriving (Show, Eq, Ord)

persons :: [Person]
persons =
  [ Person 1 "Иванов" "Иван" "Иванович" Male Nothing
  , Person 2 "Петров" "Петр" "Петрович" Male (Just 7)
  , Person 3 "Соловьева" "Алия" "Фаридовна" Female Nothing
  , Person 4 "Кузнецова" "Мария" "Ивановна" Female (Just 8)
  , Person 5 "Гринько" "Юлия" "Владимировна" Female Nothing
  , Person 6 "Кабанов" "Александр" "Романович" Male Nothing
  , Person 7 "Петрова" "Екатерина" "Алексеевна" Female (Just 2)
  , Person 8 "Кузнецов" "Евгений" "Семёнович" Male (Just 4)
  , Person 9 "Антонов" "Юрий" "Васильевич" Male Nothing
  ]

-- Поиск персоны по номеру
findById :: PersonId -> Reader [Person] (Maybe Person)
findById pId = do
                ps <- ask
                return $ find (\(Person id _ _ _ _ _) -> id == pId) ps 

processSingle :: Person -> String
processSingle (Person _ _ name surname sex _) = 
        case sex of
            Female -> "Уважаемая " ++ name ++ " " ++ surname ++ "!" ++ "\n" ++ "Разрешите предложить Вам наши услуги."
            Male -> "Уважаемый " ++ name ++ " " ++ surname ++ "!" ++ "\n" ++ "Разрешите предложить Вам наши услуги."


processPair :: Person -> Person -> String
processPair (Person humanId _ huName huSurname _ (Just mwId)) 
            (Person womanId _ woName woSurname _ (Just mhId)) = 
            if womanId == mwId && humanId == mhId then
                 "Уважаемые "++ huName ++ " " ++ huSurname ++ " и " ++ woName ++ " " ++ woSurname ++ "!" ++ "\n" ++ "Разрешите предложить вам наши услуги." 
            else
                 "no such pair"

processPair _ _ = "no such pair"

processPerson :: PersonId -> Reader [Person] (Maybe String)
processPerson pId = do
            p <- findById pId
            res <- case p of
                Just hu@(Person _ _ _ _ Male (Just id)) -> do 
                                            pairPerson <- findById id
                                            return $ case pairPerson of 
                                                Just wo@(Person _ _ _ _ Female _) -> return $ processPair hu wo
                                                _ -> return $ processSingle hu
                Just wo@(Person _ _ _ _ Female (Just id)) -> do 
                                            pairPerson <- findById id
                                            return $ case pairPerson of 
                                                Just hu@(Person _ _ _ _ Male _) -> return $ processPair wo hu
                                                _ -> return $ processSingle wo
                Just per -> return $ Just $ processSingle per
                _ -> return $ Nothing

            return $ res

processPersons :: [PersonId] -> [Maybe String]
processPersons pIds = do
              pId <- pIds
              return $ runReader (processPerson pId) persons

-- </Задачи для самостоятельного решения>
