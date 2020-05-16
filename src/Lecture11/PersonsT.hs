{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Lecture11.PersonsT where

import Data.List
import Data.Maybe
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Prelude hiding (id)
import Lecture10.Reader (Person (..), PersonId, persons, processSingle, processPair, Sex(..))


-- <Задачи для самостоятельного решения>

{-
  В этом задании нужно адаптировать код из 10 лекции к новым требованиям
  при помощи трансформеров.

  1. Необходимо собирать статистику найденных людей:
    - количество одиноких персон
    - количество замужних персон
  2. В функцию `findById` добавить логгирование с помощью монады Write.
    Нужно сообщать была ли найдена персона по данному id или нет.

  Вы можете переиспользовать функции, которые остались без изменения из Lecture10.Reader
-}

data PersonSearchStats = PersonSearchStats
  { marriedPersonsCount :: Integer
  , singlePersonsCount :: Integer
  } deriving (Show)

emptyStats :: PersonSearchStats
emptyStats = PersonSearchStats 0 0

newtype PersonsT a = PersonsT
  { runPersonsT :: (ReaderT [Person] (StateT PersonSearchStats (Writer [String])) a) }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadWriter [String]
    , MonadState PersonSearchStats
    , MonadReader [Person]
    )

runPersons :: PersonsT a -> ((a, PersonSearchStats), [String])
runPersons p = runWriter . flip runStateT emptyStats . flip runReaderT persons . runPersonsT $ p

findById :: PersonId -> PersonsT (Maybe Person)
findById pId = do
    ps <- ask
    let found = find (\p -> id p == pId) ps
    case found of
            Just p -> tell ["Found: " ++ show p ]
            _ -> tell ["Failed to find person with id: " ++ show pId]
    return $ found

processPerson :: PersonId -> PersonsT (Maybe String)
processPerson pId = do
            put emptyStats
            p <- findById pId
            res <- case p of
                Just hu@(Person _ _ _ _ Male (Just id)) -> do
                                            pairPerson <- findById id
                                            res <- case pairPerson of
                                                Just wo@(Person _ _ _ _ Female _) -> do 
                                                        _ <- modify pairStat
                                                        return $ Just $ processPair hu wo
                                                _ -> do
                                                        _ <- modify singleStat 
                                                        return $ Just $ processSingle hu
                                            return $ res 
                Just wo@(Person _ _ _ _ Female (Just id)) -> do
                                            pairPerson <- findById id
                                            res <- case pairPerson of
                                                Just hu@(Person _ _ _ _ Male _) -> do 
                                                        _ <- modify pairStat   
                                                        return $ Just $ processPair wo hu
                                                _ -> do
                                                        _ <- modify singleStat 
                                                        return $ Just $ processSingle wo
                                            return res
                Just per -> do
                    _ <- modify singleStat 
                    return $ Just $ processSingle per
                _ -> return $ Nothing

            return $ res
            where 
                singleStat = \s@(PersonSearchStats mpc spc) -> s {marriedPersonsCount=mpc, singlePersonsCount=spc+1}
                pairStat = \s@(PersonSearchStats mpc spc) -> s {marriedPersonsCount=mpc+1, singlePersonsCount=spc}

{-
  Функция должна выводить на экран:
  - общую поисковую статистику по всем найденым персонам.
  - результат каждого поиска

  Записывать в "persons.log" общий лог всех поисков.
-}
processPersons :: [PersonId] -> IO ()
processPersons personIds = do
  let ((results, stats), logs) = runPersons $ mapM (\p -> do 
                                                        processed <- processPerson p
                                                        return $ (p, processed)) personIds

  mapM_ (\(i, res) -> putStrLn ("Found: " ++ show res ++ "; id: " ++ show i)) results
  putStrLn ("number of persons: \n" ++ show stats)
  writeFile "run.log" (show logs)

-- </Задачи для самостоятельного решения>
