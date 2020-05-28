{-# LANGUAGE DeriveAnyClass #-}

module DB.Booking where

import Data.Aeson
import Data.Time
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField
import Servant.API
import GHC.Generics

import DB.MovieSession
import DB.Seat (SeatId)
import DB.Internal
import API.Types 

import Control.Monad.IO.Class


{-
  Тип для идентификатора бронирования
-}
newtype BookingId = BookingId
  { unBookingId :: Integer }
  deriving (Eq, Show)
  deriving ToRow via (Only Integer)
  -- ^ этот инстанс позволяет использовать `BookingId` с функциями для базы данных
  -- `via` говорит о том, что `BookingId` нужно использовать как `Integer`.
  -- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/deriving_via.html
  deriving (FromHttpApiData, FromField, ToField, FromJSON, ToJSON)
    via Integer
  -- ^ тоже самое для других классов

{-
  Record, описывающий таблицу `bookings`
-}
data Booking = Booking
  { bookingId :: BookingId
  , seatId :: SeatId
  , movieSessionId :: MovieSessionId
  , isPreliminary :: Bool
  , createdAt :: UTCTime
  } deriving (Eq, Show, Generic)
-- Класс Generic отвечает за универсальное кодирование типа, т.е. за  такое представление,
-- в котором используются конструкторы типов из ограниченного набора
-- https://hackage.haskell.org/package/base-4.14.0.0/docs/GHC-Generics.html
-- Это представление используется при выводе instance'ов других классов

deriving instance FromRow Booking
deriving instance ToRow Booking
-- ^ получаем возможность записывать и читать данные из базы данных с помощью `Booking`

instance ToJSON Booking
instance FromJSON Booking
-- ^ возможность для работы с JSON

{-
  Booking запрос должен проверить наличие предварительного бронирования.
  Если оно существует и прошло меньше 10 минут от создания, то бронирование
  проходит успешно, иначе необходимо вернуть сообщение об ошибке в JSON формате.
-}
getBook
  :: DBMonad m
  => BookingId
  -> m [Booking]
getBook bookingId = runSQL $ \conn ->
  query conn "SELECT * from bookings where id = ? " bookingId

checkoutBooking :: DBMonad m => SeatId -> BookingId -> m ()
checkoutBooking sId bId = runSQL $ \conn -> do
    _ <- execute conn "UPDATE seats SET available = false WHERE id = ?" sId
    _ <- execute conn "UPDATE bookings SET is_preliminary = false WHERE id = ?" bId
    return $ ()

rmBookingOnly :: DBMonad m =>BookingId -> m ()
rmBookingOnly bId = runSQL $ \conn -> do 
    _ <- execute conn "DELETE FROM bookings WHERE id = ?" bId
    return $ ()

rmBooking :: DBMonad m => SeatId -> BookingId -> m ()
rmBooking sId bId = runSQL $ \conn -> do 
    _ <- execute conn "UPDATE seats SET available = true WHERE id = ?" sId
    _ <- execute conn "DELETE FROM bookings WHERE id = ?" bId
    return $ ()

tryBook :: DBMonad m => BookingId -> m (Maybe PaymentResponse)
tryBook bookingId = do
    booking <- getBook bookingId
    case booking of
        (Booking _ seat movie isPreliminary created : _) -> do
            cTime <- liftIO $  getCurrentTime
            return $ Just $ if addUTCTime 600 created < cTime then BookingFail { result = Expired }
                                else if isPreliminary then toRes movie seat else BookingFail { result = AlreadyPaid }
        _ -> return Nothing
