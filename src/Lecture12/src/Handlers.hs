{-
  Этот модуль содержит функции для обработки API запросов.
  В MVC паттерне их можно рассматривать как контроллеры.
-}
module Handlers where

import Control.Monad.IO.Class (MonadIO)
import Servant.Server

import App
import DB.MovieSession
import DB.Seat
import DB.Preliminary
import DB.Booking
import Utils
import API.Types

getSessions :: MonadIO m => AppT m [MovieSession]
getSessions = getMovieSessions

getSeats :: MonadIO m => MovieSessionId -> AppT m [Seat]
getSeats = getSeatsBySessionId

postPreliminary :: MonadIO m => MovieSessionId -> SeatId -> AppT m BookingId
postPreliminary msId seatId = do
  bookings <- createPreliminary msId seatId
  case bookings of
    (b:_) -> pure $ bookingId b
    _ -> throwJSONError err404 $ JSONError "booking is not found"


refund :: MonadIO m => BookingId -> AppT m ()
refund bId = do
    booking <- getBook bId
    case booking of
        Booking _ sId _ _ _ : _ -> do
            res <- rmBooking sId bId
            return $ res
        _ -> throwJSONError err404 $ JSONError "booking is not found"

checkout :: MonadIO m => BookingId -> AppT m PaymentResponse
checkout bId = do
    booking <- tryBook bId
    case booking of
        Just f@(BookingFail Expired) -> do
            _ <- rmBookingOnly bId
            return f
        Just f@(BookingFail _) -> return f
        Just s@(BookingOk _ _ sId) -> do    
            _ <- checkoutBooking sId bId
            return s
        _ -> throwJSONError err404 $ JSONError "booking is not found"

