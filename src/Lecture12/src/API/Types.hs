  
module API.Types where

import Data.Aeson
import GHC.Generics

import DB.MovieSession
import DB.Seat (SeatId)

data PaymentResult = AlreadyPaid | Expired | PaidOK deriving (Eq, Show, Generic)

instance ToJSON PaymentResult
instance FromJSON PaymentResult

data PaymentResponse = BookingOk{ result :: PaymentResult, mId :: MovieSessionId, sId :: SeatId} | BookingFail { result :: PaymentResult }
  deriving (Eq, Show, Generic)

instance ToJSON PaymentResponse
instance FromJSON PaymentResponse

toRes :: MovieSessionId -> SeatId -> PaymentResponse
toRes m s = BookingOk PaidOK m s
