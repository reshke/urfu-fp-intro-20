module API.Payment where

import Servant.API
import DB.Booking (BookingId)
import API.Types

type PaymentAPI
  = "api" :> "checkout" :> Capture "id" BookingId :> Get '[JSON] PaymentResponse
    -- ^ checkout requested booking id
  :<|> "api" :> "refund" :> Capture "id" BookingId :> Get '[JSON] ()
    -- ^ refund requested booking id

