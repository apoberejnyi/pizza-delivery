{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Feature.OrderOption.Gateway.DTO where

import           Data.Aeson
import           Data.List.NonEmpty
import           Data.Text
import           Data.UUID
import           Feature.OrderOption.Types
import           Gateway.Util
import           GHC.Generics
import           Prelude                 hiding ( id )

instance FromJSON OrderOptionDto
instance ToJSON OrderOptionDto
data OrderOptionDto = OrderOptionDto
    { id    :: UUID
    , name  :: Text
    , sizes :: NonEmpty SizeDto
    }
    deriving (Show, Generic)

instance FromJSON OrderOptionPayloadDto
instance ToJSON OrderOptionPayloadDto
data OrderOptionPayloadDto = OrderOptionPayloadDto
    { name  :: Text
    , sizes :: NonEmpty SizeDto
    }
    deriving (Show, Generic)

instance FromJSON SizeDto
instance ToJSON SizeDto
data SizeDto = SizeDto
    { diameter :: Double
    , cost     :: Double
    }
    deriving (Show, Generic)

instance FromDTO OrderOptionDto OrderOption where
  fromDTO OrderOptionDto {..} =
    OrderOption (OrderOptionId id) (Pizza name (fmap fromDTO sizes))

instance ToDTO OrderOptionDto OrderOption where
  toDTO (OrderOption (OrderOptionId id') (Pizza name' sizes')) =
    OrderOptionDto { id = id', name = name', sizes = fmap toDTO sizes' }

instance FromDTO OrderOptionPayloadDto OrderOptionPayload where
  fromDTO OrderOptionPayloadDto {..} = Pizza name (fmap fromDTO sizes)

instance ToDTO OrderOptionPayloadDto OrderOptionPayload where
  toDTO (Pizza name' sizes') =
    OrderOptionPayloadDto { name = name', sizes = fmap toDTO sizes' }

instance FromDTO SizeDto PizzaSize where
  fromDTO (SizeDto diameter cost) =
    PizzaSize (PizzaDiameter diameter) (PizzaCost cost)

instance ToDTO SizeDto PizzaSize where
  toDTO (PizzaSize (PizzaDiameter diameter') (PizzaCost cost')) =
    SizeDto { diameter = diameter', cost = cost' }
