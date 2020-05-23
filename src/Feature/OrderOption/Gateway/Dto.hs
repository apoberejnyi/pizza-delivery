{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module Feature.OrderOption.Gateway.Dto where

import Base.HTTP
import Data.Aeson
import Data.List.NonEmpty
import Data.Text
import Data.UUID
import Feature.OrderOption.Types
import GHC.Generics
import Prelude hiding (id)

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

instance DTO OrderOptionDto OrderOption where
    toModel OrderOptionDto{..} = OrderOption (OrderOptionId id) (Pizza name (fmap toModel sizes))
    fromModel (OrderOption (OrderOptionId id') (Pizza name' sizes')) = OrderOptionDto
        { id = id'
        , name = name'
        , sizes = fmap fromModel sizes'
        }

instance DTO OrderOptionPayloadDto OrderOptionPayload where
    toModel OrderOptionPayloadDto{..} = Pizza name (fmap toModel sizes)
    fromModel (Pizza name' sizes') = OrderOptionPayloadDto
        { name = name'
        , sizes = fmap fromModel sizes'
        }

instance DTO SizeDto PizzaSize where
    toModel (SizeDto diameter cost) = PizzaSize (PizzaDiameter diameter) (PizzaCost cost)
    fromModel (PizzaSize (PizzaDiameter diameter') (PizzaCost cost')) = SizeDto
        { diameter = diameter'
        , cost = cost'
        }
