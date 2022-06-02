{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-orphans #-}
module TextShow.Data.UUID where
import qualified Data.UUID as U
import TextShow

instance TextShow U.UUID where
    showb = showtToShowb showt
    showt = U.toText
