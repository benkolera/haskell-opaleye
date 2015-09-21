{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Opaleye.Array where

import Opaleye.Constant (Constant,constant)
import Opaleye.Internal.Column (Column(Column),unColumn)
import Opaleye.PGTypes (PGArray,PGBool)
import Data.Profunctor.Product.Default (Default)
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ

arrayContains :: Column (PGArray a) -> Column (PGArray a) -> Column PGBool
arrayContains a b = Column (HPQ.BinExpr HPQ.OpContains (unColumn a) (unColumn b))

pgArray :: forall a b. Default Constant a (Column b) => [a] -> Column (PGArray b)
pgArray = Column . HPQ.ArrayExpr . fmap ( unColumn . (constant :: a -> Column b))
