{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pos.Binary.Size
       ( ExactSize(..)
       , ExactSized(..)
       , exactSize'
       ) where

import           Universum

-- | Exact size.
newtype ExactSize a = ExactSize {getExactSize :: Int}
    deriving (Eq, Show, Functor, Num)

instance Applicative ExactSize where
    pure _ = ExactSize 0
    (<*>) (ExactSize a) (ExactSize b) = ExactSize (a + b)

{- | A class for things that have constant size when serialized via 'Bi'.

We use a newtype with a phantom parameter because then we can do nifty things
like automatically getting correct size for compound types by writing:

@
    FooBar <$> exactSize <*> exactSize
@

instead of more error-prone:

@
    exactSize @Foo + exactSize @Bar
@
-}
class ExactSized a where
    exactSize :: ExactSize a

exactSize' :: forall a. ExactSized a => Int
exactSize' = getExactSize (exactSize @a)
