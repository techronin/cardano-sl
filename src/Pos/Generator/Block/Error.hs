-- | Errors which can happen during blockchain generation.

module Pos.Generator.Block.Error
       ( BlockGenError (..)
       ) where

import           Universum

import           Control.Exception   (Exception (..))
import qualified Data.Text.Buildable
import           Formatting          (bprint, stext, (%))

import           Pos.Core            (StakeholderId)
import           Pos.Crypto          (shortHashF)
import           Pos.Exception       (cardanoExceptionFromException,
                                      cardanoExceptionToException)

-- | Errors which can happen during blockchain generation.
data BlockGenError
    = BGUnknownSecret !StakeholderId
    -- ^ Generator needs secret key of given stakeholder, but it can't
    -- be found in the context.
    | BGFailedToCreate !Text
    -- ^ Block generator failed to create a block.
    | BGCreatedInvalid !Text
    -- ^ Block generator created invalid block.
    | BGInternal !Text
    -- ^ Internall error occurred.
    deriving (Show)

instance Buildable BlockGenError where
    build (BGUnknownSecret sId) =
        bprint
            ("Secret key of "%shortHashF%" is required but isn't known") sId
    build (BGFailedToCreate reason) =
        bprint ("Failed to create a block: "%stext) reason
    build (BGCreatedInvalid reason) =
        bprint ("Unfortunately, I created invalid block: "%stext) reason
    build (BGInternal reason) =
        bprint ("Internal error: "%stext) reason

instance Exception BlockGenError where
    toException = cardanoExceptionToException
    fromException = cardanoExceptionFromException
    displayException = toString . pretty
