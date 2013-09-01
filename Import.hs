module Import
    ( module Import
    , module Import.Map
    ) where

import           Prelude              as Import hiding (head, init, last,
                                                 readFile, tail, writeFile)
import           Yesod                as Import hiding (Route (..))

import           Control.Applicative  as Import (pure, (<$>), (<*>))
import           Data.Text            as Import (Text)

import           Foundation           as Import
import           Settings             as Import
import           Settings.Development as Import
import           Settings.StaticFiles as Import
import           Data.Map             as Import.Map (Map)
import           Data.Map.Strict      as Import.Map (empty)
import           Data.Maybe           as Import (fromMaybe, listToMaybe)
import           Data.String          as Import (IsString(..))
#if __GLASGOW_HASKELL__ >= 704
import           Data.Monoid          as Import
                                                 (Monoid (mappend, mempty, mconcat),
                                                 (<>))
#else
import           Data.Monoid          as Import
                                                 (Monoid (mappend, mempty, mconcat))

infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
#endif
