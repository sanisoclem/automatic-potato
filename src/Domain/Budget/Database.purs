module AP.Domain.Budget.Database where

import Prelude

import AP.Capability.Storage.Database (class DatabaseDocument, class DatabaseDocumentId, class DatabaseId, class DocumentId)
import AP.Data.Instant (Instant)
import Data.Argonaut (decodeJson, encodeJson, printJsonDecodeError)
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..))
import Effect.Exception (error)
import Safe.Coerce (coerce)

data BudgetDatabaseId
  = DbBudgerSummary BudgetSummaryId

data BudgetSummaryId = BudgetSummaryId

instance DatabaseId BudgetDatabaseId where
  dbIdString (DbBudgerSummary _) = "summary"
  dbIdFromString = case _ of
    "summary" -> Just $ DbBudgerSummary BudgetSummaryId
    _ -> Nothing

instance DatabaseDocumentId BudgetDatabaseId BudgetSummaryId where
  wrapDocumentId = DbBudgerSummary
  tryUnwrapDocumentId (DbBudgerSummary x) = Just x

type BudgetSummaryRecord =
  { name :: String
  , createdAt :: Instant
  }
newtype BudgetSummaryDocument = BudgetSummaryDocument BudgetSummaryRecord
unBudgetSummaryDocument :: BudgetSummaryDocument -> BudgetSummaryRecord
unBudgetSummaryDocument = coerce
budgetSummaryDocument :: BudgetSummaryRecord -> BudgetSummaryDocument
budgetSummaryDocument = coerce
instance DocumentId BudgetSummaryDocument BudgetSummaryId
instance DatabaseDocument BudgetSummaryDocument where
  decodeDocument json = lmap (error <<< printJsonDecodeError) <<< map BudgetSummaryDocument $ decodeJson json
  encodeDocument (BudgetSummaryDocument x) = encodeJson x
