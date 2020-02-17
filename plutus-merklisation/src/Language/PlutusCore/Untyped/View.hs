-- | Various views of PLC entities.

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.PlutusCore.Untyped.View
    ( IterApp(..)
    , TermIterApp
    , PrimIterApp
    , constantAsInteger
    , constantAsStagedBuiltinName
    , termAsBuiltin
    , termAsTermIterApp
    , termAsPrimIterApp
    ) where

import qualified Language.PlutusCore.Core         as PLC
import           Language.PlutusCore.Untyped.Term
import           PlutusPrelude

-- | A function (called "head") applied to a list of arguments (called "spine").
data IterApp head arg = IterApp
    { _iterAppHead  :: head
    , _iterAppSpine :: [arg]
    }

-- | An iterated application of a 'Term' to a list of 'Term's.
type TermIterApp name a =
    IterApp (Term name a) (Term name a)

-- | An iterated application of a 'BuiltinName' to a list of 'Value's.
type PrimIterApp name a =
    IterApp PLC.StagedBuiltinName (Value name a)

instance (PrettyBy config head, PrettyBy config arg) => PrettyBy config (IterApp head arg) where
    prettyBy config (IterApp appHead appSpine) =
        parens $ foldl' (\fun arg -> fun <+> prettyBy config arg) (prettyBy config appHead) appSpine

-- | View a 'Constant' as an 'Integer'.
constantAsInteger :: Constant a -> Maybe Integer
constantAsInteger (BuiltinInt _ int) = Just int
constantAsInteger _                  = Nothing

-- | View a 'Constant' as a 'StagedBuiltinName'.
constantAsStagedBuiltinName :: Builtin a -> PLC.StagedBuiltinName
constantAsStagedBuiltinName (BuiltinName    _ name) = PLC.StaticStagedBuiltinName  name
constantAsStagedBuiltinName (DynBuiltinName _ name) = PLC.DynamicStagedBuiltinName name

-- | View a 'Term' as a 'Constant'.
termAsBuiltin :: Term name a -> Maybe (Builtin a)
termAsBuiltin (Builtin _ bi) = Just bi
termAsBuiltin _              = Nothing

-- | An iterated application of a 'Term' to a list of 'Term's.
termAsTermIterApp :: Term name a -> TermIterApp name a
termAsTermIterApp = go [] where
    go args (Apply _ fun arg) = go (arg : args) fun
    go args  fun              = IterApp fun args

-- | View a 'Term' as an iterated application of a 'BuiltinName' to a list of 'Value's.
termAsPrimIterApp :: Term name a -> Maybe (PrimIterApp name a)
termAsPrimIterApp term = do
    let IterApp termHead spine = termAsTermIterApp term
    headName <- constantAsStagedBuiltinName <$> termAsBuiltin termHead
    -- This is commented out for two reasons:
    -- 1. we use 'termAsPrimIterApp' in abstract machines and we may not want to have this overhead
    -- 2. 'Error' is not a value, but we can return 'Error' from a failed constant application
    --    and then this function incorrectly returns 'Nothing' instead of indicating that an error
    --    has occurred or doing something else that makes sense.
    -- TODO: resolve this.
    -- guard $ all isTermValue spine
    Just $ IterApp headName spine