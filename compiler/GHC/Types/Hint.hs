{-# LANGUAGE ExistentialQuantification #-}

module GHC.Types.Hint (
    GhcHint(..)
  , InstantiationSuggestion(..)
  , LanguageExtensionHint(..)
  , suggestExtension
  , suggestExtensionWithInfo
  , suggestExtensions
  , suggestExtensionsWithInfo
  , suggestAnyExtension
  , suggestAnyExtensionWithInfo
  ) where

import GHC.Prelude

import GHC.Utils.Outputable
import qualified GHC.LanguageExtensions as LangExt
import Data.Typeable
import GHC.Unit.Module (ModuleName, Module)
import GHC.Hs.Extension (GhcTc)
import GHC.Core.Coercion
import GHC.Types.Basic (Activation, RuleName)
import GHC.Parser.Errors.Basic
import {-# SOURCE #-} Language.Haskell.Syntax.Expr
  -- This {-# SOURCE #-} import should be removable once
  -- 'Language.Haskell.Syntax.Bind' no longer depends on 'GHC.Tc.Types.Evidence'.


data LanguageExtensionHint
  = -- | Suggest to enable the input extension. If the input 'SDoc'
    -- is not empty, it will contain some extra information about the
    -- why the extension is required, but it's totally irrelevant/redundant
    -- for IDEs and other tools.
     SuggestSingleExtension !SDoc !LangExt.Extension
    -- | Suggest to enable the input extensions. The list
    -- is to be intended as /disjuctive/ i.e. the user is
    -- suggested to enable /any/ of the extensions listed. If
    -- the input 'SDoc' is not empty, it will contain some extra
    -- information about the why the extensions are required, but
    -- it's totally irrelevant/redundant for IDEs and other tools.
  | SuggestAnyExtension !SDoc [LangExt.Extension]
    -- | Suggest to enable the input extensions. The list
    -- is to be intended as /conjunctive/ i.e. the user is
    -- suggested to enable /all/ the extensions listed. If
    -- the input 'SDoc' is not empty, it will contain some extra
    -- information about the why the extensions are required, but
    -- it's totally irrelevant/redundant for IDEs and other tools.
  | SuggestExtensions !SDoc [LangExt.Extension]

-- | Suggests a single extension without extra user info.
suggestExtension :: LangExt.Extension -> GhcHint
suggestExtension ext = SuggestExtension (SuggestSingleExtension empty ext)

-- | Like 'suggestExtension' but allows supplying extra info for the user.
suggestExtensionWithInfo :: SDoc -> LangExt.Extension -> GhcHint
suggestExtensionWithInfo extraInfo ext = SuggestExtension (SuggestSingleExtension extraInfo ext)

-- | Suggests to enable /every/ extension in the list.
suggestExtensions :: [LangExt.Extension] -> GhcHint
suggestExtensions exts = SuggestExtension (SuggestExtensions empty exts)

-- | Like 'suggestExtensions' but allows supplying extra info for the user.
suggestExtensionsWithInfo :: SDoc -> [LangExt.Extension] -> GhcHint
suggestExtensionsWithInfo extraInfo exts = SuggestExtension (SuggestExtensions extraInfo exts)

-- | Suggests to enable /any/ extension in the list.
suggestAnyExtension :: [LangExt.Extension] -> GhcHint
suggestAnyExtension exts = SuggestExtension (SuggestAnyExtension empty exts)

-- | Like 'suggestAnyExtension' but allows supplying extra info for the user.
suggestAnyExtensionWithInfo :: SDoc -> [LangExt.Extension] -> GhcHint
suggestAnyExtensionWithInfo extraInfo exts = SuggestExtension (SuggestAnyExtension extraInfo exts)

-- | A type for hints emitted by GHC.
-- A /hint/ suggests a possible way to deal with a particular warning or error.
data GhcHint
  =
    {-| An \"unknown\" hint. This type constructor allows arbitrary
    -- hints to be embedded. The typical use case would be GHC plugins
    -- willing to emit hints alongside their custom diagnostics.
    -}
    forall a. (Outputable a, Typeable a) => UnknownHint a
    {-| Suggests adding a particular language extension. GHC will do its best trying
        to guess when the user is using the syntax of a particular language extension
        without having the relevant extension enabled.

        Example: If the user uses the keyword \"mdo\" (and we are in a monadic block), but
        the relevant extension is not enabled, GHC will emit a 'SuggestExtension RecursiveDo'.

        Test case(s): parser/should_fail/T12429, parser/should_fail/T8501c,
                      parser/should_fail/T18251e, ... (and many more)

    -}
  | SuggestExtension !LanguageExtensionHint
    {-| Suggests that a monadic code block is probably missing a \"do\" keyword.

        Example:
            main =
              putStrLn "hello"
              putStrLn "world"

        Test case(s): parser/should_fail/T8501a, parser/should_fail/readFail007,
                      parser/should_fail/InfixAppPatErr, parser/should_fail/T984
    -}
  | SuggestMissingDo
    {-| Suggests that a \"let\" expression is needed in a \"do\" block.

       Test cases: None (that explicitly test this particular hint is emitted).
    -}
  | SuggestLetInDo
    {-| Suggests to add an \".hsig\" signature file to the Cabal manifest.

      Triggered by: 'GHC.Driver.Errors.Types.DriverUnexpectedSignature', if Cabal
                    is being used.

      Example: See comment of 'DriverUnexpectedSignature'.

      Test case(s): driver/T12955

    -}
  | SuggestAddSignatureCabalFile !ModuleName
    {-| Suggests to explicitly list the instantiations for the signatures in
        the GHC invocation command.

      Triggered by: 'GHC.Driver.Errors.Types.DriverUnexpectedSignature', if Cabal
                    is /not/ being used.

      Example: See comment of 'DriverUnexpectedSignature'.

      Test case(s): driver/T12955
    -}
  | SuggestSignatureInstantiations !ModuleName [InstantiationSuggestion]
    {-| Suggests to use spaces instead of tabs.

        Triggered by: 'GHC.Parser.Errors.Types.PsWarnTab'.

        Examples: None
        Test Case(s): None
    -}
  | SuggestUseSpaces
    {-| Suggests adding a whitespace after the given symbol.

        Examples: None
        Test Case(s): parser/should_compile/T18834a.hs
    -}
  | SuggestUseWhitespaceAfter !OperatorWhitespaceSymbol
    {-| Suggests adding a whitespace around the given operator symbol,
        as it might be repurposed as special syntax by a future language extension.
        The second parameter is how such operator occurred, if in a prefix, suffix
        or tight infix position.

        Triggered by: 'GHC.Parser.Errors.Types.PsWarnOperatorWhitespace'.

        Example:
          h a b = a+b -- not OK, no spaces around '+'.

        Test Case(s): parser/should_compile/T18834b.hs
    -}
  | SuggestUseWhitespaceAround !String !OperatorWhitespaceOccurrence
    {-| Suggests wrapping an expression in parentheses

        Examples: None
        Test Case(s): None
    -}
  | SuggestParentheses
    {-| Suggests to increase the -fmax-pmcheck-models limit for the pattern match checker.

      Triggered by: 'GHC.HsToCore.Errors.Types.DsMaxPmCheckModelsReached'

      Test case(s): pmcheck/should_compile/TooManyDeltas
                    pmcheck/should_compile/TooManyDeltas
                    pmcheck/should_compile/T11822
    -}
  | SuggestIncreaseMaxPmCheckModels
    {-| Suggests adding a type signature, typically to resolve ambiguity or help GHC inferring types.

    -}
  | SuggestAddTypeSignature
    {-| Suggests to explicitly discard the result of a monadic action by binding the result to
        the '_' wilcard.

        Example:
           main = do
             _ <- getCurrentTime

    -}
  | SuggestBindToWildcard !(LHsExpr GhcTc)

  | SuggestAddInlineOrNoInlinePragma !Var !Activation

  | SuggestAddPhaseToCompetingRule !RuleName
    {-| Suggests increasing the limit for the number of iterations in the simplifier.

    -}
  | SuggestIncreaseSimplifierIterations
    {-| Suggests to explicitly import 'Type' from the 'Data.Kind' module, because
        using "*" to mean 'Data.Kind.Type' relies on the StarIsType extension, which
        will become deprecated in the future.

        Triggered by: 'GHC.Parser.Errors.Types.PsWarnStarIsType'
        Example: None
        Test case(s): wcompat-warnings/WCompatWarningsOn.hs

    -}
  | SuggestUseTypeFromDataKind

    {-| Suggests placing the 'qualified' keyword /after/ the module name.

        Triggered by: 'GHC.Parser.Errors.Types.PsWarnImportPreQualified'
        Example: None
        Test case(s): module/mod184.hs

    -}
  | SuggestQualifiedAfterModuleName

    {-| Suggests using TemplateHaskell quotation syntax.

        Triggered by: 'GHC.Parser.Errors.Types.PsErrEmptyDoubleQuotes' only if TemplateHaskell
                      is enabled.
        Example: None
        Test case(s): parser/should_fail/T13450TH.hs

    -}
  | SuggestThQuotationSyntax

    {-| Suggests alternative roles in case we found an illegal one.

        Triggered by: 'GHC.Parser.Errors.Types.PsErrIllegalRoleName'
        Example: None
        Test case(s): roles/should_fail/Roles7.hs

    -}
  | SuggestRoles [Role]

    {-| Suggests qualifying the '*' operator in modules where StarIsType is enabled.

        Triggered by: 'GHC.Parser.Errors.Types.PsWarnStarBinder'
        Test case(s): warnings/should_compile/StarBinder.hs
    -}
  | SuggestQualifyStarOperator

    {-| Suggests that a type signature should have form <variable> :: <type>
        in order to be accepted by GHC.

        Triggered by: 'GHC.Parser.Errors.Types.PsErrInvalidTypeSignature'
        Test case(s): parser/should_fail/T3811
    -}
  | SuggestTypeSignatureForm

-- | An 'InstantiationSuggestion' for a '.hsig' file. This is generated
-- by GHC in case of a 'DriverUnexpectedSignature' and suggests a way
-- to instantiate a particular signature, where the first argument is
-- the signature name and the second is the module where the signature
-- was defined.
-- Example:
--
-- src/MyStr.hsig:2:11: error:
--     Unexpected signature: ‘MyStr’
--     (Try passing -instantiated-with="MyStr=<MyStr>"
--      replacing <MyStr> as necessary.)
data InstantiationSuggestion = InstantiationSuggestion !ModuleName !Module
