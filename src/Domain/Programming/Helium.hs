module Domain.Programming.Helium 
   ( compile, module UHA_Syntax, module UHA_Range
   , emptyProg, range
   , undefExpr, undefPattern, undefRHS, undefLHS, undefDecl, undefFunBind, undefGuardedExpr
   , undefName, undefBody
   ) where

import PhaseLexer
import PhaseParser
import PhaseResolveOperators
import PhaseStaticChecks
import PhaseTypingStrategies ()
import PhaseTypeInferencer
import UHA_Syntax
import Data.IORef
import Messages
import HeliumMessages
import System.IO.Unsafe (unsafePerformIO)
import CompileUtils hiding (doPhaseWithExit)
import qualified Core
import Id(Id)
import UHA_Syntax
import UHA_Utils
import UHA_Range(noRange)
import Standard(searchPath)
import LvmImport(lvmImportDecls)
import Id(stringFromId)
import CoreToImportEnv(getImportEnvironment)
import qualified ExtractImportDecls(sem_Module)
import Data.List(isPrefixOf)
import Control.Monad.Trans
-- import Common.Uniplate

import Data.Typeable
import Data.Data hiding (Fixity)


-- main = either print (\_ -> print "OK") $ compile "mysum xs = foldr (+) 0 xs"

-- Some help functions
range :: (Int, Int) -> Range
range (line, col) = Range_Range (Position_Position "" line col) Position_Unknown

emptyProg =  Module_Module noRange MaybeName_Nothing MaybeExports_Nothing undefBody
undef = Name_Special noRange [] "undefined"

-- Typed holes in a incomplete program
undefDecl :: Declaration
undefDecl = Declaration_Empty noRange

undefBody :: Body
undefBody = Body_Body noRange [] []

undefExpr :: Expression
undefExpr = Expression_Variable noRange undef

undefGuardedExpr :: GuardedExpression
undefGuardedExpr = GuardedExpression_GuardedExpression noRange undefExpr undefExpr

undefLHS :: LeftHandSide
undefLHS = LeftHandSide_Function noRange undef []

undefRHS :: RightHandSide
undefRHS = RightHandSide_Expression noRange undefExpr MaybeDeclarations_Nothing

undefFunBind :: FunctionBinding
undefFunBind = FunctionBinding_FunctionBinding noRange undefLHS undefRHS

undefPattern :: Pattern
undefPattern = Pattern_Variable noRange undef

undefName :: Name
undefName = undef

-- the compiler/parser
compile :: String -> Either String Module
compile txt = unsafePerformIO $ do
   ea <- run $ compile_ txt [Overloading, Verbose] [".", "../../../heliumsystem/helium/lib"
                                                   ,"/Users/alex/Documents/heliumsystem/helium/lib"] []
   case ea of
      Left ms -> return $ Left $ unlines ms
      Right a -> return $ Right a 

newtype Compile a = C { run :: IO (Either [String] a) }

instance Monad Compile where
   return a  = C (return (Right a))
   C m >>= f = C $ do 
      ea <- m
      case ea of 
         Left err -> return (Left err)
         Right a  -> do
            let C m2 = f a
            m2

instance MonadIO Compile where
   liftIO m = C $ do
      a <- m
      return (Right a)

--------------------------------------------------------------------
-- Adjusted code from Compile

compile_ :: String -> [Option] -> [String] -> [String] -> Compile Module
compile_ contents options lvmPath doneModules =
    do
        let fullName = "..."
        let compileOptions = (options, fullName, doneModules)
        liftIO $ putStrLn ("Compiling")
        
        -- Phase 1: Lexing
        (lexerWarnings, tokens) <- 
            doPhaseWithExit $
               phaseLexer fullName contents options
        
        
        --unless (NoWarnings `elem` options) $
        --    showMessages lexerWarnings

        -- Phase 2: Parsing
        parsedModule <- 
            doPhaseWithExit $
               phaseParser fullName tokens options
        
        -- Phase 3: Importing
        (indirectionDecls, importEnvs) <-
            liftIO $ phaseImport fullName parsedModule lvmPath options
        
        -- Phase 4: Resolving operators
        resolvedModule <- 
            doPhaseWithExit $
               phaseResolveOperators parsedModule importEnvs options
        -- Phase 5: Static checking
        (localEnv, typeSignatures, staticWarnings) <-
            doPhaseWithExit $
               phaseStaticChecks fullName resolvedModule importEnvs options        

        --unless (NoWarnings `elem` options) $
        --    showMessages staticWarnings

        -- Phase 6: Kind inferencing (skipped)
        let combinedEnv = foldr combineImportEnvironments localEnv importEnvs              
        -- Phase 7: Type Inference Directives (skipped)
        let beforeTypeInferEnv = combinedEnv

        -- Phase 8: Type inferencing
        (dictionaryEnv, afterTypeInferEnv, toplevelTypes, typeWarnings) <- 
            doPhaseWithExit $ 
               phaseTypeInferencer fullName resolvedModule {-doneModules-} localEnv beforeTypeInferEnv options

        --unless (NoWarnings `elem` options) $
        --    showMessages typeWarnings

        return resolvedModule

--------------------------------------------------------------------
-- Adjusted code from PhaseImport

phaseImport :: String -> Module -> [String] -> [Option] -> 
                    IO ([Core.CoreDecl], [ImportEnvironment])
phaseImport _ module_ lvmPath options = do
    enterNewPhase "Importing" options

    let (filePath, baseName, _) = (".", "baseName", ()) --splitFilePath fullName

    -- Add HeliumLang and Prelude import
    let moduleWithExtraImports = addImplicitImports module_
             
    -- Chase imports
    chasedImpsList <- chaseImports lvmPath moduleWithExtraImports
    
    let indirectionDecls   = concat chasedImpsList
        importEnvs = 
            map (getImportEnvironment baseName) chasedImpsList
    
    return (indirectionDecls, importEnvs)

chaseImports :: [String] -> Module -> IO [[Core.CoreDecl]]
chaseImports lvmPath mod = 
    let (coreImports,_)   = ExtractImportDecls.sem_Module mod -- Expand imports
        findModule    = searchPath lvmPath ".lvm" . stringFromId
        doImport :: (Core.CoreDecl,[Id]) -> IO [Core.CoreDecl]
        doImport (importDecl,hidings)
          = do decls <- lvmImportDecls findModule [importDecl]
               return [ d
                      | d <- concat decls
                      , let name = Core.declName d
                      , "show" `isPrefixOf` stringFromId name || name `notElem` hidings
                      ]

    in mapM doImport coreImports
        -- zipWith ($) filterImports (lvmImportDecls findModule coreImportDecls)

-- Add "import Prelude" if
--   the currently compiled module is not the Prelude and
--   the Prelude is not explicitly imported
-- Always add "import HeliumLang
addImplicitImports :: Module -> Module
addImplicitImports m@(Module_Module moduleRange maybeName exports
                    (Body_Body bodyRange explicitImportDecls decls)) =
    Module_Module
        moduleRange
        maybeName
        exports
        (Body_Body
            bodyRange
            ( case maybeName of
                MaybeName_Just n
                    | getNameName n == "Prelude" -> []
                _ -> if "Prelude" `elem` map stringFromImportDeclaration explicitImportDecls
                     then []
                     else [ implicitImportDecl "Prelude" ]
            ++ [ implicitImportDecl "HeliumLang" ]
            ++ explicitImportDecls
            ) decls
        )
  where

    -- Artificial import declaration for implicit Prelude import
    implicitImportDecl :: String -> ImportDeclaration
    implicitImportDecl moduleName =
        ImportDeclaration_Import
            noRange
            False
            (Name_Identifier noRange [] moduleName) -- !!!Name
            MaybeName_Nothing
            MaybeImportSpecification_Nothing
            
--------------------------------------------------------------------
-- Adjusted code from CompileUtils

doPhaseWithExit :: HasMessage err => Phase err a -> Compile a
doPhaseWithExit phase = C $
   do result <- phase
      case result of
         Left errs ->
            do 
               --showErrorsAndExit errs nrOfMsgs
               return (Left (map showMessage errs))
         Right a ->
            return (Right a)

--------------------------------------------------------------------
-- Derived instances

deriving instance Show Module
deriving instance Show Body
deriving instance Show MaybeName
deriving instance Show MaybeNames
deriving instance Show MaybeExports
deriving instance Show Declaration
deriving instance Show ImportDeclaration
deriving instance Show Export 
deriving instance Show Type
deriving instance Show RightHandSide
deriving instance Show Pattern
deriving instance Show Constructor
deriving instance Show FunctionBinding
deriving instance Show MaybeInt
deriving instance Show Fixity
deriving instance Show MaybeDeclarations
deriving instance Show SimpleType
deriving instance Show ContextItem
deriving instance Show MaybeImportSpecification
deriving instance Show Expression
deriving instance Show RecordPatternBinding
deriving instance Show Literal
deriving instance Show GuardedExpression
deriving instance Show FieldDeclaration
deriving instance Show AnnotatedType
deriving instance Show LeftHandSide
deriving instance Show ImportSpecification
deriving instance Show RecordExpressionBinding
deriving instance Show MaybeExpression
deriving instance Show Statement
deriving instance Show Qualifier
deriving instance Show Alternative
deriving instance Show Import

deriving instance Eq Module
deriving instance Eq Body
deriving instance Eq MaybeName
deriving instance Eq MaybeNames
deriving instance Eq MaybeExports
deriving instance Eq ImportDeclaration
deriving instance Eq Export 
deriving instance Eq Expression
deriving instance Eq Type
deriving instance Eq RecordExpressionBinding
deriving instance Eq Literal
deriving instance Eq Declaration
deriving instance Eq Pattern
deriving instance Eq MaybeExpression
deriving instance Eq Statement
deriving instance Eq Qualifier
deriving instance Eq Alternative
deriving instance Eq RightHandSide
deriving instance Eq Constructor
deriving instance Eq FunctionBinding
deriving instance Eq MaybeInt
deriving instance Eq Fixity
deriving instance Eq MaybeDeclarations
deriving instance Eq SimpleType
deriving instance Eq FieldDeclaration
deriving instance Eq AnnotatedType
deriving instance Eq LeftHandSide
deriving instance Eq ContextItem
deriving instance Eq RecordPatternBinding
deriving instance Eq GuardedExpression
deriving instance Eq MaybeImportSpecification
deriving instance Eq ImportSpecification
deriving instance Eq Import

deriving instance Data Module
deriving instance Data Range
deriving instance Data Position
deriving instance Data Name
deriving instance Data Body
deriving instance Data MaybeName
deriving instance Data MaybeNames
deriving instance Data MaybeExports
deriving instance Data Declaration
deriving instance Data ImportDeclaration
deriving instance Data Export 
deriving instance Data Type
deriving instance Data RightHandSide
deriving instance Data Pattern
deriving instance Data Constructor
deriving instance Data FunctionBinding
deriving instance Data MaybeInt
deriving instance Data Fixity
deriving instance Data MaybeDeclarations
deriving instance Data SimpleType
deriving instance Data ContextItem
deriving instance Data MaybeImportSpecification
deriving instance Data Expression
deriving instance Data RecordPatternBinding
deriving instance Data Literal
deriving instance Data GuardedExpression
deriving instance Data FieldDeclaration
deriving instance Data AnnotatedType
deriving instance Data LeftHandSide
deriving instance Data ImportSpecification
deriving instance Data RecordExpressionBinding
deriving instance Data MaybeExpression
deriving instance Data Statement
deriving instance Data Qualifier
deriving instance Data Alternative
deriving instance Data Import

deriving instance Typeable Module
deriving instance Typeable Range
deriving instance Typeable Position
deriving instance Typeable Name
deriving instance Typeable Body
deriving instance Typeable MaybeName
deriving instance Typeable MaybeNames
deriving instance Typeable MaybeExports
deriving instance Typeable Declaration
deriving instance Typeable ImportDeclaration
deriving instance Typeable Export 
deriving instance Typeable Type
deriving instance Typeable RightHandSide
deriving instance Typeable Pattern
deriving instance Typeable Constructor
deriving instance Typeable FunctionBinding
deriving instance Typeable MaybeInt
deriving instance Typeable Fixity
deriving instance Typeable MaybeDeclarations
deriving instance Typeable SimpleType
deriving instance Typeable ContextItem
deriving instance Typeable MaybeImportSpecification
deriving instance Typeable Expression
deriving instance Typeable RecordPatternBinding
deriving instance Typeable Literal
deriving instance Typeable GuardedExpression
deriving instance Typeable FieldDeclaration
deriving instance Typeable AnnotatedType
deriving instance Typeable LeftHandSide
deriving instance Typeable ImportSpecification
deriving instance Typeable RecordExpressionBinding
deriving instance Typeable MaybeExpression
deriving instance Typeable Statement
deriving instance Typeable Qualifier
deriving instance Typeable Alternative
deriving instance Typeable Import
