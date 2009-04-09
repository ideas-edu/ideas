module Domain.Programming.Helium 
   (compile, module UHA_Syntax) where

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

-- main = either print (\_ -> print "OK") $ compile "mysum xs = foldr (+) 0 xs"

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