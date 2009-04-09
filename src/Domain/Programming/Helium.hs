module Domain.Programming.Helium 
   (compile, module UHA_Syntax) where

import PhaseLexer
import PhaseParser
--import PhaseImport
import PhaseResolveOperators
import PhaseStaticChecks
import PhaseKindInferencer
import PhaseTypingStrategies
import PhaseTypeInferencer
import PhaseDesugarer
import PhaseCodeGenerator
import CompileUtils
import Utils
import UHA_Syntax
import Data.IORef
import StaticErrors(errorsLogCode)
import System.IO.Unsafe (unsafePerformIO)
import CompileUtils
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
import CorePretty
import Data.List(isPrefixOf)

main = either print (\_ -> print "OK") $ compile "mysum = foldr (+) 0"

compile :: String -> Either String Module
compile txt = unsafePerformIO $ do
   m <- compile_ txt [Overloading, Verbose] [".", "../../../heliumsystem/helium/lib"] []
   return (Right m)

--------------------------------------------------------------------
-- Adjusted code from Compile

compile_ :: String -> [Option] -> [String] -> [String] -> IO Module
compile_ contents options lvmPath doneModules =
    do
        let fullName = "..."
        let compileOptions = (options, fullName, doneModules)
        putStrLn ("Compiling " ++ fullName)

        -- Phase 1: Lexing
        (lexerWarnings, tokens) <- 
            doPhaseWithExit 20 (const "L") compileOptions $
               phaseLexer fullName contents options
        
        unless (NoWarnings `elem` options) $
            showMessages lexerWarnings

        -- Phase 2: Parsing
        parsedModule <- 
            doPhaseWithExit 20 (const "P") compileOptions $
               phaseParser fullName tokens options

        -- Phase 3: Importing
        (indirectionDecls, importEnvs) <-
            phaseImport fullName parsedModule lvmPath options
        
        -- Phase 4: Resolving operators
        resolvedModule <- 
            doPhaseWithExit 20 (const "R") compileOptions $
               phaseResolveOperators parsedModule importEnvs options
            
        stopCompilingIf (StopAfterParser `elem` options)

        -- Phase 5: Static checking
        (localEnv, typeSignatures, staticWarnings) <-
            doPhaseWithExit 20 (("S"++) . errorsLogCode) compileOptions $
               phaseStaticChecks fullName resolvedModule importEnvs options        

        unless (NoWarnings `elem` options) $
            showMessages staticWarnings

        stopCompilingIf (StopAfterStaticAnalysis `elem` options)

        -- Phase 6: Kind inferencing (by default turned off)
        let combinedEnv = foldr combineImportEnvironments localEnv importEnvs
        when (KindInferencing `elem` options) $
           doPhaseWithExit maximumNumberOfKindErrors (const "K") compileOptions $
              phaseKindInferencer combinedEnv resolvedModule options
              
        -- Phase 7: Type Inference Directives
        --(beforeTypeInferEnv, typingStrategiesDecls) <-
        --    phaseTypingStrategies fullName combinedEnv typeSignatures options
        let beforeTypeInferEnv = combinedEnv

        -- Phase 8: Type inferencing
        (dictionaryEnv, afterTypeInferEnv, toplevelTypes, typeWarnings) <- 
            doPhaseWithExit maximumNumberOfTypeErrors (const "T") compileOptions $ 
               phaseTypeInferencer fullName resolvedModule {-doneModules-} localEnv beforeTypeInferEnv options

        unless (NoWarnings `elem` options) $
            showMessages typeWarnings

        stopCompilingIf (StopAfterTypeInferencing `elem` options)

        return resolvedModule

stopCompilingIf :: Bool -> IO ()
stopCompilingIf bool = when bool (exitWith (ExitFailure 1))

maximumNumberOfTypeErrors :: Int
maximumNumberOfTypeErrors = 3

maximumNumberOfKindErrors :: Int
maximumNumberOfKindErrors = 1

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