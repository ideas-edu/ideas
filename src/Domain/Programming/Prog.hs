module Domain.Programming.Prog where

import Common.Context
import Common.Strategy
import Control.Monad.State
import Data.Generics.Biplate
import Data.Generics.PlateData
import Data.Data hiding (Fixity)
import Data.List
import Data.Typeable
import Domain.Programming.Helium


-- write a PreludeS, with strategies for prelude functions

-- Some help functions
range :: (Int, Int) -> Range
range (line, col) = Range_Range (Position_Position "" line col) Position_Unknown

emptyProg =  Module_Module noRange MaybeName_Nothing MaybeExports_Nothing undefBody
undef = Name_Special noRange [] "undefined"

collectNames :: Module -> [String]
collectNames m = nub [ s | Name_Identifier _ _ s <- universeBi m ]

equalModules :: Module -> Module -> Bool
equalModules x y = f x == f y
  where
    f = alphaConversion . removeRanges
    
removeRanges = transformBi (\(Range_Range  _ _) -> noRange)

alphaPairs m = zip names freshnames
  where 
    names      = nub [ Name_Identifier r m s | Name_Identifier r m s <- universeBi m
                     ,                         s `notElem` preludeIdentifiers ]
    freshnames = ['x' : show i | i <- [1..]]

rename (old, new) = transformBi f
  where 
    f x | x == old  = Name_Identifier noRange [] new
        | otherwise = x

alphaConversion m = foldr rename m $ alphaPairs m

allSolutions strat = map (fromContext . snd . last . snd) $ derivations strat $ inContext emptyProg
isSolution mod strat = any (equalModules mod) $ allSolutions strat

checkExercises :: Strategy (Context Module) -> [String] -> IO ()
checkExercises strat xs = mapM_ f xs
  where
    f x = putStrLn $ x ++ " : " ++ (show (isSolution (fromRight (compile x)) strat))
    fromRight x = case x of
                    Right y -> y
                    _       -> error "no compile"

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

undefLiteral :: Literal
undefLiteral = Literal_String noRange "undefined"

undefName :: Name
undefName = undef

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

-- Reserved identifiers
preludeIdentifiers = ["$!","catch","!!","$","&&","++",".","=<<","Bool","False","True","Bounded","minBound","maxBound","Char","Double","Either","Left","Right","Enum","succ","pred","toEnum","fromEnum","enumFrom","enumFromThen","enumFromTo","enumFromThenTo","Eq","==","/=","FilePath","Float","Fractional","pi","exp","sqrt","log","**","logBase","sin","tan","cos","asin","atan","acos","sinh","tanh","cosh","asinh","atanh","acosh","Fractional","/","recip","fromRational","Functor","fmap","IO","IOError","Int","Integer","Integral","quot","rem","div","mod","quotRem","divMod","toInteger","Maybe","Nothing","Just","Monad",">>=",">>","return","fail","Num","+","*","-","negate","abs","signum","fromInteger","Ord","compare","<",">=",">","<=","max","min","Ordering","LT","EQ","GT","Rational","Read","readsPrec","readList","ReadS","Real","toRational","RealFloat","floatRadix","floatDigits","floatRange","decodeFloat","encodeFloat","exponent","significand","scaleFloat","isNaN","isInfinite","isDenormalized","isNegativeZero","isIEEE","atan2","RealFrac","properFraction","truncate","round","ceiling","floor","Show","showsPrec","show","showList","ShowS","String","^","^^","all","and","any","appendFile","asTypeOf","break","concat","concatMap","const","curry","cycle","drop","dropWhile","either","elem","error","even","filter","flip","foldl","foldl1","foldr","foldr1","fromIntegral","fst","gcd","getChar","getContents","getLine","head","id","init","interact","ioError","iterate","last","lcm","length","lex","lines","lookup","map","mapM","mapM_","maximum","maybe","minimum","not","notElem","null","odd","or","otherwise","print","product","putChar","putStr","putStrLn","read","readFile","readIO","readLn","readParen","reads","realToFrac","repeat","replicate","reverse","scanl","scanl1","scanr","scanr1","seq","sequence","sequence_","showChar","showParen","showString","shows","snd","span","splitAt","subtract","sum","tail","take","takeWhile","uncurry","undefined","unlines","until","unwords","unzip","unzip3","userError","words","writeFile","zip","zip3","zipWith","zipWith3","||"]
