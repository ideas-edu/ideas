module Service.TypedExample (typedExample) where

import Data.Char
import Service.ModeXML
import Service.ExerciseList
import Service.ServiceList
import Service.Types
import Common.Exercise
import Text.XML
   
typedExample :: Monad m => 
               ExercisePackage a -> Service a -> [TypedValue a] -> m (XML, XML, Bool)
typedExample pkg service args = do
   -- Construct a request in xml
   xmlRequest <- 
      case makeArgType args of
         reqTuple ::: reqTp -> 
            case encodeType (encoder evaluator) reqTp reqTuple of
               Left err  -> fail err
               Right xml -> return $ 
                  stdReply (serviceName service) enc (exercise pkg) xml
   -- Construct a reply in xml
   xmlReply <- return $
      case appls (serviceFunction service) args of
         reply ::: replyTp ->
            case encodeType (encoder evaluator) replyTp reply of
               Left err  -> resultError err
               Right xml -> resultOk xml
   -- Check request/reply pair
   xmlTest <- return $
      case processXML (show xmlRequest) of
         Left s -> False `const` (s::String)
         Right (_, textReply, _) ->
            let p = filter (not . isSpace)
            in p textReply == p (show xmlReply)

   return (xmlRequest, xmlReply, xmlTest)
 where
   (evaluator, enc)
      | withOpenMath pkg = (openMathConverterTp pkg, "openmath")
      | otherwise        = (stringFormatConverterTp pkg, "string")
         

stdReply :: String -> String -> Exercise a -> XMLBuilder -> XML
stdReply s enc ex body = makeXML "request" $ do 
   "service"    .=. s
   "exerciseid" .=. show (exerciseCode ex)
   "source"     .=. "test"
   "encoding"   .=. enc
   body

makeArgType :: [TypedValue a] -> TypedValue a
makeArgType [tv] = tv
makeArgType [a1 ::: t1, a2 ::: t2] = (a1, a2) ::: Tuple t1 t2
makeArgType [a1 ::: t1, a2 ::: t2, a3 ::: t3] = (a1, a2, a3) ::: Triple t1 t2 t3
makeArgType [a1 ::: t1, a2 ::: t2, a3 ::: t3, a4 ::: t4] = (a1, a2, a3, a4) ::: Quadruple t1 t2 t3 t4
makeArgType _ = error $ "makeArgType: invalid number of arguments"

appls :: TypedValue a -> [TypedValue a] -> TypedValue a
appls = foldl appl

appl :: TypedValue a -> TypedValue a -> TypedValue a
appl fun arg =
   case (fun, arg) of
      (f ::: t1 :-> t2, a ::: t3) -> 
         case equal t3 t1 of 
            Just eq -> f (eq a) ::: t2
            Nothing -> error $ "mismatch (argument type): " ++ show t3 ++ " does not match " ++ show t1
      _ -> error "mismatch (not a function)"
      
equal :: Type a t1 -> Type a t2 -> Maybe (t1 -> t2)
equal t1 t2 = 
   case (t1, t2) of
      (Maybe a, Maybe b) -> fmap fmap (equal a b)
      (StrategyCfg, StrategyCfg) -> Just id
      (State, State) -> Just id
      (Location, Location) -> Just id
      _ -> Nothing
      
{-
showVal :: Exercise a -> TypedValue a -> String
showVal ex tv = 
   case tv of
      xs  ::: List t -> "[" ++ commaList (map (showVal ex . (::: t)) xs) ++ "]"
      pr ::: Tuple t1 t2 -> "(" ++ showVal ex (fst pr:::t1) ++ "," ++ showVal ex (snd pr:::t2) ++ ")"
      r   ::: Rule -> show r
      ctx ::: Context -> prettyPrinter ex (fromContext ctx)
      _ -> "(typed value)" -}