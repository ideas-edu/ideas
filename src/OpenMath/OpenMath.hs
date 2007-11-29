module OpenMath.OpenMath where

import Text.XML.HaXml.Xml2Haskell
import Text.XML.HaXml.OneOfN
import Char (isSpace)


{-Type decls-}

data OMOBJ = OMOBJOMS OMOBJ_Attrs OMS
           | OMOBJOMV OMOBJ_Attrs OMV
           | OMOBJOMI OMOBJ_Attrs OMI
           | OMOBJOMB OMOBJ_Attrs OMB
           | OMOBJOMSTR OMOBJ_Attrs OMSTR
           | OMOBJOMF OMOBJ_Attrs OMF
           | OMOBJOMA OMOBJ_Attrs OMA
           | OMOBJOMBIND OMOBJ_Attrs OMBIND
           | OMOBJOME OMOBJ_Attrs OME
           | OMOBJOMATTR OMOBJ_Attrs OMATTR
           | OMOBJOMR OMOBJ_Attrs OMR
           deriving (Eq,Show)
data OMOBJ_Attrs = OMOBJ_Attrs
    { oMOBJXmlns :: (Defaultable String)
    , oMOBJId :: (Maybe String)
    , oMOBJCdbase :: (Maybe String)
    , oMOBJVersion :: (Maybe String)
    } deriving (Eq,Show)
data OMS = OMS
    { oMSXmlns :: (Defaultable String)
    , oMSId :: (Maybe String)
    , oMSName :: String
    , oMSCd :: String
    , oMSCdbase :: (Maybe String)
    } deriving (Eq,Show)
data OMV = OMV
    { oMVXmlns :: (Defaultable String)
    , oMVId :: (Maybe String)
    , oMVName :: String
    } deriving (Eq,Show)
data OMI = OMI OMI_Attrs String
         deriving (Eq,Show)
data OMI_Attrs = OMI_Attrs
    { oMIXmlns :: (Defaultable String)
    , oMIId :: (Maybe String)
    } deriving (Eq,Show)
data OMB = OMB OMB_Attrs String
         deriving (Eq,Show)
data OMB_Attrs = OMB_Attrs
    { oMBXmlns :: (Defaultable String)
    , oMBId :: (Maybe String)
    } deriving (Eq,Show)
data OMSTR = OMSTR OMSTR_Attrs String
           deriving (Eq,Show)
data OMSTR_Attrs = OMSTR_Attrs
    { oMSTRXmlns :: (Defaultable String)
    , oMSTRId :: (Maybe String)
    } deriving (Eq,Show)
data OMF = OMF
    { oMFXmlns :: (Defaultable String)
    , oMFId :: (Maybe String)
    , oMFDec :: (Maybe String)
    , oMFHex :: (Maybe String)
    } deriving (Eq,Show)
data OMA = OMA OMA_Attrs (List1 OMA_)
         deriving (Eq,Show)
data OMA_Attrs = OMA_Attrs
    { oMAXmlns :: (Defaultable String)
    , oMAId :: (Maybe String)
    , oMACdbase :: (Maybe String)
    } deriving (Eq,Show)
data OMA_ = OMA_OMS OMS
          | OMA_OMV OMV
          | OMA_OMI OMI
          | OMA_OMB OMB
          | OMA_OMSTR OMSTR
          | OMA_OMF OMF
          | OMA_OMA OMA
          | OMA_OMBIND OMBIND
          | OMA_OME OME
          | OMA_OMATTR OMATTR
          | OMA_OMR OMR
          deriving (Eq,Show)
data OMBIND = OMBIND OMBIND_Attrs
                     (OneOf11 OMS OMV OMI OMB OMSTR OMF OMA OMBIND OME OMATTR OMR)
                     OMBVAR
                     (OneOf11 OMS OMV OMI OMB OMSTR OMF OMA OMBIND OME OMATTR OMR)
            deriving (Eq,Show)
data OMBIND_Attrs = OMBIND_Attrs
    { oMBINDXmlns :: (Defaultable String)
    , oMBINDId :: (Maybe String)
    , oMBINDCdbase :: (Maybe String)
    } deriving (Eq,Show)
data OMBVAR = OMBVAR OMBVAR_Attrs (List1 OMBVAR_)
            deriving (Eq,Show)
data OMBVAR_Attrs = OMBVAR_Attrs
    { oMBVARXmlns :: (Defaultable String)
    , oMBVARId :: (Maybe String)
    } deriving (Eq,Show)
data OMBVAR_ = OMBVAR_OMV OMV
             | OMBVAR_OMATTR OMATTR
             deriving (Eq,Show)
data OME = OME OME_Attrs OMS
               [(OneOf12 OMS OMV OMI OMB OMSTR OMF OMA OMBIND OME OMATTR OMR OMFOREIGN)]
         deriving (Eq,Show)
data OME_Attrs = OME_Attrs
    { oMEXmlns :: (Defaultable String)
    , oMEId :: (Maybe String)
    } deriving (Eq,Show)
data OMATTR = OMATTR OMATTR_Attrs OMATP
                     (OneOf11 OMS OMV OMI OMB OMSTR OMF OMA OMBIND OME OMATTR OMR)
            deriving (Eq,Show)
data OMATTR_Attrs = OMATTR_Attrs
    { oMATTRXmlns :: (Defaultable String)
    , oMATTRId :: (Maybe String)
    , oMATTRCdbase :: (Maybe String)
    } deriving (Eq,Show)
data OMATP = OMATP OMATP_Attrs (List1 OMATP_)
           deriving (Eq,Show)
data OMATP_Attrs = OMATP_Attrs
    { oMATPXmlns :: (Defaultable String)
    , oMATPId :: (Maybe String)
    , oMATPCdbase :: (Maybe String)
    } deriving (Eq,Show)
data OMATP_ = OMATP_ OMS
                     (OneOf12 OMS OMV OMI OMB OMSTR OMF OMA OMBIND OME OMATTR OMR OMFOREIGN)
            deriving (Eq,Show)
data OMFOREIGN = OMFOREIGN OMFOREIGN_Attrs ANYContent
               deriving (Eq,Show)
data OMFOREIGN_Attrs = OMFOREIGN_Attrs
    { oMFOREIGNXmlns :: (Defaultable String)
    , oMFOREIGNId :: (Maybe String)
    , oMFOREIGNCdbase :: (Maybe String)
    , oMFOREIGNEncoding :: (Maybe String)
    } deriving (Eq,Show)
data OMR = OMR
    { oMRXmlns :: (Defaultable String)
    , oMRId :: (Maybe String)
    , oMRHref :: String
    } deriving (Eq,Show)


{-Instance decls-}

instance XmlContent OMOBJ where
    fromElem (CElem (Elem "OMOBJ" as c0):rest) =
        case (fromElem c0) of
        (Just a,_) -> (Just (OMOBJOMS (fromAttrs as) a), rest)
        (_,_) ->
                case (fromElem c0) of
                (Just a,_) -> (Just (OMOBJOMV (fromAttrs as) a), rest)
                (_,_) ->
                        case (fromElem c0) of
                        (Just a,_) -> (Just (OMOBJOMI (fromAttrs as) a), rest)
                        (_,_) ->
                                case (fromElem c0) of
                                (Just a,_) -> (Just (OMOBJOMB (fromAttrs as) a), rest)
                                (_,_) ->
                                        case (fromElem c0) of
                                        (Just a,_) -> (Just (OMOBJOMSTR (fromAttrs as) a), rest)
                                        (_,_) ->
                                                case (fromElem c0) of
                                                (Just a,_) -> (Just (OMOBJOMF (fromAttrs as) a), rest)
                                                (_,_) ->
                                                        case (fromElem c0) of
                                                        (Just a,_) -> (Just (OMOBJOMA (fromAttrs as) a), rest)
                                                        (_,_) ->
                                                                case (fromElem c0) of
                                                                (Just a,_) -> (Just (OMOBJOMBIND (fromAttrs as) a), rest)
                                                                (_,_) ->
                                                                        case (fromElem c0) of
                                                                        (Just a,_) -> (Just (OMOBJOME (fromAttrs as) a), rest)
                                                                        (_,_) ->
                                                                                case (fromElem c0) of
                                                                                (Just a,_) -> (Just (OMOBJOMATTR (fromAttrs as) a), rest)
                                                                                (_,_) ->
                                                                                        case (fromElem c0) of
                                                                                        (Just a,_) -> (Just (OMOBJOMR (fromAttrs as) a), rest)
                                                                                        (_,_) ->
                                                                                            (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (OMOBJOMS as a) = [CElem (Elem "OMOBJ" (toAttrs as) (toElem a) )]
    toElem (OMOBJOMV as a) = [CElem (Elem "OMOBJ" (toAttrs as) (toElem a) )]
    toElem (OMOBJOMI as a) = [CElem (Elem "OMOBJ" (toAttrs as) (toElem a) )]
    toElem (OMOBJOMB as a) = [CElem (Elem "OMOBJ" (toAttrs as) (toElem a) )]
    toElem (OMOBJOMSTR as a) = [CElem (Elem "OMOBJ" (toAttrs as) (toElem a) )]
    toElem (OMOBJOMF as a) = [CElem (Elem "OMOBJ" (toAttrs as) (toElem a) )]
    toElem (OMOBJOMA as a) = [CElem (Elem "OMOBJ" (toAttrs as) (toElem a) )]
    toElem (OMOBJOMBIND as a) = [CElem (Elem "OMOBJ" (toAttrs as) (toElem a) )]
    toElem (OMOBJOME as a) = [CElem (Elem "OMOBJ" (toAttrs as) (toElem a) )]
    toElem (OMOBJOMATTR as a) = [CElem (Elem "OMOBJ" (toAttrs as) (toElem a) )]
    toElem (OMOBJOMR as a) = [CElem (Elem "OMOBJ" (toAttrs as) (toElem a) )]
instance XmlAttributes OMOBJ_Attrs where
    fromAttrs as =
        OMOBJ_Attrs
          { oMOBJXmlns = defaultA fromAttrToStr "http://www.openmath.org/OpenMath" "xmlns" as
          , oMOBJId = possibleA fromAttrToStr "id" as
          , oMOBJCdbase = possibleA fromAttrToStr "cdbase" as
          , oMOBJVersion = possibleA fromAttrToStr "version" as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrStr "xmlns" (oMOBJXmlns v)
        , maybeToAttr toAttrFrStr "id" (oMOBJId v)
        , maybeToAttr toAttrFrStr "cdbase" (oMOBJCdbase v)
        , maybeToAttr toAttrFrStr "version" (oMOBJVersion v)
        ]
instance XmlContent OMS where
    fromElem (CElem (Elem "OMS" as []):rest) =
        (Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
        [CElem (Elem "OMS" (toAttrs as) [])]
instance XmlAttributes OMS where
    fromAttrs as =
        OMS
          { oMSXmlns = defaultA fromAttrToStr "http://www.openmath.org/OpenMath" "xmlns" as
          , oMSId = possibleA fromAttrToStr "id" as
          , oMSName = definiteA fromAttrToStr "OMS" "name" as
          , oMSCd = definiteA fromAttrToStr "OMS" "cd" as
          , oMSCdbase = possibleA fromAttrToStr "cdbase" as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrStr "xmlns" (oMSXmlns v)
        , maybeToAttr toAttrFrStr "id" (oMSId v)
        , toAttrFrStr "name" (oMSName v)
        , toAttrFrStr "cd" (oMSCd v)
        , maybeToAttr toAttrFrStr "cdbase" (oMSCdbase v)
        ]
instance XmlContent OMV where
    fromElem (CElem (Elem "OMV" as []):rest) =
        (Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
        [CElem (Elem "OMV" (toAttrs as) [])]
instance XmlAttributes OMV where
    fromAttrs as =
        OMV
          { oMVXmlns = defaultA fromAttrToStr "http://www.openmath.org/OpenMath" "xmlns" as
          , oMVId = possibleA fromAttrToStr "id" as
          , oMVName = definiteA fromAttrToStr "OMV" "name" as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrStr "xmlns" (oMVXmlns v)
        , maybeToAttr toAttrFrStr "id" (oMVId v)
        , toAttrFrStr "name" (oMVName v)
        ]
instance XmlContent OMI where
    fromElem (CElem (Elem "OMI" as c0):rest) =
        (\(a,ca)->
           (Just (OMI (fromAttrs as) a), rest))
        (definite fromText "text" "OMI" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (OMI as a) =
        [CElem (Elem "OMI" (toAttrs as) (toText a))]
instance XmlAttributes OMI_Attrs where
    fromAttrs as =
        OMI_Attrs
          { oMIXmlns = defaultA fromAttrToStr "http://www.openmath.org/OpenMath" "xmlns" as
          , oMIId = possibleA fromAttrToStr "id" as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrStr "xmlns" (oMIXmlns v)
        , maybeToAttr toAttrFrStr "id" (oMIId v)
        ]
instance XmlContent OMB where
    fromElem (CElem (Elem "OMB" as c0):rest) =
        (\(a,ca)->
           (Just (OMB (fromAttrs as) a), rest))
        (definite fromText "text" "OMB" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (OMB as a) =
        [CElem (Elem "OMB" (toAttrs as) (toText a))]
instance XmlAttributes OMB_Attrs where
    fromAttrs as =
        OMB_Attrs
          { oMBXmlns = defaultA fromAttrToStr "http://www.openmath.org/OpenMath" "xmlns" as
          , oMBId = possibleA fromAttrToStr "id" as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrStr "xmlns" (oMBXmlns v)
        , maybeToAttr toAttrFrStr "id" (oMBId v)
        ]
instance XmlContent OMSTR where
    fromElem (CElem (Elem "OMSTR" as c0):rest) =
        (\(a,ca)->
           (Just (OMSTR (fromAttrs as) a), rest))
        (definite fromText "text" "OMSTR" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (OMSTR as a) =
        [CElem (Elem "OMSTR" (toAttrs as) (toText a))]
instance XmlAttributes OMSTR_Attrs where
    fromAttrs as =
        OMSTR_Attrs
          { oMSTRXmlns = defaultA fromAttrToStr "http://www.openmath.org/OpenMath" "xmlns" as
          , oMSTRId = possibleA fromAttrToStr "id" as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrStr "xmlns" (oMSTRXmlns v)
        , maybeToAttr toAttrFrStr "id" (oMSTRId v)
        ]
instance XmlContent OMF where
    fromElem (CElem (Elem "OMF" as []):rest) =
        (Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
        [CElem (Elem "OMF" (toAttrs as) [])]
instance XmlAttributes OMF where
    fromAttrs as =
        OMF
          { oMFXmlns = defaultA fromAttrToStr "http://www.openmath.org/OpenMath" "xmlns" as
          , oMFId = possibleA fromAttrToStr "id" as
          , oMFDec = possibleA fromAttrToStr "dec" as
          , oMFHex = possibleA fromAttrToStr "hex" as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrStr "xmlns" (oMFXmlns v)
        , maybeToAttr toAttrFrStr "id" (oMFId v)
        , maybeToAttr toAttrFrStr "dec" (oMFDec v)
        , maybeToAttr toAttrFrStr "hex" (oMFHex v)
        ]
instance XmlContent OMA where
    fromElem (CElem (Elem "OMA" as c0):rest) =
        (\(a,ca)->
           (Just (OMA (fromAttrs as) a), rest))
        (definite fromElem "OMA+" "OMA" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (OMA as a) =
        [CElem (Elem "OMA" (toAttrs as) (toElem a))]
instance XmlAttributes OMA_Attrs where
    fromAttrs as =
        OMA_Attrs
          { oMAXmlns = defaultA fromAttrToStr "http://www.openmath.org/OpenMath" "xmlns" as
          , oMAId = possibleA fromAttrToStr "id" as
          , oMACdbase = possibleA fromAttrToStr "cdbase" as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrStr "xmlns" (oMAXmlns v)
        , maybeToAttr toAttrFrStr "id" (oMAId v)
        , maybeToAttr toAttrFrStr "cdbase" (oMACdbase v)
        ]
instance XmlContent OMA_ where
    fromElem c0 =
        case (fromElem c0) of
        (Just a,rest) -> (Just (OMA_OMS a), rest)
        (_,_) ->
                case (fromElem c0) of
                (Just a,rest) -> (Just (OMA_OMV a), rest)
                (_,_) ->
                        case (fromElem c0) of
                        (Just a,rest) -> (Just (OMA_OMI a), rest)
                        (_,_) ->
                                case (fromElem c0) of
                                (Just a,rest) -> (Just (OMA_OMB a), rest)
                                (_,_) ->
                                        case (fromElem c0) of
                                        (Just a,rest) -> (Just (OMA_OMSTR a), rest)
                                        (_,_) ->
                                                case (fromElem c0) of
                                                (Just a,rest) -> (Just (OMA_OMF a), rest)
                                                (_,_) ->
                                                        case (fromElem c0) of
                                                        (Just a,rest) -> (Just (OMA_OMA a), rest)
                                                        (_,_) ->
                                                                case (fromElem c0) of
                                                                (Just a,rest) -> (Just (OMA_OMBIND a), rest)
                                                                (_,_) ->
                                                                        case (fromElem c0) of
                                                                        (Just a,rest) -> (Just (OMA_OME a), rest)
                                                                        (_,_) ->
                                                                                case (fromElem c0) of
                                                                                (Just a,rest) -> (Just (OMA_OMATTR a), rest)
                                                                                (_,_) ->
                                                                                        case (fromElem c0) of
                                                                                        (Just a,rest) -> (Just (OMA_OMR a), rest)
                                                                                        (_,_) ->
                                                                                            (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (OMA_OMS a) = toElem a
    toElem (OMA_OMV a) = toElem a
    toElem (OMA_OMI a) = toElem a
    toElem (OMA_OMB a) = toElem a
    toElem (OMA_OMSTR a) = toElem a
    toElem (OMA_OMF a) = toElem a
    toElem (OMA_OMA a) = toElem a
    toElem (OMA_OMBIND a) = toElem a
    toElem (OMA_OME a) = toElem a
    toElem (OMA_OMATTR a) = toElem a
    toElem (OMA_OMR a) = toElem a
instance XmlContent OMBIND where
    fromElem (CElem (Elem "OMBIND" as c0):rest) =
        (\(a,ca)->
           (\(b,cb)->
              (\(c,cc)->
                 (Just (OMBIND (fromAttrs as) a b c), rest))
              (definite fromElem "OneOf" "OMBIND" cb))
           (definite fromElem "<OMBVAR>" "OMBIND" ca))
        (definite fromElem "OneOf" "OMBIND" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (OMBIND as a b c) =
        [CElem (Elem "OMBIND" (toAttrs as) (toElem a ++ toElem b ++
                                            toElem c))]
instance XmlAttributes OMBIND_Attrs where
    fromAttrs as =
        OMBIND_Attrs
          { oMBINDXmlns = defaultA fromAttrToStr "http://www.openmath.org/OpenMath" "xmlns" as
          , oMBINDId = possibleA fromAttrToStr "id" as
          , oMBINDCdbase = possibleA fromAttrToStr "cdbase" as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrStr "xmlns" (oMBINDXmlns v)
        , maybeToAttr toAttrFrStr "id" (oMBINDId v)
        , maybeToAttr toAttrFrStr "cdbase" (oMBINDCdbase v)
        ]
instance XmlContent OMBVAR where
    fromElem (CElem (Elem "OMBVAR" as c0):rest) =
        (\(a,ca)->
           (Just (OMBVAR (fromAttrs as) a), rest))
        (definite fromElem "OMBVAR+" "OMBVAR" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (OMBVAR as a) =
        [CElem (Elem "OMBVAR" (toAttrs as) (toElem a))]
instance XmlAttributes OMBVAR_Attrs where
    fromAttrs as =
        OMBVAR_Attrs
          { oMBVARXmlns = defaultA fromAttrToStr "http://www.openmath.org/OpenMath" "xmlns" as
          , oMBVARId = possibleA fromAttrToStr "id" as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrStr "xmlns" (oMBVARXmlns v)
        , maybeToAttr toAttrFrStr "id" (oMBVARId v)
        ]
instance XmlContent OMBVAR_ where
    fromElem c0 =
        case (fromElem c0) of
        (Just a,rest) -> (Just (OMBVAR_OMV a), rest)
        (_,_) ->
                case (fromElem c0) of
                (Just a,rest) -> (Just (OMBVAR_OMATTR a), rest)
                (_,_) ->
                    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (OMBVAR_OMV a) = toElem a
    toElem (OMBVAR_OMATTR a) = toElem a
instance XmlContent OME where
    fromElem (CElem (Elem "OME" as c0):rest) =
        (\(a,ca)->
           (\(b,cb)->
              (Just (OME (fromAttrs as) a b), rest))
           (many fromElem ca))
        (definite fromElem "<OMS>" "OME" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (OME as a b) =
        [CElem (Elem "OME" (toAttrs as) (toElem a ++ concatMap toElem b))]
instance XmlAttributes OME_Attrs where
    fromAttrs as =
        OME_Attrs
          { oMEXmlns = defaultA fromAttrToStr "http://www.openmath.org/OpenMath" "xmlns" as
          , oMEId = possibleA fromAttrToStr "id" as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrStr "xmlns" (oMEXmlns v)
        , maybeToAttr toAttrFrStr "id" (oMEId v)
        ]
instance XmlContent OMATTR where
    fromElem (CElem (Elem "OMATTR" as c0):rest) =
        (\(a,ca)->
           (\(b,cb)->
              (Just (OMATTR (fromAttrs as) a b), rest))
           (definite fromElem "OneOf" "OMATTR" ca))
        (definite fromElem "<OMATP>" "OMATTR" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (OMATTR as a b) =
        [CElem (Elem "OMATTR" (toAttrs as) (toElem a ++ toElem b))]
instance XmlAttributes OMATTR_Attrs where
    fromAttrs as =
        OMATTR_Attrs
          { oMATTRXmlns = defaultA fromAttrToStr "http://www.openmath.org/OpenMath" "xmlns" as
          , oMATTRId = possibleA fromAttrToStr "id" as
          , oMATTRCdbase = possibleA fromAttrToStr "cdbase" as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrStr "xmlns" (oMATTRXmlns v)
        , maybeToAttr toAttrFrStr "id" (oMATTRId v)
        , maybeToAttr toAttrFrStr "cdbase" (oMATTRCdbase v)
        ]
instance XmlContent OMATP where
    fromElem (CElem (Elem "OMATP" as c0):rest) =
        (\(a,ca)->
           (Just (OMATP (fromAttrs as) a), rest))
        (definite fromElem "OMATP+" "OMATP" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (OMATP as a) =
        [CElem (Elem "OMATP" (toAttrs as) (toElem a))]
instance XmlAttributes OMATP_Attrs where
    fromAttrs as =
        OMATP_Attrs
          { oMATPXmlns = defaultA fromAttrToStr "http://www.openmath.org/OpenMath" "xmlns" as
          , oMATPId = possibleA fromAttrToStr "id" as
          , oMATPCdbase = possibleA fromAttrToStr "cdbase" as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrStr "xmlns" (oMATPXmlns v)
        , maybeToAttr toAttrFrStr "id" (oMATPId v)
        , maybeToAttr toAttrFrStr "cdbase" (oMATPCdbase v)
        ]
instance XmlContent OMATP_ where
    fromElem c0 =
        case (\(a,ca)->
                (\(b,cb)->
                   (a,b,cb))
                (fromElem ca))
             (fromElem c0) of
        (Just a,Just b,rest) -> (Just (OMATP_ a b), rest)
        (_,_,_) ->
            (Nothing, c0)
    toElem (OMATP_ a b) =
        (toElem a ++ toElem b)
instance XmlContent OMFOREIGN where
    fromElem (CElem (Elem "OMFOREIGN" as c0):rest) =
        (\(a,ca)->
           (Just (OMFOREIGN (fromAttrs as) a), rest))
        (definite fromElem "ANY" "OMFOREIGN" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (OMFOREIGN as a) =
        [CElem (Elem "OMFOREIGN" (toAttrs as) (toElem a))]
instance XmlAttributes OMFOREIGN_Attrs where
    fromAttrs as =
        OMFOREIGN_Attrs
          { oMFOREIGNXmlns = defaultA fromAttrToStr "http://www.openmath.org/OpenMath" "xmlns" as
          , oMFOREIGNId = possibleA fromAttrToStr "id" as
          , oMFOREIGNCdbase = possibleA fromAttrToStr "cdbase" as
          , oMFOREIGNEncoding = possibleA fromAttrToStr "encoding" as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrStr "xmlns" (oMFOREIGNXmlns v)
        , maybeToAttr toAttrFrStr "id" (oMFOREIGNId v)
        , maybeToAttr toAttrFrStr "cdbase" (oMFOREIGNCdbase v)
        , maybeToAttr toAttrFrStr "encoding" (oMFOREIGNEncoding v)
        ]
instance XmlContent OMR where
    fromElem (CElem (Elem "OMR" as []):rest) =
        (Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
        [CElem (Elem "OMR" (toAttrs as) [])]
instance XmlAttributes OMR where
    fromAttrs as =
        OMR
          { oMRXmlns = defaultA fromAttrToStr "http://www.openmath.org/OpenMath" "xmlns" as
          , oMRId = possibleA fromAttrToStr "id" as
          , oMRHref = definiteA fromAttrToStr "OMR" "href" as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrStr "xmlns" (oMRXmlns v)
        , maybeToAttr toAttrFrStr "id" (oMRId v)
        , toAttrFrStr "href" (oMRHref v)
        ]


{-Done-}
