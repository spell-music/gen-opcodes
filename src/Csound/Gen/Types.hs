module Csound.Gen.Types where

import Data.Char
import Data.Maybe

data PackageType = Dynamic | Typed
    deriving (Show)

data Node a = Node 
    { nodeName  :: String
    , nodeItems :: [a]
    } deriving (Show)

type Chap   = Node Sec
type Sec    = Node Opc

data Opc = Opc
    { opcName       :: String
    , opcSignature  :: Signature
    , opcDoc        :: OpcDoc 
    } deriving (Show)

data Signature = Signature
    { rates         :: Rates
    , types         :: Types 
    } deriving (Show)

data Rates 
    = Single [(Rate, RateList)] | Multi RateList RateList
    | Opr1 | Opr1k | InfOpr 
    | SingleOpr [(Rate, RateList)]
    deriving (Show)

data RateList = JustList [Rate] | Repeat Rate | Append RateList RateList
    deriving (Show)

data Types = Types
    { inTypes   :: InTypes
    , outTypes  :: OutTypes 
    } deriving (Show)

newtype InTypes = InTypes [Type]
    deriving (Show)

data OutTypes 
    = OutTuple | Tuple | TheTuple [Type] 
    | SingleOut Type | SE OutTypes | OutNone
    deriving (Show)

data OpcDoc = OpcDoc
    { opcDocShortDescription    :: String
    , opcDocLongDescription     :: String
    , opcDocCode                :: [String]
    , opcDocLink                :: String
    } deriving (Show)

data Rate = Xr | Ar | Kr | Ir | Sr | Fr | Wr | Tvar 
    deriving (Show, Eq)

data Type = Sig | D | Tab | Str | Spec | Wspec | TvarType | Msg | TypeList Type | SigOrD
    deriving (Show, Eq)

data OpcType 
    = PureSingle | DirtySingle | Procedure 
    | PureMulti  | DirtyMulti
    deriving (Show, Eq)
    
isConstant :: Opc -> Bool 
isConstant = isConst . rates . opcSignature
    where 
        isConst x = case x of
            Single rs   -> all (isNull . snd) rs
            Multi _ ins -> isNull ins
            _           -> False

        isNull x = case x of
            JustList []     -> True
            _               -> False
  
opcType :: Opc -> OpcType
opcType a = case outTypes $ types $ opcSignature a of
    SingleOut _         -> PureSingle
    SE (SingleOut _)    -> DirtySingle
    OutNone             -> Procedure
    SE OutNone          -> Procedure
    x    | isTuple x    -> PureMulti
    SE x | isTuple x    -> DirtyMulti
    where 
        isTuple x = case x of
            OutTuple    -> True
            Tuple       -> True
            TheTuple _  -> True
            _           -> False
    
getSingleRateList :: RateList -> Maybe Rate
getSingleRateList x = case x of
    JustList [a]    -> Just a
    _               -> Nothing
    
isSingleRateList :: RateList -> Bool
isSingleRateList = isJust . getSingleRateList

isEmptyRateList :: RateList -> Bool
isEmptyRateList x = case x of
    JustList []     -> True
    _               -> False

longerRateList :: RateList -> RateList -> Ordering
longerRateList x y = case (x, y) of
    (JustList a, JustList b) -> length a `compare` length b
    (JustList _, _)          -> LT
    (_         , JustList _) -> GT
    _                        -> EQ

isDirty :: OpcType -> Bool
isDirty x = case x of
    DirtySingle -> True
    Procedure   -> True
    DirtyMulti  -> True
    _           -> False
    
isPure :: OpcType -> Bool
isPure = not . isDirty

packageName packageType = "csound-expression-opcodes-" ++ firstLower (show packageType)

cabalFileName packageType = packageName packageType ++ ".cabal"
libCabalFileName packageType = packageName packageType ++ "/" ++ cabalFileName packageType 

fullModuleName packageType name =  "Csound." ++ show packageType ++  ".Opcode." ++ name
fullPath packageType name = packageName packageType ++ "/src/Csound/" ++ show packageType ++ "/Opcode/" ++ name ++ ".hs"
mainModuleName packageType = "Csound." ++ show packageType ++ ".Opcode"
mainModulePath packageType =  packageName packageType ++ "/src/Csound/" ++ show packageType ++ "/Opcode.hs"

----------------------------------------------------------

firstLower, firstUpper :: String -> String

firstUpper x = toUpper (head x) : tail x
firstLower x = toLower (head x) : tail x


allOpcs :: Chap -> [Opc]
allOpcs = (nodeItems =<< ) . nodeItems
    

