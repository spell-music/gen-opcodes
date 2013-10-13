module Pretty(
    prettyModules, mainModule
) where

import Data.List
import Data.Char

import Text.PrettyPrint.Leijen

import Types

mainModule :: PackageType -> [Chap] -> String
mainModule packageType chaps = pp $ vCat 
    [ text ("module Csound." ++ (show packageType) ++ ".Opcode") <+> nest 4 (parens $
        vcat $ punctuate comma $ fmap ((text "module" <+> ) . text . fullName packageType) names) <+> text "where"
    , vcat $ fmap ((text "import" <+> ) . text . fullName packageType) names
    ]
    where names = fmap nodeName chaps

-- noisy head
headBoom :: String -> [a] -> a
headBoom msg as = case as of
    []  -> error msg
    _   -> head as

vCat :: [Doc] -> Doc
vCat = vcat . punctuate line

------------------------------------------------------------------

type RenderOpc = Opc -> Doc

renderOpc :: PackageType -> RenderOpc
renderOpc x = case x of
    Dynamic -> prettyOpcDynamic
    Typed   -> prettyOpcTyped

data Import = SimpleImport String | QualifiedImport String String

imports :: PackageType -> [Import]
imports x = case x of
    Dynamic  -> [SimpleImport "Csound.Dynamic"]
    Typed    -> [QualifiedImport "Csound.Dynamic.Opcode" anAlias, SimpleImport "Csound.Typed"]

anAlias :: String
anAlias = "D"

------------------------------------------------------------------

prettyModules :: PackageType -> [Chap] -> [(String, String)]
prettyModules packageType = 
    fmap $ \x -> (nodeName x, pp (pChap packageType (imports packageType) (renderOpc packageType) $ removeEmptySecs x))

pp a = displayS (renderPretty 0.5 200 a) ""

removeEmptySecs :: Chap -> Chap
removeEmptySecs a = a { nodeItems = filter (not . null . nodeItems) $ nodeItems a }

pChap :: PackageType -> [Import] -> RenderOpc -> Chap -> Doc
pChap packageType imports renderOpc a = vCat [chapHeader, chapImports, chapBody]
    where 
        chapHeader  = hsHeader (fullName packageType $ nodeName a) (fmap getFuns $ nodeItems a)
        chapBody    = vCat $ fmap (pSec renderOpc $ nodeName a) $ nodeItems a
        chapImports = hsImports imports 

        getFuns x = (nodeName x, fmap hsOpcName $ nodeItems x)

fullName :: PackageType -> String -> String
fullName packageType a = "Csound." ++ (show packageType) ++ ".Opcode." ++ a

pSec :: RenderOpc -> String -> Sec -> Doc
pSec renderOpc chapName a = vCat
    [ text "--" <+> text (nodeName a)
    , vCat $ fmap renderOpc $ nodeItems a ]

---------------------------------------------------------------------------------------
-- pretty dynamic opcode

prettyOpcDynamic :: Opc -> Doc
prettyOpcDynamic a = vcat 
    [ pretty $ opcDoc a
    , hsFun (hsOpcName a) (opcDynamicSignature a) (opcDynamicBody a) ]


opcDynamicSignature :: Opc -> Doc
opcDynamicSignature a = ins <+> outs
    where
        ins 
            | isConstant a  = empty
            | otherwise     = text "[E] ->"
    
        outs = text $ case opcType a of
            PureSingle      -> "E"
            DirtySingle     -> "Dep E"
            PureMulti       -> "MultiOut [E]"
            DirtyMulti      -> "MultiOut (Dep [E])"
            Procedure       -> "Dep ()"

opcDynamicBody :: Opc -> Doc
opcDynamicBody a = hsep [cons, name, signature, mConst]
    where
        cons = text $ case opcType a of
            PureSingle      -> "opcs"
            DirtySingle     -> "dep . opcs"
            PureMulti       -> "mopcs"
            DirtyMulti      -> "mdep . mopcs"
            Procedure       -> "dep_ . opcs"
        
        name = dquotes $ text $ opcName a    

        signature = pretty $ rates $ opcSignature a

        mConst 
            | isConstant a  = text "$ []"
            | otherwise     = empty

---------------------------------------------------------------------------------------
-- pretty typed opcode

prettyOpcTyped :: Opc -> Doc
prettyOpcTyped a = vcat 
    [ pretty $ opcDoc a
    , hsFun (hsOpcName a) (opcTypedSignature a) (opcTypedBody a) ]

opcTypedSignature :: Opc -> Doc
opcTypedSignature = pretty . types . opcSignature

opcTypedBody :: Opc -> Doc
opcTypedBody a 
    | isConstant a  = hsep [cons, text "$ const", name]
    | otherwise     = addArgs $ hsep [cons, name]
    where 
        name = text $ qualifiedHsOpcName a
        cons = onMulti $ text $ firstLower $ show $ opcType a
        onMulti x = case opcType a of
            PureMulti   -> text "fromPm $" <+> x
            DirtyMulti  -> text "fromDm $" <+> x
            _           -> x

        -- assumption: one of the addSigOrDArgs or addMultiArgs
        -- is always identity function. 
        addArgs = addSigOrDArgs . addMultiArgs

        addSigOrDArgs = case outTypes $ types $ opcSignature a of
            SingleOut SigOrD        -> args "fromGE $"
            SE (SingleOut SigOrD)   -> args "fmap fromGE $"
            _                       -> id
            where
                args fun body = case inTypes $ types $ opcSignature a of
                    InTypes xs | length xs == 0 -> text fun <+> body
                    InTypes xs                  -> 
                        let args = fmap ((char 'a' <> ) . int) [1 .. length xs]
                        in  hsep [char '\\' <> hsep args, text "->", text fun, body, hsep $ zipWith convertSigOrDs xs args]
                convertSigOrDs ty arg = case ty of
                    SigOrD  -> parens $ text "toGE" <+> arg
                    _       -> arg                                         

        addMultiArgs = case opcType a of
            PureMulti   -> args
            DirtyMulti  -> args
            _           -> id
            where                
                args = case inTypes $ types $ opcSignature a of
                    InTypes xs | length xs == 0 -> id
                    InTypes xs                  -> 
                        let args = hsep $ fmap ((char 'a' <> ) . int) [1 .. length xs]
                        in  \x -> hsep [char '\\' <> args, text "->", x, args]


---------------------------------------------------------------------------------------
-- pretty primitives

instance Pretty OpcDoc where
    pretty a = vcat $ intersperse (text "--") [desc, signature, link] 
        where
            desc = text "-- |" <+> text (opcDocDescription a)

            signature = vcat $ fmap ((text "-- >" <+>) . text) $  nestTail . lines =<< opcDocCode a
                where                
                    nestTail xs = case xs of
                        []      -> []
                        [y]     -> [y]
                        y:ys    -> y : fmap ("    " ++ ) ys

            link = text "-- csound doc:" <+> text (inUrl $ fullPath $ opcDocLink a)
                where 
                    fullPath x = "http://www.csounds.com/manual/html/" ++ x
                    inUrl x = "<" ++ x ++ ">"

instance Pretty Rates where
    pretty x = case x of
        Single rs -> pretty rs
        Multi a b -> pretty (a, b)

instance Pretty RateList where
    pretty x = case x of
        JustList as -> pretty as
        Repeat rate -> parens $ text "repeat" <+> pretty rate
        Append a b  -> pretty a <+> text "++" <+> pretty b

instance Pretty Types where
    pretty a = hsep [pConstr (outTypes a), ins, pretty (outTypes a)]
        where 
            pConstr x = case x of
                OutTuple    -> text "Tuple a =>"
                Tuple       -> text "Tuple a =>"
                -- assumption: SigOrD can be only in Single or SE output
                SingleOut SigOrD -> text $ "SigOrD " ++ nameSigOrD ++ " =>"
                SE a        -> pConstr a
                _           -> empty

            ins = case inTypes a of
                InTypes []  -> empty
                _           -> hsep [pretty $ inTypes a, text "->"]

instance Pretty InTypes where
    pretty (InTypes as) = hsep $ intersperse (text "->") $ fmap pretty as

instance Pretty OutTypes where
    pretty x = case x of
        OutTuple    -> text "a"
        Tuple       -> text "a"
        TheTuple ts -> tupled $ fmap pretty ts
        SingleOut a -> pretty a
        SE (SE a)   -> pretty (SE a)
        OutNone     -> text "SE ()"
        SE OutNone  -> text "SE ()"
        SE a        -> text "SE" <+> pretty a

instance Pretty Rate where
    pretty = text . show

instance Pretty Type where
    pretty x = case x of
        TypeList a  -> pretty [a]        
        SigOrD      -> text nameSigOrD
        _           -> text $ show x

nameSigOrD = "a"

-----------------------------------------------------------

qualifiedHsOpcName :: Opc -> String
qualifiedHsOpcName a = anAlias ++ "." ++ hsOpcName a

hsOpcName :: Opc -> String
hsOpcName = resolveCollisions . toHsName . opcName
    where
        toHsName ('S' : 'T' : 'K' : rest) = "stk" ++ firstUpper rest

        toHsName x 
            | isFirstUpper x = firstLower x
            | null pre  = x
            | null post = lowerPre
            | otherwise = lowerPre ++ firstUpper post
            where
                (pre, post) = span isUpper x
                lowerPre = fmap toLower pre

        isFirstUpper x = case x of
            (a:b:_) -> isUpper a && isLower b
            _       -> False

        resolveCollisions x
            | x `elem` specNames    = x ++ "'"
            | otherwise             = x

        specNames = ["in", "product", "sum", "max", "min", "print"]
        


--------------------------------------------------------------------
-- haskell syntax structures 

hsHeader :: String -> [(String, [String])] -> Doc
hsHeader name secs = text "module" <+> text name <+> nest 4 (parens 
    (vCat (line : (punctuate comma $ fmap (uncurry fromSec) secs)))) <+> text "where"
    where
        fromSec title funs = vcat 
            [ if null title then empty else text "-- *" <+> text title 
            , hsep $ punctuate comma $ fmap text funs ]
    
hsImports :: [Import] -> Doc
hsImports as = vcat $ fmap pretty as

instance Pretty Import where
    pretty x = case x of
        SimpleImport    name        -> text "import" <+> text name
        QualifiedImport name alias  -> text "import qualified" <+> text name <+> text "as" <+> text alias

hsFun :: String -> Doc -> Doc -> Doc
hsFun name ty body = vcat [hsDef name ty, hsConst name body] 

hsDef :: String -> Doc -> Doc
hsDef name ty = text name <+> text "::" <+> ty

hsConst :: String -> Doc -> Doc
hsConst name body = text name <+> char '=' <+> body
