{-# Language OverloadedStrings #-}
module Main where

import qualified Data.Tree as T
import Data.String
import Shelly

import Types
import Parse
import Pretty

import Paths_gen_opcodes

main = do 
    mainBy Dynamic
    mainBy Typed

-- print "hi" -- mainBy Dynamic

mkProjectDir :: PackageType -> IO ()
mkProjectDir packageType = shelly $ mkdirTree $ 
    (fromString $ packageName packageType) # 
        [ "src" # 
            [ "Csound" #
                [ (fromString $ show packageType) #
                    [ "Opcode" # []] 
                ]
            ]
        ]
    where 
        (#) = T.Node

mainBy :: PackageType -> IO ()
mainBy packageType = do
    mkProjectDir packageType
    (chaps, _) <- parsed
    mapM_ (uncurry $ saveFile packageType) $ prettyModules packageType $ chaps
    saveMainModule packageType chaps
    saveCabalFile packageType chaps
    saveLicenseFile packageType
    saveSetupFile packageType

parsed :: IO ([Chap], [Unparsed])
parsed = fmap parse . readFile =<< getDataFileName "resources/MiscQuickref.html" 

saveFile :: PackageType -> String -> String -> IO ()
saveFile packageType fileName fileText = writeFile (fullPath packageType fileName) fileText
    
saveMainModule :: PackageType -> [Chap] -> IO ()
saveMainModule packageType a = writeFile (mainModulePath packageType) $ mainModule packageType a

saveCabalFile :: PackageType -> [Chap] -> IO ()
saveCabalFile packageType chaps = 
    writeFile (libCabalFileName packageType) . appendDepsToCabalFile packageType chaps 
        =<< readFile =<< getDataFileName ("resources/" ++ cabalFileName packageType)

appendDepsToCabalFile :: PackageType -> [Chap] -> String -> String
appendDepsToCabalFile packageType as = ( ++ deps) 
    where 
        deps = unlines $ fmap nest $ (mainModuleName packageType : ) $ fmap (fullModuleName packageType . nodeName) as
        nest = (replicate 4 ' ' ++ )
        
saveLicenseFile :: PackageType -> IO ()
saveLicenseFile packageType =
    writeFile (packageName packageType ++ "/LICENSE") 
    =<< readFile =<< getDataFileName ("resources/LICENSE")
        
saveSetupFile :: PackageType -> IO ()
saveSetupFile packageType =
    writeFile (packageName packageType ++ "/Setup.hs") 
    =<< readFile =<< getDataFileName ("resources/Setup.hs")

