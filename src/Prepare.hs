module Prepare where

import Csound.Gen.Types
import Csound.Gen.Parse
import Paths_gen_opcodes

main = writeFile "docs.txt" . show =<< getDocs =<< readFile =<< getDataFileName "resources/MiscQuickref.html" 

getDocs :: String -> IO [(String, (String, String))]
getDocs quickRef = do
    docs <- mapM download $ zip counter opcs
    return $ zip names docs
    where 
        counter = reverse [0 .. length opcs - 1]
        names = fmap opcName opcs
        opcs = (allOpcs =<< ) . fst . parse $ quickRef
        download (n, x) = do
            res <- downloadDesc x
            putStrLn $ (opcName x) ++ ": " ++ show n ++ " left."
            return res

        

