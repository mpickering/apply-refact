{-# LANGUAGE TemplateHaskell,EmptyDataDecls #-} 
$(fmap return $ dataD (return []) (mkName "Void") [] [] [])