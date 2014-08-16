{-# LANGUAGE OverloadedStrings                 #-}
{-# LANGUAGE QuasiQuotes                       #-}
{-# LANGUAGE RecordWildCards                   #-}
{-# LANGUAGE TemplateHaskell                   #-}
{-# LANGUAGE FlexibleContexts                  #-}
{-# LANGUAGE ScopedTypeVariables               #-}

module Yesod.Angular.Macros
    ( mkAngViewADT )
where

import           Language.Haskell.TH           (mkName, DecQ, Q, Dec,
                                                dataD, normalC,
                                                Type(ConT), Strict(..),
                                                Name, ConQ)

mkSimpleValCon :: Name -> [Name] -> ConQ
mkSimpleValCon cname params =
    normalC cname [return (NotStrict, ConT typeC ) | typeC <- params ]

mkSimpleSumProdADT :: Name            -- ^ name of type constructor
                   -> [(Name,[Name])] -- ^ (constructor name, list of parameters)
                   -> [Name]
                   -> DecQ
mkSimpleSumProdADT typConName terms typeClasses =
    dataD (return []) typConName []
          [mkSimpleValCon c ts | (c,ts) <- terms]
          typeClasses

mkAngRouteADT :: [(Name,[Name])] -> DecQ
mkAngRouteADT terms = 
    mkSimpleSumProdADT angRouteADTName terms [''Show, ''Eq]
  where
    angRouteADTName = mkName "AngularRoute" 

mkAngViewADT :: [Name] -> Q [Dec]
mkAngViewADT enumTerms = do
    exp <- mkSimpleSumProdADT angRouteADTName (zip enumTerms $ repeat []) [''Enum, ''Eq, ''Show, ''Read, ''Bounded]
    return [exp]
  where
    angRouteADTName = mkName "AngularViewName" 
