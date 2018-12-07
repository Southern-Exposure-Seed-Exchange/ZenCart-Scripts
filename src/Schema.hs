{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Schema where

import           Data.Text                      ( Text )
import           Database.Persist.TH



share [mkPersist sqlSettings] [persistLowerCase|
Product sql=products
    Id sql=products_id
    model Text sql=products_model
    status Bool sql=products_status
    price Rational sql=products_price sqltype=decimal(15,4)

ProductIcon sql=sese_products_icons
    Id sql=icon_id
    product ProductId sql=products_id
    isOrganic Bool sql=is_organic
    isEco Bool sql=is_eco

ProductDescription sql=products_description
    product ProductId sql=products_id
    language Int sql=language_id
    name Text sql=products_name
    description Text sql=products_description
    Primary product language
|]
