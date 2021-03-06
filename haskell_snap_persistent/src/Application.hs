{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import Control.Lens
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import Snap.Snaplet.Persistent
import Snap.Snaplet.Session

------------------------------------------------------------------------------
data App = App
    { _heist :: Snaplet (Heist App)
    , _sess  :: Snaplet SessionManager
    , _db    :: Snaplet PersistState
    , _auth  :: Snaplet (AuthManager App)
    }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist


instance HasPersistPool (Handler b App) where
    getPersistPool = with db getPersistPool

------------------------------------------------------------------------------
type AppHandler = Handler App App


