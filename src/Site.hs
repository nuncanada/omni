{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Lens
import           Control.Monad.Logger
import           Control.Monad.State
import           Control.Monad.Trans
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Reader
import           Data.ByteString                             (ByteString)
import           Data.Map.Syntax                             (( ## ))
import           Data.Monoid
import qualified Data.Text                                   as T
import qualified Data.Text.Encoding                          as T
import qualified Database.Persist                            as P
import           Database.Persist.Sql
import qualified Heist.Interpreted                           as I
import           Snap
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.Persistent
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Persistent
import           Snap.Snaplet.Session
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe

import           Control.Applicative                         (pure)
import           Control.Monad.Logger                        (MonadLogger,
                                                              monadLoggerLog)



------------------------------------------------------------------------------
import           Application


instance MonadLogger IO where
    monadLoggerLog _ _ _ = pure $ pure ()

------------------------------------------------------------------------------
-- | Render login form
handleLogin :: Maybe T.Text -> Handler App (AuthManager App) ()
handleLogin authError = heistLocal (I.bindSplices errs) $ render "login"
  where
    errs = maybe mempty splice authError
    splice err = "loginError" ## I.textSplice err


------------------------------------------------------------------------------
-- | Handle login submit
handleLoginSubmit :: Handler App (AuthManager App) ()
handleLoginSubmit =
    loginUser "login" "password" Nothing
              (\_ -> handleLogin err) (redirect "/")
  where
    err = Just "Unknown user or password"


------------------------------------------------------------------------------
-- | Logs out and redirects the user to the site index.
handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"


------------------------------------------------------------------------------
-- | Handle new user form submit
handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = method GET handleForm <|> method POST handleFormSubmit
  where
    handleForm = render "new_user"
    handleFormSubmit = registerUser "login" "password" >> redirect "/"


------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/login",     with auth handleLoginSubmit)
         , ("/logout",    with auth handleLogout)
         , ("/new_user",  with auth handleNewUser)
         , ("/hello",     writeText "hello")
         , ("foo",        fooHandler)
         , ("add/:uname", addHandler)
         , ("",           serveDirectory "static")
         ]

fooHandler = do
    results <- runPersist $ P.selectList [] []
    liftIO $ print (map db2au results)

addHandler = do
    mname <- getParam "uname"
    let name = maybe "guest" T.decodeUtf8 mname
    u <- with auth $ createUser name ""
    liftIO $ print u

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $
         initCookieSessionManager "site_key.txt" "sess" (Just 3600)
    d <- nestSnaplet "db" db $ initPersist (runMigrationUnsafe migrateAuth)
    a <- nestSnaplet "auth" auth $
         initPersistAuthManager sess (persistPool $ view snapletValue d)

    addRoutes routes
    addAuthSplices h auth
    return $ App h s d a

