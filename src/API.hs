{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE KindSignatures #-}

module API where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except (throwE)
import           Data.Int (Int64)
import           Data.Proxy (Proxy(..))
import           Database.Persist (Key, Entity)
import           Network.Wai.Handler.Warp (run)
import           Servant.API
import           Servant.Client
import           Servant.Server
import           Servant.Utils.StaticFiles
import           Servant.HTML.Blaze (HTML)

import           Database --(fetchUserPG, createUserPG, fetchPostgresConnection, PGInfo, RedisInfo,
                           --fetchUserRedis, cacheUser, fetchRedisConnection, createArticlePG,
                           --fetchArticlePG, fetchArticlesByAuthorPG, fetchRecentArticlesPG)
import           Schema
import           Text.Blaze.Html (Html, preEscapedToHtml)
import qualified Data.Text.IO as T (readFile)
import           Data.Text (unpack, Text(..), append, pack)
import           GHC.TypeLits (Nat)
import           Data.Monoid


type PostRedirect (code :: Nat) loc
  = Verb 'POST code '[JSON] (Headers '[Header "Location" loc] NoContent)


type FullAPI =
             Get '[HTML] Html
        :<|> "home" :> Get '[HTML] Html
        :<|> "home" :> Capture "year" Int :> Get '[HTML] Html
        :<|> Capture "page" Text :> Capture "year" Int :> Get '[HTML] Html
        :<|> Raw
  --      "users" :> Capture "userid" Int64 :> Get '[JSON] User
  -- :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] Int64
  -- :<|> "articles" :> Capture "articleid" Int64 :> Get '[JSON] Article
  -- :<|> "articles" :> ReqBody '[JSON] Article :> Post '[JSON] Int64
  -- :<|> "articles" :> "author" :> Capture "authorid" Int64 :> Get '[JSON] [Entity Article]
  -- :<|> "articles" :> "recent" :> Get '[JSON] [(Entity User, Entity Article)]

api :: Proxy FullAPI
api = Proxy

runServer :: IO ()
runServer = do
  pgInfo <- fetchPostgresConnection
  redisInfo <- fetchRedisConnection
  run 8000 (serve api (fullAPIServer pgInfo redisInfo))

homeHandler :: PGInfo -> RedisInfo -> Int -> Handler Html
homeHandler pgInfo redisInfo year = do
  maybeYear <- liftIO $ getContentByYear pgInfo year
  case maybeYear of
    Just (Year filename _) -> do
      content <- liftIO $ T.readFile $ unpack (append filename ".html" )
      let html = preEscapedToHtml content
      pure html
    _ -> Handler $ (throwE $ err401 { errBody = "Could not find file associated with that year"})

defHomeHandler :: PGInfo -> RedisInfo -> Int -> Handler Html
defHomeHandler = homeHandler --TODO pass year

baseHandler :: PGInfo -> RedisInfo -> Handler Html
baseHandler pgInfo redisInfo = do
  year <- liftIO $ getContentDef pgInfo
  homeHandler pgInfo redisInfo year

customPageHandler :: PGInfo -> RedisInfo -> Text -> Int -> Handler Html
customPageHandler pgInfo redisInfo pageTitle year = do
  maybePath <- liftIO $ getCustomByName pgInfo "customtest"
  case maybePath of
    Just (Custom filename _) -> do
      content <- liftIO $ T.readFile $ unpack (filename <> (pack $ show year) <> ".html")--(append filename ".html")
      let html = preEscapedToHtml content
      pure html
    _ -> Handler $ (throwE $ err401 {errBody = "Could not find custom page of that name"})


fullAPIServer :: PGInfo -> RedisInfo -> Server FullAPI
fullAPIServer pgInfo redisInfo =
       baseHandler pgInfo redisInfo
  :<|> defHomeHandler pgInfo redisInfo 2022
  :<|> homeHandler pgInfo redisInfo
  :<|> customPageHandler pgInfo redisInfo
  :<|> serveDirectoryWebApp "static"


-- fetchUsersHandler :: PGInfo -> RedisInfo -> Int64 -> Handler User
-- fetchUsersHandler pgInfo redisInfo uid = do
--   maybeCachedUser <- liftIO $ fetchUserRedis redisInfo uid
--   case maybeCachedUser of
--     Just user -> return user
--     Nothing -> do
--       maybeUser <- liftIO $ fetchUserPG pgInfo uid
--       case maybeUser of
--         Just user -> liftIO (cacheUser redisInfo uid user) >> return user
--         Nothing -> Handler $ (throwE $ err401 { errBody = "Could not find user with that ID" })

-- createUserHandler :: PGInfo -> User -> Handler Int64
-- createUserHandler pgInfo user = liftIO $ createUserPG pgInfo user

-- fetchArticleHandler :: PGInfo -> Int64 -> Handler Article
-- fetchArticleHandler pgInfo aid = do
--   maybeArticle <- liftIO $ fetchArticlePG pgInfo aid
--   case maybeArticle of
--     Just article -> return article
--     Nothing -> Handler $ (throwE $ err401 { errBody = "Could not find article with that ID" })

-- createArticleHandler :: PGInfo -> Article -> Handler Int64
-- createArticleHandler pgInfo article = liftIO $ createArticlePG pgInfo article

-- fetchArticlesByAuthorHandler :: PGInfo -> Int64 -> Handler [Entity Article]
-- fetchArticlesByAuthorHandler pgInfo uid = liftIO $ fetchArticlesByAuthorPG pgInfo uid

-- fetchRecentArticlesHandler :: PGInfo -> Handler [(Entity User, Entity Article)]
-- fetchRecentArticlesHandler pgInfo = liftIO $ fetchRecentArticlesPG pgInfo

  -- (fetchUsersHandler pgInfo redisInfo) :<|>
  -- (createUserHandler pgInfo) :<|>
  -- (fetchArticleHandler pgInfo) :<|>
  -- (createArticleHandler pgInfo) :<|>
  -- (fetchArticlesByAuthorHandler pgInfo) :<|>
  -- (fetchRecentArticlesHandler pgInfo)

-- fetchUserClient :: Int64 -> ClientM User
-- createUserClient :: User -> ClientM Int64
-- fetchArticleClient :: Int64 -> ClientM Article
-- createArticleClient :: Article -> ClientM Int64
-- fetchArticlesByAuthorClient :: Int64 -> ClientM [Entity Article]
-- fetchRecentArticlesClient :: ClientM [(Entity User, Entity Article)]
-- ( fetchUserClient             :<|>
--   createUserClient            :<|>
--   fetchArticleClient          :<|>
--   createArticleClient         :<|>
--   fetchArticlesByAuthorClient :<|>
--   fetchRecentArticlesClient )  = client (Proxy :: Proxy FullAPI)
