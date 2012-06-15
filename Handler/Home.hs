{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Data.Time (getCurrentTime)
import Yesod.Form.Nic
import Yesod.Auth

-- Handler for the home page.  Just displays a list of posts.

getHomeR :: Handler RepHtml
getHomeR = do
  entries <- runDB $ selectList [] [Desc EntryPosted]
  defaultLayout $ do
    setTitle "How do you plan to make money this week?"
    $(widgetFile "homepage")

-- Creating new entries: GET displays the new-post form, and POST
-- creates it (and redirects to the new post)

getPostR :: Handler RepHtml
getPostR = do
  (formWidget, encType) <- generateFormPost newpostForm
  defaultLayout $ do
      setTitle "New Post"
      $(widgetFile "newpost")

postPostR :: Handler RepHtml
postPostR = do
  ((res, _formWidget), _encType) <- runFormPost newpostForm
  case res of
    FormSuccess entry -> do
        entryId <- runDB $ insert entry
        setMessage "Your post was created"
        redirect $ EntryR entryId
    _ -> do
        setMessage "Error creating your post, sorry"
        redirect PostR

-- Entries themselves.  GET displays the individual entry; POST adds a
-- comment.

getEntryR :: EntryId -> Handler RepHtml
getEntryR entryId = do
  (entry, comments) <- runDB $ do
         entry    <- get404 entryId
         comments <- selectList [CommentEntry ==. entryId] [Asc CommentPosted]
         return (entry, map entityVal comments)
  (commentWidget, encType) <- generateFormPost (commentForm entryId)
  defaultLayout $ do
    setTitle $ toHtml $ entryTitle entry
    $(widgetFile "entry")

postEntryR :: EntryId -> Handler RepHtml
postEntryR entryId = do
  ((res, _formWidget), _encType) <- runFormPost $ commentForm entryId
  case res of
    FormSuccess comment -> do
        _ <- runDB $ insert comment
        setMessage "Comment posted"
        redirect $ EntryR entryId
    _ -> do
        setMessage "Couldn't add your comment, sorry"
        redirect $ EntryR entryId

-- Forms:

newpostForm :: Form Entry
newpostForm = renderBootstrap $ Entry
    <$> areq textField "Title" Nothing
    <*> aformM requireAuthId
    <*> aformM (liftIO getCurrentTime)
    <*> areq nicHtmlField "Post" Nothing

commentForm :: EntryId -> Form Comment
commentForm entryId = renderBootstrap $ Comment
    <$> pure entryId
    <*> aformM (liftIO getCurrentTime)
    <*> aformM requireAuthId
    <*> areq textField "Comment" Nothing
