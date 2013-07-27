module Handler.Task
    ( getTaskR
    , putTaskR
    , deleteTaskR
    , postTaskToggleDoneR
    ) where

import Import

import Data.Time

getTaskR :: TaskId -> Handler Value
getTaskR tid = do
    task <- runDB $ get404 tid
    return $ toJSONWithId tid task

putTaskR :: TaskId -> Handler RepPlain
putTaskR tid = do
    taskUpdate <- parseJsonBody_
    time <- liftIO getCurrentTime
    runDB $ do
        task <- get404 tid
        let task' = updateTask time task taskUpdate
        replace tid task'
    return $ RepPlain emptyContent

deleteTaskR :: TaskId -> Handler RepPlain
deleteTaskR tid = do
    runDB $ delete tid
    return $ RepPlain emptyContent

postTaskToggleDoneR :: TaskId -> Handler RepPlain
postTaskToggleDoneR tid = do
    runDB $ do
        task <- get404 tid
        update tid [TaskDone =. not (taskDone task)]
    return $ RepPlain emptyContent
