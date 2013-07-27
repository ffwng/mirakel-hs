module Handler.List
    ( getListsR
    , postListsR
    , getListR
    , putListR
    , deleteListR
    , postListSortByR
    , postListMoveR
    , getListTasksR
    , postListTasksR
    ) where

import Import

import qualified Database.Esqueleto as E
import Database.Esqueleto
    (select, from, where_, orderBy, val, (^.), asc, desc, Esqueleto)

import Data.Time

getListsR :: Handler Value
getListsR = do
    uid <- getUserId
    lists <- runDB $ selectList [ListUserId ==. uid] [Asc ListPosition]
    let listsWithId = map (\(Entity k v) -> toJSONWithId k v) lists
    returnJson listsWithId

postListsR :: Handler RepPlain
postListsR = do
    uid <- getUserId
    listCreate <- parseJsonBody_
    time <- liftIO getCurrentTime
    lid <- runDB $ do
        cnt <- count [ListUserId ==. uid]
        let newPos = succ $ fromIntegral cnt
        let list = createList uid time newPos listCreate
        insert list
    sendResponseCreated (ListR lid)

getListR :: ListId -> Handler Value
getListR lid = do
    list <- runDB $ get404 lid
    return $ toJSONWithId lid list

putListR :: ListId -> Handler RepPlain
putListR lid = do
    listUpdate <- parseJsonBody_
    time <- liftIO getCurrentTime
    runDB $ do
        list <- get404 lid
        let list' = updateList time list listUpdate
        replace lid list'
    return $ RepPlain emptyContent

deleteListR :: ListId -> Handler RepPlain
deleteListR lid = do
    runDB $ delete lid
    return $ RepPlain emptyContent

postListSortByR :: ListId -> TasksSorting -> Handler RepPlain
postListSortByR lid s = do
    runDB $ update lid [ListSorting =. s]
    return $ RepPlain emptyContent

postListMoveR :: ListId -> ListMovement -> Handler RepPlain
postListMoveR lid1 mov = do
    runDB $ case mov of
        First -> moveListAfter lid1 0
        After lid2 -> do
            pos <- listPosition <$> get404 lid2
            moveListAfter lid1 pos
    return $ RepPlain emptyContent

moveListAfter :: ListId -> Position -> SqlPersistT (HandlerT site IO) ()
moveListAfter lid pos = do
    curpos <- listPosition <$> get404 lid
    if curpos > pos then do
        updateWhere [ListPosition >. pos, ListPosition <. curpos] [ListPosition +=. 1]
        update lid [ListPosition =. succ pos]
    else if curpos < pos then do
        updateWhere [ListPosition >. curpos, ListPosition <=. pos] [ListPosition -=. 1]
        update lid [ListPosition =. pos]
    else return ()

getListTasksR :: ListId -> Handler Value
getListTasksR lid = do
    tasks <- runDB $ do
        sorting <- listSorting <$> get404 lid
        select $ from $ \t -> do
            where_ (t ^. TaskListId E.==. val lid)
            orderBySorting sorting t
            return t
    let tasksWithId = map (\(Entity k v) -> toJSONWithId k v) tasks
    returnJson tasksWithId
    where
        orderBySorting :: Esqueleto query expr b
                  => TasksSorting
                  -> expr (Entity (TaskGeneric b1))
                  -> query ()
        orderBySorting ts t = orderBy $ case ts of
            Done -> [asc $ t ^. TaskDone]
            Due -> [asc (E.isNothing $ t ^. TaskDue), asc (t ^. TaskDue)]
            Priority -> [desc $ t ^. TaskPriority]
            Created -> [asc $ t ^. TaskCreatedAt]


postListTasksR :: ListId -> Handler RepPlain
postListTasksR lid = do
    taskCreate <- parseJsonBody_
    time <- liftIO getCurrentTime
    let task = createTask lid time taskCreate
    tid <- runDB $ insert task
    sendResponseCreated (TaskR tid)
