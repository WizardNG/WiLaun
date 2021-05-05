module FileControl (
                     isOwner
                   , isGroup
                   , isExec
                   , isRead
                   , isWrite
                   ) where
    
import Control.Monad

import System.Posix.Files
import System.Posix.Types
import System.FilePath.Posix
import System.Posix.User

isOwner :: FilePath -> IO Bool
isOwner f = liftM2 (==) (fileOwner <$> getFileStatus f) getRealUserID  

isGroup :: FilePath -> IO Bool
isGroup f = liftM2 (==) (fileGroup <$> getFileStatus f) getRealGroupID 

checkMode :: FileMode -> FileMode -> Bool
checkMode fm cm = cm == intersectFileModes fm cm

isExec :: FilePath  -> IO Bool
isExec fp = do
    fm <- fileMode <$> getFileStatus fp
    own <- isOwner fp
    grp <- isGroup fp
    let ex =  ((own &&checkMode fm ownerExecuteMode) 
            || (grp &&checkMode fm groupExecuteMode)
            || checkMode fm otherExecuteMode)
            && not (checkMode fm directoryMode)
    return ex

isRead :: FilePath  -> IO Bool
isRead fp = do
    fm <- fileMode <$> getFileStatus fp
    own <- isOwner fp
    grp <- isGroup fp
    let ex =  ((own &&checkMode fm ownerReadMode) 
            || (grp &&checkMode fm groupReadMode)
            || checkMode fm otherReadMode)
            && not (checkMode fm directoryMode)
    return ex

isWrite :: FilePath  -> IO Bool
isWrite fp = do
    fm <- fileMode <$> getFileStatus fp
    own <- isOwner fp
    grp <- isGroup fp
    let ex =  ((own &&checkMode fm ownerWriteMode) 
            || (grp &&checkMode fm groupWriteMode)
            || checkMode fm otherWriteMode)
            && not (checkMode fm directoryMode)
    return ex

