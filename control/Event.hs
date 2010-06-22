module Event where

import Control.Monad
import Control.Applicative ((<$>))
import Data.Maybe (catMaybes)
import qualified Data.Time.Clock.POSIX as T
import qualified Data.Map as M

import Control.Concurrent (forkOS,yield)
import Control.Concurrent.MVar

type Events = M.Map Int Event
data Status = Running | Ready
    deriving Eq

data Event = Event {
    evStatus :: Status,
    evTime :: Double,
    evElapsed :: Double,
    evAction :: Event -> IO (Maybe Event)
}

runEvents :: Int -> [Event] -> IO ()
runEvents threadPoolSize evs = do
    t0 <- fromRational . toRational <$> T.getPOSIXTime
    eVar <- newMVar $ M.fromList $ zip [0..] evs
    replicateM_ (threadPoolSize) (forkOS $ eventThread eVar t0)

eventThread :: MVar Events -> Double -> IO ()
eventThread eVar t0 = do
    now <- fromRational . toRational <$> T.getPOSIXTime
    evs <- readMVar eVar
    mapM_ (runEvent eVar (now - t0)) $ M.keys evs
    yield
    unless (M.null evs) $ eventThread eVar t0

runEvent :: MVar Events -> Double -> Int -> IO ()
runEvent eVar elapsed eid = do
    evs <- takeMVar eVar
    let ev@Event{ evStatus = s, evElapsed = e, evTime = t } = evs M.! eid
    if (s == Ready && elapsed - e > t)
        then do
            -- not already running and due to be run
            putMVar eVar $ M.insert eid (ev { evStatus = Running }) evs
            mEv <- evAction ev $ ev
            case mEv of
                Nothing -> modifyMVar_ eVar (return . M.delete eid)
                Just e -> modifyMVar_ eVar (return . M.insert eid e)
        else putMVar eVar evs

setTimeout :: Double -> IO a -> [Event] -> [Event]
setTimeout t f
    = (:) Event {
        evTime = t,
        evElapsed = 0,
        evAction = \_ -> f >> return Nothing,
        evStatus = Ready
    }

setInterval :: Double -> IO a -> [Event] -> [Event]
setInterval t f
    = (:) Event {
        evTime = t,
        evElapsed = 0,
        evAction = \ev@Event{ evElapsed = e } -> do
            f
            return $ Just (ev { evElapsed = e + t, evStatus = Ready }),
        evStatus = Ready
    }
