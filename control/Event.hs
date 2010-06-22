module Event where

import Control.Monad
import Control.Applicative ((<$>))
import Data.Maybe (catMaybes)
import qualified Data.Time.Clock.POSIX as T

data Event = Event {
    evTime :: Double,
    evElapsed :: Double,
    evAction :: Event -> IO (Maybe Event)
}

runEvents :: [Event] -> IO ()
runEvents evs = do
    t0 <- fromRational . toRational <$> T.getPOSIXTime
    runEvents' t0 evs

runEvents' :: Double -> [Event] -> IO ()
runEvents' _ [] = return ()
runEvents' t0 evs = do
    now <- fromRational . toRational <$> T.getPOSIXTime
    runEvents' t0 . catMaybes =<< mapM (runEvent (now - t0)) evs

runEvent :: Double -> Event -> IO (Maybe Event)
runEvent elapsed ev@Event{ evTime = t, evElapsed = e, evAction = f }
    = if (elapsed - e > t)
        then f ev
        else return (Just ev)

setTimeout :: Double -> IO a -> [Event] -> [Event]
setTimeout t f
    = (:) Event {
        evTime = t,
        evElapsed = 0,
        evAction = \_ -> f >> return Nothing
    }

setInterval :: Double -> IO a -> [Event] -> [Event]
setInterval t f
    = (:) Event {
        evTime = t,
        evElapsed = 0,
        evAction = \ev@Event{ evElapsed = e } -> do
            f
            return $ Just (ev { evElapsed = e + t })
    }
