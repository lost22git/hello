import Text.Printf
import Control.Concurrent

main = do
  test_mvar
  test_chan


-- - Control.Concurrent.MVar => SynchronousQueue
-- - Control.Concurrent.TVar => STM
-- - Control.Concurrent.Chan => Channel/BlockingQueue

test_mvar = do
    mv <- newEmptyMVar
    _ <- forkIO $ do
        threadId <- myThreadId
        printf "[%s] MVar sent: 42\n" $ show threadId
        putMVar mv 42
    v <- takeMVar mv
    putStrLn $ "[main] MVar received: " ++ show v

test_chan = do
    ch <- newChan
    _ <- forkIO $ do
        threadId <- myThreadId
        printf "[%s] Chan sent: 42\n" $ show threadId
        writeChan ch 42
    v <- readChan ch
    putStrLn $ "[main] Chan received: " ++ show v



