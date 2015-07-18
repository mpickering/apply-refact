
{- | Client inner-loop

     This function is generally only needed if you are adding a new communication channel.
-}
processRemoteState :: IsAcidic st =>
                      IO CommChannel -- ^ (re-)connect function
                   -> IO (AcidState st)
processRemoteState reconnect
  = do cmdQueue    <- atomically newTQueue
       ccTMV       <- atomically newEmptyTMVar
       isClosed    <- newIORef False

       let actor :: Command -> IO (MVar Response)
           actor command =
               do debugStrLn "actor: begin."
                  readIORef isClosed >>= flip when (throwIO AcidStateClosed)
                  ref <- newEmptyMVar
                  atomically $ writeTQueue cmdQueue (command, ref)
                  debugStrLn "actor: end."
                  return ref

           expireQueue listenQueue =
               do mCallback <- atomically $ tryReadTQueue listenQueue
                  case mCallback of
                    Nothing         -> return ()
                    (Just callback) ->
                        do callback ConnectionError
                           expireQueue listenQueue

           handleReconnect :: SomeException -> IO ()
           handleReconnect e
             = case fromException e of
                 (Just ThreadKilled) ->
                     do debugStrLn "handleReconnect: ThreadKilled. Not attempting to reconnect."
                        return ()
                 _ ->
                   do debugStrLn $ "handleReconnect begin."
                      tmv <- atomically $ tryTakeTMVar ccTMV
                      case tmv of
                        Nothing ->
                            do debugStrLn $ "handleReconnect: error handling already in progress."
                               debugStrLn $ "handleReconnect end."
                               return ()
                        (Just (oldCC, oldListenQueue, oldListenerTID)) ->
                            do thisTID <- myThreadId
                               when (thisTID /= oldListenerTID) (killThread oldListenerTID)
                               ccClose oldCC
                               expireQueue oldListenQueue
                               cc <- reconnect
                               listenQueue <- atomically $ newTQueue
                               listenerTID <- forkIO $ listener cc listenQueue
                               atomically $ putTMVar ccTMV (cc, listenQueue, listenerTID)
                               debugStrLn $ "handleReconnect end."
                               return ()

           listener :: CommChannel -> TQueue (Response -> IO ()) -> IO ()
           listener cc listenQueue
             = getResponse Strict.empty `catch` handleReconnect
               where
                 getResponse leftover =
                     do debugStrLn $ "listener: listening for Response."
                        let go inp = case inp of
                                   Fail msg _     -> error msg
                                   Partial cont   -> do debugStrLn $ "listener: ccGetSome"
                                                        bs <- ccGetSome cc 1024
                                                        go (cont bs)
                                   Done resp rest -> do debugStrLn $ "listener: getting callback"
                                                        callback <- atomically $ readTQueue listenQueue
                                                        debugStrLn $ "listener: passing Response to callback"
                                                        callback (resp :: Response)
                                                        return rest
                        rest <- go (runGetPartial get leftover) -- `catch` (\e -> do handleReconnect e
                                                                --                   throwIO e
                                                                 --        )
                        getResponse rest

           actorThread :: IO ()
           actorThread = forever $
             do debugStrLn "actorThread: waiting for something to do."
                (cc, cmd) <- atomically $
                  do (cmd, ref)        <- readTQueue cmdQueue
                     (cc, listenQueue, _) <- readTMVar ccTMV
                     writeTQueue listenQueue (putMVar ref)
                     return (cc, cmd)
                debugStrLn "actorThread: sending command."
                ccPut cc (encode cmd) `catch` handleReconnect
                debugStrLn "actorThread: sent."
                return ()

           shutdown :: ThreadId -> IO ()
           shutdown actorTID =
               do debugStrLn "shutdown: update isClosed IORef to True."
                  writeIORef isClosed True
                  debugStrLn "shutdown: killing actor thread."
                  killThread actorTID
                  debugStrLn "shutdown: taking ccTMV."
                  (cc, listenQueue, listenerTID) <- atomically $ takeTMVar ccTMV -- FIXME: or should this by tryTakeTMVar
                  debugStrLn "shutdown: killing listener thread."
                  killThread listenerTID
                  debugStrLn "shutdown: expiring listen queue."
                  expireQueue  listenQueue
                  debugStrLn "shutdown: closing connection."
                  ccClose cc
                  return ()

       cc <- reconnect
       listenQueue <- atomically $ newTQueue

       actorTID    <- forkIO $ actorThread
       listenerTID <- forkIO $ listener cc listenQueue

       atomically $ putTMVar ccTMV (cc, listenQueue, listenerTID)

       return (toAcidState $ RemoteState actor (shutdown actorTID))
