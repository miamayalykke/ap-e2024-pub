# Week 6 - Concurrency

## Slides and Material

TBA

## Suggested Reading

* [Concurrent Haskell](concurrent-haskell.pdf)

### Going Beyond

* [The three kinds of Haskell exceptions and how to use
  them](https://www.tweag.io/blog/2020-04-16-exceptions-in-haskell/)

## Exercises

In these exercises, and the associated assignment, you will be
implementing the [*Stateful Planning
Committee*](https://en.wikipedia.org/wiki/Gosplan) (SPC), a *job
scheduler* for managing the employment of workers and allocation of
resources. A job is any Haskell computation in the `IO` monad,
evaluated for its side effects. After a job is *enqueued* in SPC, we
can ask for the status of the job. At some point, SPC will decide to
actually execute the job.

We can imagine using a job scheduler such as SPC to enqueue thousands
of jobs for downloading various files from the Internet, with the
scheduler taking care of ensuring only a limited number of jobs run
concurrently, handling timeouts, performing logging, and so on. Our
scheduler will be fairly simple, but will have a lot of the machinery
you also need in a real scheduler.

SPC will take the form of various Haskell threads that communicate
through message passing. The main challenge is that the jobs are
*untrusted*, and may signal errors and go into infinite loops, which
must be handled without any damage to the system as a whole. In
particular, there will be a central *SPC thread* which we must protect
by ensuring evaluation of untrusted code happens in separate threads,
and with appropriate handlers.

There are some limits to the antagonism we will assume of enqueued
work: we cannot cope with work that uses functions from
[System.Exit](https://hackage.haskell.org/package/base-4.20.0.1/docs/System-Exit.html)
to shut down the entire process, or maliciously try to subvert SPC
itself. Since jobs are (for now) arbitrary `IO` actions, there is not
any true security in SPC.

## Creating the Event Loop

You will find a skeletal code handout in [handout/](handout/). The
module `SPC.Core` contains two utility functions that will become
useful later (`getSeconds` and `removeAssoc`). It also contains two
types (`SPCMsg`, `SPC`) and one value definition (`startSPC`). It also
contains a bunch of imports that will become necessary, but adds a lot
of warning clutter - feel free to comment them until you need them.

The `SPC` type is our handle to the SPC, and it is complete - you do
not need to modify it.

The `SPCMsg` type represents messages that will be sent *to* the SPC
thread. We will extend this type with new messages as we go along.

The first thing you must do is to extend `startSPC` such that it
launches a new thread that runs a loop that reads messages from the
created channel and acts on them. To do this:

1. Add a constructor `MsgPing (Chan Int)` to `SPCMsg`. We will remove
   this later, but it is useful for testing that we get the event loop
   right.

2. Modify `startSPC` such that it creates a thread that runs in an
   infinite loop. This thread runs in an infinite loop and reads
   `SPCMsg` messages. When it receives a `MsgPing` message, it should
   send back an integer (your choice) on the channel in the `MsgPing`
   message.

3. Write a function `pingSPC :: SPC -> IO Int` that sends a `MsgPing`
   message to SPC, waits for a response, then returns the integer in
   the response. You must also add `pingSPC` to the module export
   list.

4. Add a test to `SPC.Core_Tests` that exercises `pingSPC`.

### Hints

Use `forever` from `Control.Monad` to write infinite monadic loops.

### Solution

<details>
<summary>Open this to see the implementation</summary>

```Haskell

data SPCMsg = MsgPing (Chan Int)

startSPC :: IO SPC
startSPC = do
  c <- newChan
  _ <- forkIO $ forever $ handle c
  pure $ SPC c
  where
    handle c = do
      msg <- readChan c
      case msg of
        MsgPing reply_chan ->
          writeChan reply_chan 1337

pingSPC :: SPC -> IO Int
pingSPC (SPC c) = do
  reply_chan <- newChan
  writeChan c $ MsgPing reply_chan
  readChan reply_chan

-- And in SPC.Core_Tests:

tests :: TestTree
tests =
  localOption (mkTimeout 3000000) $
    testGroup
      "SPC (core)"
      [ testCase "ping" $ do
          spc <- startSPC
          x <- pingSPC spc
          x @?= 1337
      ]

```

</details>

## A Stateful Monadic Event Loop

Most servers maintain some kind of state that is modified by the
messages they receive. Eventually SPC will have a rather complicated
notion of state, but to start out, we will simply have it track how
many ping messages it has received, and reply to pings with that
number.

To make things interesting (and ultimately, more manageable), we will
maintain the state using a monad. First, add a type repesenting the
SPC state:

```Haskell
data SPCState = SPCState
  { spcPingCounter :: Int
  }
```

Then we define an SPC monad that is a state monad with a state of type
`SPCState`, but which also supports `IO`:

```Haskell
newtype SPCM a = SPCM (SPCState -> IO (a, SPCState))
```

* Implement the `Functor`, `Applicative`, and `Monad` instances for
  `SPCM`.

<details>
<summary>Open this to see the implementation</summary>

```Haskell
instance Functor SPCM where
  fmap = liftM

instance Applicative SPCM where
  pure x = SPCM $ \state -> pure (x, state)
  (<*>) = ap

instance Monad SPCM where
  SPCM m >>= f = SPCM $ \state -> do
    (x, state') <- m state
    let SPCM f' = f x
    f' state'
```

</details>

We will also need the usual utility functions for managing the state:

```Haskell
get :: SPCM SPCState
put :: SPCState -> SPCM ()
```

* Implement `get` and `put`.

<details>
<summary>Open this to see the implementation</summary>

```Haskell
-- | Retrieve the state.
get :: SPCM SPCState
get = SPCM $ \state -> pure (state, state)

-- | Overwrite the state.
put :: SPCState -> SPCM ()
put state = SPCM $ \_ -> pure ((), state)
```

</details>

And we also need a function for lifting an arbitrary IO action into
`SPCM`:

```Haskell
io :: IO a -> SPCM a
```

* Implement `io`.

<details>
<summary>Open this to see the implementation</summary>

```Haskell
io m = SPCM $ \state -> do
  x <- m
  pure (x, state)
```

</details>

Finally, we need a function for running an `SPCM` operation, which
necessarily must be done in the `IO` monad:

```Haskell
runSPCM :: SPCState -> SPCM a -> IO a
```

* Implement `runSPCM`.

<details>
<summary>Open this to see the implementation</summary>

```Haskell
runSPCM state (SPCM f) = fst <$> f state
```

</details>

Now we are ready to use `SPCM`.

* Modify `startSPC` such that the loop runs inside the `SPCM` monad.

* Modify the handling of `MsgPing` such that it replies with the
  `spcPingCounter` field from the state, and then increments the field
  by one.

* Move the handling of a message into a separate function with
  signature `handleMsg :: Chan SPCMsg -> SPCM ()`.

* Add an appropriate test.

### Hints

* To read a message from a channel `c` inside `SPCM`: `io $ readChan c`.

* `succ x` produces `x+1`.

### Solution

<details>
<summary>Open this to see the implementation</summary>

```Haskell
handleMsg :: Chan SPCMsg -> SPCM ()
handleMsg c = do
  msg <- io $ readChan c
  case msg of
    MsgPing reply_chan -> do
      state <- get
      io $ writeChan reply_chan $ spcPingCounter state
      put $ state {spcPingCounter = succ $ spcPingCounter state}

startSPC :: IO SPC
startSPC = do
  c <- newChan
  let initial_state =
        SPCState
          { spcPingCounter = 0
          }
  _ <- forkIO $ runSPCM initial_state $ forever $ handle c
  pure $ SPC c

-- And a test:

tests :: TestTree
tests =
  localOption (mkTimeout 3000000) $
    testGroup
      "SPC (core)"
      [ testCase "ping" $ do
          spc <- startSPC
          x <- pingSPC spc
          x @?= 0
          y <- pingSPC spc
          y @?= 1
          z <- pingSPC spc
          z @?= 2
      ]
```

</details>

## Adding Jobs

We have now constructed the skeleton of a stateful message passing
server. Essentially all servers can start out this way, although the
monadic approach to state handling may be overkill for very simple
ones. We will now extend this skeleton with the actual job handling
functionality that is the purpose of our server. Feel free to remove
the `MsgPing` message and the related functions once you add more
interesting messages.

First we will implement a way to add jobs. Jobs are described by this
type:

```Haskell
-- | A job that is to be enqueued in the glorious SPC.
data Job = Job
  { -- | The IO action that comprises the actual action of the job.
    jobAction :: IO (),
    -- | The maximum allowed runtime of the job, counting from when
    -- the job begins executing (not when it is enqueued).
    jobMaxSeconds :: Int
  }
```

And are added by this function:

```
-- | A unique identifier of a job that has been enqueued.
newtype JobId = JobId Int
  deriving (Eq, Ord, Show)

-- | Add a job for scheduling.
jobAdd :: SPC -> Job -> IO JobId
```

The `Job` type (and its definition) should be exported, but the
`JobId` should not have its constructor exported. It must be an
abstract type from the point of view of users. This is achieved by
putting respectively `Job(..)` and `JobId` in the module export list.

When a job is enqueued, it is not executed immediately. Rather, it is
added to a list of *pending* jobs. Each job is associated with a
unique `JobId`, which is used to reference it later.

* Extend `SPCState` with a field of type `[(JobId,Job)]` which tracks
  pending jobs, and a field of type `JobId` which contains the next
  available job identifier. Update other parts of the code as
  necessary.

* Add a synchronous message to `SPCMsg` for adding a new job. You must
  decide what payload this message should contain, but it must at
  least provide a channel for the reply, which is a `JobId`. When
  handling this message, you must add the job to the list that you
  added to the state.

* Implement `jobAdd` by using the message you added above.

* Add an appropriate test.

### Solution

<details>
<summary>Open this to see the implementation</summary>

```Haskell
data SPCState = SPCState
  { spcJobsPending :: [(JobId, Job)],
    spcJobCounter :: JobId
  }

handleMsg :: Chan SPCMsg -> SPCM ()
handleMsg c = do
  msg <- io $ readChan c
  case msg of
    MsgJobAdd job reply -> do
      state <- get
      let JobId jobid = spcJobCounter state
      put $
        state
          { spcJobsPending =
              (spcJobCounter state, job) : spcJobsPending state,
            spcJobCounter = JobId $ succ jobid
          }
      io $ writeChan reply $ JobId jobid

startSPC :: IO SPC
startSPC = do
  c <- newChan
  let initial_state =
        SPCState
          { spcJobCounter = JobId 0,
            spcJobsPending = []
          }
  _ <- forkIO $ runSPCM initial_state $ forever $ handleMsg c
  pure $ SPC c

-- | Add a job for scheduling.
jobAdd :: SPC -> Job -> IO JobId
jobAdd (SPC c) job = do
  reply_chan <- newChan
  writeChan c $ MsgJobAdd job reply_chan
  readChan reply_chan

-- And a test:

tests :: TestTree
tests =
  localOption (mkTimeout 3000000) $
    testGroup
      "SPC (core)"
      [ testCase "adding job" $ do
          spc <- startSPC
          _ <- jobAdd spc $ Job (pure ()) 1
          pure ()
      ]

```

</details>

## Job Status

We will now add functionality for querying the status of a job. The
status of the job is expressed by the following two types:

```Haskell
-- | How a job finished.
data JobDoneReason
  = -- | Normal termination.
    Done
  | -- | The job was killed because it ran for too long.
    DoneTimeout
  | -- | The job was explicitly cancelled.
    DoneCancelled
  | -- | The job crashed due to an exception.
    DoneCrashed
  deriving (Eq, Ord, Show)

-- | The status of a job.
data JobStatus
  = -- | The job is done and this is why.
    JobDone JobDoneReason
  | -- | The job is still running.
    JobRunning
  | -- | The job is enqueued, but is waiting for an idle worker.
    JobPending
  | -- | A job with this ID is not known to this SPC instance.
    JobUnknown
  deriving (Eq, Ord, Show)
```

* Add these type definitions to `SPC.Core` and also add them to the
  module export list.

Initially a job has status `JobPending`. In fact, since we have yet to
implement actual execution of jobs, that is the only constructor we
will make use of.

Your task is now to implement the function with the following
signature:

```Haskell
-- | Query the job status.
jobStatus :: SPC -> JobId -> IO JobStatus
```

* Add a constructor to `SPCMsg` for requesting the status of a job.

* Handle the new message in `handleMsg`. If the provided `JobId` is
  found in the list of pending jobs, then the response should be
  `JobPending`, and otherwise `JobUnknown`.

* Implement the `jobStatus` function and add it to the module export
  list.

* Add an appropriate test, or modify an existing one to make use of
  `jobStatus`.

### Solution

<details>
<summary>Open this to see the implementation</summary>

```Haskell

data SPCMsg
  = ...
  | -- | Immediately reply the status of the job.
    MsgJobStatus JobId (Chan JobStatus)

handleMsg :: Chan SPCMsg -> SPCM ()
handleMsg c = do
  msg <- io $ readChan c
  case msg of
    ...
    MsgJobStatus jobid reply -> do
      state <- get
      io $ writeChan reply $ case lookup jobid $ spcJobsPending state of
        Just _ -> JobPending
        _ -> JobUnknown

-- | Add a job for scheduling.
jobAdd :: SPC -> Job -> IO JobId
jobAdd (SPC c) job = do
  reply_chan <- newChan
  writeChan c $ MsgJobAdd job reply_chan
  readChan reply_chan

-- And a test

tests :: TestTree
tests =
  localOption (mkTimeout 3000000) $
    testGroup
      "SPC (core)"
      [ testCase "adding job" $ do
          spc <- startSPC
          j <- jobAdd spc $ Job (pure ()) 1
          r <- jobStatus spc j
          r @?= JobPending
      ]


```

</details>
