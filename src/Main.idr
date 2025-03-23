module Main

import IO.Async.File
import IO.Async.Loop.Posix
import IO.Async.Loop.Poller
import IO.Async.Loop.Epoll
import IO.Async.Channel
import IO.Async.Posix
import IO.Async.Signal
import System
import System.File.Error
import System.Posix.File


%default total


countSeconds : Nat -> Channel String -> Async Poll es ()
countSeconds 0 chan = do
  _ <- send chan "Timer Expired"
  close chan
countSeconds (S k) chan = do
  Sent <- send chan "\{show $ S k} s left" | _ => pure ()
  sleep 1.s
  countSeconds k chan

covering
stdin : Has Errno es => Channel String -> Async Poll es ()
stdin chan = do
  bytes <- readnb Stdin ByteString 16
  Sent <- send chan $ show bytes | _ => pure ()
  stdin chan

covering
mainloop : Channel String -> Async Poll es ()
mainloop chan = do
  Just msg <- receive chan | _ => pure ()
  stdoutLn $ show msg
  mainloop chan

export covering
main : IO ()
main = epollApp $ handle [\e => stderrLn "Error \{e}"] run
where
  run : Async Poll [Errno] ()
  run = use1 rawMode $ \_ => do
    chan <- channel 1
    race () [
      countSeconds 10 chan,
      stdin chan,
      -- close chanel when the timer expires.
      -- mainloop is made uncancelable, so that all remaining events
      -- are guaranteed to be processed.
      guarantee (mainloop chan) (close chan >> mainloop chan)
    ]
