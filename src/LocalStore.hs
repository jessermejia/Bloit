module LocalStore where

import qualified Data.IntMap.Lazy as IntMap
import           Routine
import           Story
import           Text.Printf
import           Type
import           Utility

data T = T {  locals            :: IntMap.IntMap Int
            , count             :: Int
            , argumentsSupplied :: Int
           }

empty :: LocalStore.T
empty =
  LocalStore.T { locals = IntMap.empty, count = 0, argumentsSupplied = 0 }

writeLocal :: LocalStore.T -> LocalVariable -> Int -> LocalStore.T
writeLocal localStore (Local local) value =
  let value' = unsignedWord value
      locals' = IntMap.insert local value' (locals localStore) in
  localStore { locals = locals' }

readLocal :: LocalStore.T -> LocalVariable -> Maybe Int
readLocal localStore (Local local) =
  IntMap.lookup local (locals localStore)

add :: LocalStore.T -> LocalVariable -> Int -> LocalStore.T
add localStore (Local n) defaultValue =
  let locals' = IntMap.insert n defaultValue (locals localStore)
      count' = max (count localStore) n in
  localStore { locals = locals', count = count' }

createDefaultLocals :: Story.T -> RoutineAddress -> LocalStore.T
createDefaultLocals story routineAddress =
  aux empty 1
  where
    count = Routine.localsCount story routineAddress
    aux acc i =
      if i > count then
        acc
      else
        let defaultValue = Routine.localDefaultValue story routineAddress i
            newStore = add acc (Local i) defaultValue in
      aux newStore (i + 1)

-- BUG?: determine whether this implementation is buggy.
-- eric lippert wrote: Note the use of IntMap.fold, which is the logical equivalent of List.fold_left.
-- but i could only get this to compile with foldrWithKey. However, my output is reversed!
-- so maybe i have this wrong...i worked around this by concatenating the acc on the right...
display :: LocalStore.T -> String
display localStore =
  IntMap.foldrWithKey folder "" locals'
  where
    toString local value =
      printf "local%01x=%04x " (local - 1) value
    folder local value acc =
      -- acc ++ toString local value
      toString local value ++ acc
    locals' = locals localStore

-- original ocaml
-- let display local_store =
--   let to_string local value =
--     Printf.sprintf "local%01x=%04x " (local - 1) value in
--   let folder local value acc =
--     acc ^ (to_string local value) in
--   let locals = local_store.locals in
--   IntMap.fold folder locals ""
