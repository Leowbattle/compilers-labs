/-
  Direct port of `State.hs`.

  `St st a` is the pure state monad `st -> (a, st)`, and `StateIO st a` is
  its `IO`-carrying counterpart `st -> IO (a, st)`.

  These are deliberate 1:1 ports. Lean's built-in `StateM σ α` and
  `StateT σ IO α` are definitionally the same types, so a later (idiomatic)
  pass will replace this module with those, `get`/`set`/`modify`, and
  `MonadState`/`MonadLift`. The Haskell-shaped names are kept for now.
-/

def St (σ α : Type) : Type := σ → α × σ

def app {σ α : Type} (st : St σ α) (x : σ) : α × σ := st x

instance {σ : Type} : Functor (St σ) where
  map g st := fun s => let (x, s') := st s; (g x, s')

instance {σ : Type} : Monad (St σ) where
  pure x := fun s => (x, s)
  bind st f := fun s => let (x, s') := st s; f x s'

def stUpdate {σ : Type} (st1 : σ) : St σ Unit := fun _ => ((), st1)

def stState {σ : Type} : St σ σ := fun st => (st, st)

def StateIO (σ α : Type) : Type := σ → IO (α × σ)

def appT {σ α : Type} (st : StateIO σ α) (x : σ) : IO (α × σ) := st x

instance {σ : Type} : Functor (StateIO σ) where
  map g st := fun s => do
    let (x, s') ← appT st s
    return (g x, s')

instance {σ : Type} : Monad (StateIO σ) where
  pure x := fun s => pure (x, s)
  bind st f := fun s => do
    let (x, s') ← appT st s
    appT (f x) s'

def stUpdateT {σ : Type} (st1 : σ) : StateIO σ Unit := fun _ => return ((), st1)

def stStateT {σ : Type} : StateIO σ σ := fun st => return (st, st)

def lift {σ α : Type} (mx : IO α) : StateIO σ α := fun s => do
  let x ← mx
  return (x, s)
