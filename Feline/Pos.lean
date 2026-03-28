inductive Pos : Type where
  | one : Pos
  | succ : Pos → Pos

instance : Add Pos where
  add := let rec posPlus : Pos → Pos → Pos
    | Pos.one, k => Pos.succ k
    | Pos.succ n, k => Pos.succ (posPlus n k)
    posPlus

instance : HAdd Nat Pos Pos where
  hAdd := let rec addNatPos : Nat → Pos → Pos
    | 0, p => p
    | n + 1, p => Pos.succ (addNatPos n p)
    addNatPos

instance : HAdd Pos Nat Pos where
  hAdd := let rec addPosNat : Pos → Nat → Pos
    | p, 0 => p
    | p, n + 1 => Pos.succ (addPosNat p n)
    addPosNat

instance : Hashable Pos where
  hash := let rec hashPos : Pos → UInt64
    | Pos.one => 0
    | Pos.succ n => mixHash 1 (hashPos n)
    hashPos

  instance : OfNat Pos (n + 1) where
  ofNat := let rec natPlusOne : Nat → Pos
    | 0 => Pos.one
    | k + 1 => Pos.succ (natPlusOne k)
    natPlusOne n

def Pos.toNat : Pos → Nat
  | Pos.one => 1
  | Pos.succ n => n.toNat + 1

instance : ToString Pos where
  toString x := toString (x.toNat)

def posToString (atTop : Bool) (p : Pos) : String :=
  let paren s := if atTop then s else "(" ++ s ++ ")"
  match p with
  | Pos.one => "Pos.one"
  | Pos.succ n => paren s!"Pos.succ {posToString false n}"

instance : Mul Pos where
  mul := let rec posMul : Pos → Pos → Pos
    | Pos.one, k => k
    | Pos.succ n, k => posMul n k + k
    posMul

def seven : Pos := Pos.succ (Pos.succ (Pos.succ (Pos.succ (Pos.succ (Pos.succ Pos.one)))))
#eval s!"There are {seven}"
#eval (3 : Pos) + (5 : Nat)
#eval hash (3 : Pos)
