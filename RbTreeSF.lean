/- UserJY port of Red-black trees from Software Foundations verified Functional Algorithms
-/

namespace softwarefoundation
universe u

inductive rbcolor : Type
 | Red : rbcolor 
 | Black : rbcolor
  deriving Repr


inductive rbTree (T: Type u): Type u
 | leaf : rbTree T 
 | node : rbcolor -> rbTree T -> Nat -> T -> rbTree T -> rbTree T
  deriving Repr


--will try to find the value using the key given `x: Nat`. 
--If it can't be found, it will give the default value `d: T`
def rbTree.lookup {T: Type u} (d: T) (x: Nat) (t: rbTree T) : T := 
  match t with 
  | leaf => d 
  | node _ tl k v tr => if x < k then lookup d x tl 
                        else if k < x then lookup d x tr
                        else v

def rbTree.balance {T : Type u} (c: rbcolor) (t1: rbTree T) (k : Nat) (vk : T) (t2: rbTree T) :=
  match c with 
  | rbcolor.Red => node rbcolor.Red t1 k vk t2
  | _  => match t1 with
          | node rbcolor.Red (node rbcolor.Red a x vx b) y vy c => 
              node rbcolor.Red (node rbcolor.Black a x vx b) y vy (node rbcolor.Black c k vk t2)
          | node rbcolor.Red a x vx (node rbcolor.Red b y vy c) => 
              node rbcolor.Red (node rbcolor.Black a x vx b) y vy (node rbcolor.Black c k vk t2)
          | _ => match t2 with 
                 | node rbcolor.Red (node rbcolor.Red b y vy c) z  vz d => 
                    node rbcolor.Red (node rbcolor.Black t1 k vk b) y vy (node rbcolor.Black c z vz d)
                 | node rbcolor.Red b y vy (node rbcolor.Red c z vz d) => 
                    node rbcolor.Red (node rbcolor.Black t1 k vk b) y vy (node rbcolor.Black c z vz d)
                 | _ => node rbcolor.Black t1 k vk t2

def rbTree.ins {T: Type u} (x : Nat) (vx : T) (t : rbTree T) : rbTree T :=
  match t with 
  | leaf => node rbcolor.Red leaf x vx leaf
  | node c a y vy b => if x < y then 
                          balance c (ins x vx a) y vy b
                       else if x > y then 
                          balance c a y vy (ins x vx b)
                       else 
                          node c a x vx b

def rbTree.make_black {T : Type u} (t : rbTree T) : rbTree T :=
  match t with 
  | leaf => leaf 
  | node _ a x vx b => node rbcolor.Black a x vx b

def insert {T: Type u} (x: Nat) (vx : T) (t : rbTree T) :=
  rbTree.make_black (rbTree.ins x vx t)

def rbcolor.toString (t : rbcolor) : String :=
  match t with 
  | rbcolor.Red => "Red"
  | rbcolor.Black => "Black"

def rbTree.toString (tt : rbTree T) : String := 
    match tt with 
    | leaf => ""
    | node c a x vx b => s!"[key:{x.repr} {c.toString} [{a.toString}] [{b.toString}]]" 



end softwarefoundation


open softwarefoundation


#reduce insert 2 "someVal" rbTree.leaf
--rbTree.node (rbcolor.Black) (rbTree.leaf) 2 "someVal" (rbTree.leaf)
--2 is the key that the rbTree will use to rebalance, "someVal" can be anything


#reduce insert 15 15 (insert 30 30 (insert 20 20 (insert 10 10 rbTree.leaf)))

/-
rbTree.node
  (rbcolor.Black)
  (rbTree.node (rbcolor.Black) (rbTree.leaf) 10 10 (rbTree.node (rbcolor.Red) (rbTree.leaf) 15 15 (rbTree.leaf)))
  20
  20
  (rbTree.node (rbcolor.Black) (rbTree.leaf) 30 30 (rbTree.leaf))
-/

#eval rbTree.lookup "notFound" 1 (insert 2 "hello" rbTree.leaf)
#eval rbTree.lookup "notFound" 2 (insert 2 "hello" rbTree.leaf)