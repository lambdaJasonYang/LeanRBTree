import RbTreeSF

open softwarefoundation

def main : IO Unit :=
  let sampleTree := insert 2 "someVal" rbTree.leaf
  IO.println s!"Hello, {sampleTree.toString}!"


#eval IO.println s!"{(insert 2 "someVal" rbTree.leaf).toString}"

#eval IO.println s!"{(insert 15 15 (insert 30 30 (insert 20 20 (insert 10 10 rbTree.leaf)))).toString}"

/-
[key:20 Black 
  [[key:10 Black [] 
                 [[key:15 Red [] []]]]] 
  [[key:30 Black [] []]]
]
-/