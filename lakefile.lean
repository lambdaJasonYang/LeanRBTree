import Lake
open Lake DSL

package rbTreeSF {
  -- add package configuration options here
}

lean_lib RbTreeSF {
  -- add library configuration options here
}

@[default_target]
lean_exe rbTreeSF {
  root := `Main
}
