open Ocamlbuild_plugin;;
open Command;;

dispatch begin function
| After_rules ->
    ocaml_lib ~extern:true ~dir:"/home/anocka/godi/lib/ocaml/pkg-lib/extlib" "extLib";
    ocaml_lib ~extern:true ~dir:"/home/anocka/progs/ocaml/mlgrace-0.1.0"
"grace";
| _ -> ()
end;;