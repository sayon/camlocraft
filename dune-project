(lang dune 3.0)

(name camlocraft)

(generate_opam_files true)

(source
 (github sayon/camlocraft))

(authors "Igor Zhirkov")

(maintainers "Igor Zhirkov")

(license LICENSE)

(wrapped_executables false)

(package
 (name camlocraft)
 (synopsis "A study of ocaml")
 (description "Minecraft clone in Ocaml")
 (depends ocaml dune)
)
