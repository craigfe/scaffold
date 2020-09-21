(lang dune 2.7.1)
(name scaffold)
(cram enable)

(generate_opam_files true)
(source (github CraigFe/scaffold))
(maintainers "Craig Ferguson <me@craigfe.io>")
(authors "Craig Ferguson <me@craigfe.io>")

(package
 (name scaffold)
 (synopsis "Lightweight framework for propping up test frameworks")
 (documentation https://CraigFe.github.io/scaffold/)
 (depends
  fmt
  cmdliner
  ppxlib))
