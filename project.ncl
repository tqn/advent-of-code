let inputs = import "./nickel.lock.ncl" in
let organist = inputs.organist in

{
  shells = organist.shells.Bash,

  shells.build = {
    packages = {},
  },

  shells.dev = {
    packages = {
      guile = organist.import_nix "nixpkgs#guile",
      # emacs = organist.import_nix "nixpkgs#emacs",
    }
    # packages.hello = organist.import_nix "nixpkgs#hello",
    # packages.direnv = organist.import_nix "nixpkgs#direnv"
  },
}
  | (
    organist.OrganistExpression
    & organist.tools.direnv.Schema
  )
