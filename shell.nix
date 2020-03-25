let

  sources = import ./nix/sources.nix;
  pkgs = import <nixpkgs> { };
  all-hies = import sources.all-hies { };

in (pkgs.mkShell {
  buildInputs = [

    pkgs.stack
    pkgs.ghc
    pkgs.ormolu
    (all-hies.selection { selector = p: { inherit (p) ghc882; }; })
    pkgs.postgresql

  ];
})
