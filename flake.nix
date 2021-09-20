{
  description = "My elisp library.";


  inputs = {
    nixpkgs.url = "github:theosherry/nixpkgs/theo-working";

    flake-utils.url = "github:numtide/flake-utils?rev=98c8d36b1828009b20f12544214683c7489935a1";
  };


  outputs = { self, nixpkgs, flake-utils }:
  flake-utils.lib.simpleFlake {
    inherit self nixpkgs;
    name = "theo-elisplib";
    overlay = final: prev: {
      theo-elisplib = {
        theo-elisplib = with final; (emacs27Packages.melpaBuild {
          pname   = "my-libraries";
          ename   = "my-libraries";
          version = "0.10";
          recipe  = builtins.toFile "recipe" ''
            (my-libraries :fetcher github
            :repo "theosherry/elisplib"
            :files (:defaults "*.winstate"))
          '';

          src = self;
        });
      };
    };
  };
}

