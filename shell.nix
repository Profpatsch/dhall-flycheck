let
  pkgs = import <nixpkgs> {};
  overlay = import ./overlay.nix pkgs pkgs;

in overlay.dhall-flycheck.env
