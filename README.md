# Dhall flycheck plugin

Dhall support for emacs flycheck mode.

## Setup

Right now there is not much in the way of setup.

`dhall.el` contains the definition of a flycheck plugin. Add it to
your emacs config (e.g. by literally pasting it in, I have no clue
about how emacs packaging works yet).

You need the `dhall-flycheck` exeutable, which takes a source file
and outputs a json that can be understood by the flycheck parser.
That’s what the cabal project is for. You can install it via the
normal `cabal install` mechanism, or by using the `overlay.nix`
file in this repo and adding it to your nix config:

```
  dhall-flycheck =
    (import "${pkgs.fetchFromGitHub {
      owner = "Profpatsch";
      repo = "dhall-flycheck";
      rev = "<commit id you want>";
      sha256 = "<sha that nix tells you it expects>";
    }}/overlay.nix" pkgs pkgs).dhall-flycheck;
```

You can also use that file as a normal nixpkgs overlay.

## Why not LSP?

There [might be LSP support in the
making](https://github.com/dhall-lang/dhall-lang/issues/312#issuecomment-456871069),
but it’s more work and I figured adding support to flycheck must be an
easier task and usable with a small amount of work. It was so.

## TODO & Known Bugs

- The dhall import mechanism doesn’t return any source code position
  when an import fails. We can add support manually, see
  https://github.com/dhall-lang/dhall-haskell/issues/561#issuecomment-460085549
- Errors that appear at the ends of lines are not reachable by the
  cursor
- Only the beginning of source spans is used. flycheck is getting
  support for that, which is currently blocked on a non-issue sadly.
  See https://github.com/flycheck/flycheck/pull/1400

## Development

```
$ nix-shell
$ cabal build
```

## License

BSD 3-clause, same as dhall-haskell.
