{ pkgs, ... }:
{
  languages.haskell.enable = true;
  pre-commit.hooks.fourmolu.enable = true;
  pre-commit.hooks.hlint.enable = true;
  packages = with pkgs; [
    pcre
  ];
  languages.c.enable = true;
}
