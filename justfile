alias b := build
alias r := run

build:
  nix build 

run:
  nix run .#gitlab-pipes:exe:gitlab-pipes

show:
  nix flake show
