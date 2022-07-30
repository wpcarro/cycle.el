{ pkgs, depot, ... }:

let
  cycle = pkgs.callPackage
    ({ emacsPackages }:
      emacsPackages.trivialBuild {
        pname = "cycle";
        version = "1.0.0";
        src = ./cycle.el;
        packageRequires =
          (with emacsPackages; [
            dash
          ]) ++
          (with depot.users.wpcarro.emacs.pkgs; [
            list
            maybe
            struct
          ]);
      })
    { };

  emacs = (pkgs.emacsPackagesFor pkgs.emacs28).emacsWithPackages (epkgs: [
    epkgs.dash
    depot.users.wpcarro.emacs.pkgs.maybe
    cycle
  ]);
in
cycle.overrideAttrs (_old: {
  doCheck = true;
  checkPhase = ''
    ${emacs}/bin/emacs -batch \
      -l ert -l ${./tests.el} -f ert-run-tests-batch-and-exit
  '';
})
