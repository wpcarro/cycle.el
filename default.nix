{ depot, ... }:

depot.tools.emacs-pkgs.buildEmacsPackage {
  pname = "cycle";
  version = "1.0.0";
  src = ./cycle.el;
  externalRequires =
    epkgs: with epkgs;
    [
      dash
    ];
  internalRequires =
    (with depot.users.wpcarro.emacs.pkgs; [
      struct
    ]);

  doInstallCheck = true;
  installCheckPhase = ''
    emacs -batch \
      -f package-activate-all \
      -l ert -l ${./tests.el} -f ert-run-tests-batch-and-exit
  '';
  meta.ci.extraSteps.github = depot.tools.releases.filteredGitPush {
    filter = ":/users/wpcarro/emacs/pkgs/cycle";
    remote = "git@github.com:wpcarro/cycle.el.git";
    ref = "refs/heads/canon";
  };
}
