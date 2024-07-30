{ pkgs }: {
    deps = [
      pkgs.emacs-nox
      pkgs.tree
        pkgs.guile_3_0
    ];
}