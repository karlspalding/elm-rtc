{
  inputs = {
    nixpkgs = {
      url = "git+ssh://deploy@repos/srv/repos/nixpkgs.git?ref=swallsoft";
    };
  };
  outputs = { self, nixpkgs }:
    with import nixpkgs {
      system = "x86_64-linux";
    };
    {
      devShell.x86_64-linux =
        mkShell {
          buildInputs = [
            entr
            git
            nodejs-16_x
          ];
        };

        shellHook = ''
          export NPM_CONFIG_CACHE="''${TMP}/npm"
        '';
    };
}
