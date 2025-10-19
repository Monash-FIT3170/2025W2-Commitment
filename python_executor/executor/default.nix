{
  fhs ? throw "You need to provide the sandbox fhs to the python-executor"
, lib
, python3Packages
, makeWrapper
, ...
}:

let
  # Any packages with binaries that need to be available for running by subprocess.run need to be added here:
  pkgsToAddToPath = [
    fhs
  ];
in
  python3Packages.buildPythonApplication rec {
    pname = "python-executor";
    version = "1.0";

    nativeBuildInputs = [ makeWrapper ];
    propagatedBuildInputs = [
      python3Packages.flask
      fhs
    ];

    pyproject = true;
    build-system = [ python3Packages.setuptools ];

    src = ./.;

    # Add fhs to PATH
    postInstall = ''
      wrapProgram $out/bin/python_executor.py \
        --prefix PATH : ${lib.makeBinPath pkgsToAddToPath}

      # And for convenience, link the executor fhs to the result/bin/
      ln -s ${fhs}/bin/* $out/bin/
    '';
  }
