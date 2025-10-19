{
  lib
, python3Packages
, ...
}:

python3Packages.buildPythonApplication {
  pname = "python-executor";
  version = "1.0";

  propagatedBuildInputs = [ python3Packages.flask ];

  pyproject = true;
  build-system = [ python3Packages.setuptools ];

  src = ./.;
}
