self: super:

{
  diff-grep = super.rustPlatform.buildRustPackage rec {
    pname = "diff-grep";
    version = "0.2.0";

    src = super.fetchFromGitHub {
      owner = "keith";
      repo = pname;
      rev = version;
      sha256 = "sha256-DpQRzhbdvlLEuzCC8ntM6QLr0KstciuIyA1cRadTfYk=";
    };

    cargoHash = "sha256-umNtGdzsJP7ogns9noBMPD3xoM6Qpek4JHCl8rOSN44=";
    meta = with super.lib; {
      description = "Filter matching hunks in diffs";
      homepage = "https://github.com/keith/diff-grep";
      license = licenses.mit;
    };
  };
}
