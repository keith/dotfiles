self: super:

{
  dylibtree = super.rustPlatform.buildRustPackage rec {
    pname = "dylibtree";
    version = "0.1.0";

    src = super.fetchFromGitHub {
      owner = "keith";
      repo = pname;
      rev = version;
      sha256 = "sha256-XRayHTAEh3mXouOr72Gi5h7QNdJUbY+6K5PydWP2olc=";
    };

    cargoHash = "sha256-91ecGchpvp23snyTP1a+c+ZeTDSKX/bkE/u5r5eU7ck=";
    meta = with super.lib; {
      description =
        "Inspect dynamic dependencies of Mach-O binaries recursively";
      homepage = "https://github.com/keith/dylibtree";
      license = licenses.mit;
    };
  };
}
