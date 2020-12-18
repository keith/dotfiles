self: super:
{
  git-zsh-completion = super.stdenvNoCC.mkDerivation {
    pname = "git-zsh-completion";
    version = super.git.version;

    phases = [ "installPhase" ];

    installPhase = ''
      mkdir -p $out/share/zsh/site-functions
      cp ${super.git}/share/git/contrib/completion/git-completion.zsh $out/share/zsh/site-functions/_git

      # Patch the zsh completion script so it can find the Bash completion script.
      sed -i -e "/locations=(/a \${"\t\t"}'${super.git}/share/git/contrib/completion/git-completion.bash'" \
        $out/share/zsh/site-functions/_git
    '';
  };
}
