To use these dotfiles store them somewhere (like dropbox) and symlink the desired files to your home directory.
Example: `ln -s ~/.zshrc ~/Dropbox/dotfiles/zshrc`

Automate this process by running the bash or ruby scripts. The bash script exists for new systems without ruby yet.

```
./manage.sh install
```

You can also remove the symlinks with:

```
./manage.sh remove
```

