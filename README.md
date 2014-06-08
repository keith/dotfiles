# dotfiles

Automate the install by running the bash script

```sh
./manage.sh install
```

You can also remove the symlinks with:

```sh
./manage.sh remove
```

There are also tons of other things you can do:

Install each of the language scripts:

```sh
./install-langs.sh
```

Or checkout the scripts in `langs` and install them individually.

### OS X

- Bootstrap OS X by running `mac-install.sh` in `osx`
- Run `loginfix.sh` to disable all resume features of OS X
- Run `launchd.sh` to symlink the launchd plists
- Run `defaults.sh` to change tons of default settings
- Run `langs/rbenv.sh` to setup rbenv
