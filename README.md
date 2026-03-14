# dotfiles

There are submodules in this repository, when installing these dotfiles, fetch the submodules as well:

```
git submodule update --init --recursive
```

- AMD vid for undervolt (Default) -- 53

- Local Audio for Yomichan Audio setup

Exists in `~/.local/share/local-audio-yomichan`. Runs in venv `~/.local/share/venvs/localaudio/`.

Offline files are stored in `~/.local/share/local-audio-yomichan/plugin/user_files/` which are symlinked into the Anki addon's
installation and back-linked in `~/.local/share/local-audio-yomichan/` as well.

If for some reason the server refuses to launch, consider upgrading the venv

```bash
$ source <localaudio vevn>/bin/activate
$ python3 -m venv --upgrade <localaudio venv path>
```
