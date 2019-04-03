# Installing

1. Install emacs and emacs-mozc-ut2 with package manager.
2. Install a2ps for org-checklist
3. Install [cmigemo](https://github.com/koron/cmigemo/) (Could also try installing with package manager)
2. Clone Repo
3. Open emacs and watch it install everything itself
   1. If it gives warning about custom.el not existing, restart emacs
   2. Select utf-8-unix for migemo
4. Delete directory and restart if installation hangs
5. May need to restart for org-bullets to work
6. Start with systemd

``` shell
systemctl enable --user emacs
systemctl start --user emacs

```

Start with emacsclient -c
