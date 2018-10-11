![Release tag](https://img.shields.io/github/tag/UndeadKernel/pacfiles-mode.svg?label=release)
[![MIT license](https://img.shields.io/github/license/UndeadKernel/pacfiles-mode.svg)](https://github.com/UndeadKernel/pacfiles-mode/blob/master/LICENSE)

# pacfiles-mode

Emacs major mode to manage `.pacnew` and `.pacsave` files left by Arch's pacman.
To merge files, *pacfiles-mode* automatically creates an Ediff merge session
that a user can interact with. After finishing the Ediff merge session,
*pacfiles-mode* cleans up the mess that Ediff leaves behind. *pacfiles-mode*
also takes care of keeping the correct permissions of merged files, and
requests passwords (with TRAMP) to act as root when needed.

*pacfiles-mode* is free software, licensed under the MIT License.

![pacfiles-mode main interface](/../screenshots/main_ui.png "Main user interface")

## Installation and Setup
*pacfiles-mode* can only be installed manually as of now.

### Manual installation
To manually install *pacfiles-mode*, clone this repository into `.emacs.d`:

```shell
$ cd ~/.emacs.d
$ git clone https://github.com/UndeadKernel/pacfiles-mode.git
```

Then add the cloned folder to your `load-path` and require *pacfiles-mode* by
adding the following to `~/.emacs.d/init.el`:

``` emacs-lisp
(add-to-list 'load-path "~/.emacs.d/pacfiles-mode")
(require 'pacfiles-mode)
```
<!-- ### With use package -->
<!-- Add the following to your configuration file: -->
<!--     (use-package ) -->

## Overview
Start *pacfiles-mode* with the command `pacfiles`. Choose a file to merge by
clicking (or pressing `RET`) on `[merge]`. An Ediff session will start; do not
change the name of the Ediff buffers. After finishing the merging process, save
the merged file (e.g., in Ediff's command buffer, use keybindings `w c`) without
changing its name or location and quit Ediff. To apply the merge file to the
file system, click (or press `RET`) on `[apply]`. Quit *pacfiles-mode* by
pressing `q`.

## Main interface

*pacfiles-mode* searches and lists all `.pacnew` and `.pacsave` update files
found in `/etc` (by default). These update files can be discarded or, with the
help of [Ediff](https://www.gnu.org/software/emacs/manual/html_node/ediff/
"Ediff's manual"), compared or merged. Ediff is automatically setup for the
user; permissions are also taken care for.

### Pending files
`.pacnew` or `.pacsave` file that have not been merged
by the user are shown under the **pending** header. Three actions are
available on these files

![Pending Actions](/../screenshots/pending_actions.png "Pending Actions")
* `[merge]`: start an Ediff session to merge the update
* `[diff]`: compare the update with its base file
* `[delete]`: delete the update and keep the base file as is

If the `[diff]` button is not available, there is no base file to compare with.
In this case, `[merge]` will treat the update, as is, as the file to merge.

### Merged files
`.pacnew` and `.pacsave` files that have been merged, but not applied into the
file system, are shown under the **merged** header. Three actions are
available on these files:

![Merged Actions](/../screenshots/merge_actions.png "Merged Actions")
* `[apply]`: copy the merged file into the file system and delete the update
  file
* `[view]`: show the merge of the update and its base file
* `[discard]`: delete the merge file, keep the update file and the base file
  intact

### Apply or discard all changes
![Apply/Discard All Actions](/../screenshots/buttons_all.png "Apply/Discard All
Actions")

The `[Apply All]` and `[Discard All]` buttons do what you would expect them to do.

## Ediff tricks and tips
When merging an update, Ediff is setup as this:

![Ediff interface](/../screenshots/ediff_ui.png "Ediff interface")

Ediff's control buffer (on the bottom) will be focused and ready to receive user
commands. The following key bindings are most useful:

* `n` / `p`: go to the next / previous place where there is a difference
* `a`: place in the merge buffer the contents highlighted in the top left window
  (always the update file)
* `b`: place in the merge buffer the contents highlighted in the top right
  window (always the base file to update)
* `w c`: save the contents of the merge buffer
* `q`: quit Ediff
* `?`: show Ediff's help

When saving the merge buffer, do not change the merge buffer's location or name.
You can switch to the merge buffer, buffer **C** in Ediff, and modify it as any
other buffer.


## Functions and Variables

### Commands
* `pacfiles` or `pacfiles/start`: start *pacfiles-mode*.
* `pacfiles/quit`: quit *pacfiles-mode*.
* `pacfiles/revert-buffer`: reload the list of update files

### Configuration variables
* `pacfiles-updates-search-command`: command used to search for `.pacnew` and
  `.pasave` files.
* `pacfiles-activate-no-confirm`: if `t`, do not ask for user input when
  applying or discarding a merged file.
* `pacfiles-merge-file-tmp-location`: location where temporary merge files are
  stored.

### Keybindings
* `n` and `p`: move forward or backwards to the next button.
* `C-c C-n` and `C-c C-p`: move to the next and previous section headers.
* `g` or `r`: refresh the list of files.
* `TAB`: toggle hiding or showing headers

## What's happening in the back
When merging a `.pacnew` or `.pacsave` update with its corresponding base file,
*pacfiles-mode* computes a hash of the base file. The hash is used to name a
temporary merge file with the format`<hash>.pacmerge`. If *pacfiles-mode* finds
a `.pacmerge` file whose name matches the hash of a base file, the `.pacmerge`
file is associated to the base file as its merge. When applying a merge, this
temporary `.pacmerge` file replaces the base file and the corresponding update
file is deleted.
