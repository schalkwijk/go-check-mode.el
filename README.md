## Disclaimer
This is my first attempt at any elisp whatsoever, so there are bound to be kinks in the system. If you discover an issue, feel free to email me at <samlopes89@gmail.com> or, better yet, fork the repo, perform the fix, and submit a pull request (bonus points for topic branches!).

## Installation

First, you'll have to install this package's dependencies. `go-check-mode` relies on:

1. __go-mode__: `go-mode` should be installed and working properly for all go-related files. `go-mode` should come with go itself, but you can also fetch an improved version [here](https://github.com/dominikh/go-mode.el)
2. __go-check plugin__: in order to perform fine-grained testing (i.e. run a single test instead of all tests, for instance), `go-check-mode` depends (not surprisingly) on the [gocheck library](http://labix.org/gocheck)

Once all dependcies are in place, go ahead and install `go-check-mode` on the machine of your choice:

1. Either git clone this repo or simply download the raw `go-check-mode.el` file, whichever strikes your fancy
2. Put `go-check-mode.el` somewhere on Emac's load path so it can be found by those who seek it
3. Activate the mode itself by adding the following `require` line in your .emacs file

       `(require 'go-check-mode)`

4. Bask in the glory that is TDD without having to bail out of the marvelous world of Emacs to run your specs


## Customizations
The following variables can be customized to better suite your workflow (`M-x customize-group <RET> go-check-mode <RET>`):

1. __go-check-key-command-prefix__: `go-check-mode` uses `C-c ,` as its keymap prefix command by default, but this is configurable to anything of your choosing
2. __go-check-runner__: `go-check-mode` normally runs tests by calling out to `go test` via Emacs' absurdly powerful compilation-mode. You can change the `go test` command by setting `go-check-runner` to something else. Of course, note that whatever this command turns out to be, it *must* support [gocheck's](http://labix.org/gocheck) CLI flags (mainly `-gocheck.f`, `-gocheck.v`, and `-gocheck.vv`)

Remember, with great power comes great responsibility! Configure at your own peril.


## Command overview

The keybindings made available by `go-check-mode` are listed below, assuming you've kept the default keymap prefix:

| Key      | Name                | Behavior                                                                                                |
| -------- |:--------------------|:--------------------------------------------------------------------------------------------------------|
|`C-c , a` |__go-check-all__     | run all tests within the current directory                                                              |
|`C-c , s` |__go-check-single__  | run the test function the cursor is currently in. If the mark is active, run all tests found in region. |
|`C-c , c` |__go-check-current__ | run all tests in the file                                                                               |
|`C-c , e` |__go-check-ad-hoc__  | run all tests that match the regexp provided in the mini-buffer                                         |
|`C-c , r` |__go-check-rerun__   | re-run the previous test command                                                                        |
|`C-c , t` |__go-check-toggle__  | toggle between a test file and its target file                                                          |

Note that `go-check-mode` will make different keybindings available depending on what buffer is currently being visited (i.e. has focus):

- __Within an _test.go file__: all commands are available
- __Within a regular .go file__: only `go-check-rerun`, `go-check-all`, and `go-check-toggle` are available
- __Within any other file__: only `go-check-rerun` is available


### Prefixes
One can use Emacs' [prefix command arguments](http://www.gnu.org/software/emacs/manual/html_node/elisp/Prefix-Command-Arguments.html) to have better control over the compilation process' output. Each command (save for `go-check-rerun`) can be run with one of the following prefixes:

1. __C-1__: run tests with the `-gocheck.v` flag
2. __C-2__: run tests with the `-gocheck.vv` flag
3. __C-u__: edit the compilation command immediately before the command itself is run

So, for instance, doing `C-u C-c , s` lets you modify the `go-check-single` command to include another test of your choosing.


### Caveats
`go-check-mode` assumes a fairly flat file structure when it comes to how tests are organized. Basically, a test file is expected to be in the same directory as its target file and should be named identically to the target file except with a `_test.go` suffix instead of simply the `.go` extension.


## Notes:
- Much thanks to [rspec-mode](https://github.com/pezra/rspec-mode>), from which came inspiration for many of the ideas used here
