# python-MLS: multi-line Python shell commands in Emacs

<img src="https://user-images.githubusercontent.com/93749/134784188-7ac9ee9d-8e29-4c7f-82d7-2881e96d4bd2.gif">

Python-MLS (multi-line shell) is an Emacs minor mode for working directly with multi-line Python commands in the REPL. It builds on and automatically extends python-mode's inferior shell capabilities.

# Features:

- Works with both python and ipython.
- Arbitrary command lengths.
- Auto-detects and handles native continuation prompts.
- Auto-indents multi-line commands.
- Normal emacs and python editing commands work (e.g. tab to cycle indent).
- Replaces python-mode's external buffer fontifications with in-buffer
  fontification for signficant speedup on long multi-line input statements.
- Up/Down arrow history browsing with and without block movement
  (try shift arrow, or `C-S-n/p`).
- Saves and restore (multi-line) command history, per interpreter.
- Directly kill and yank multi-line code blocks to & from Python
  buffers, websites, etc..
  
# Installation

Simply install python-MLS from this repository or MELPA, and use `require` or `use-package` to load it.  To enable, arrange to have `python-mls-setup` called immediately after `python-mls` is loaded, e.g.:

```elisp
(use-package python-mls
	:after python
	:config
	(python-mls-setup))
```

Python-MLS will then  automatically enable itself in your python inferior shell, which you can start however you normally do (e.g. `C-c C-p` in a Python buffer, or `M-x run-python`).  Note: if you use `:bind` in your use-package stanza, you should add `:defer nil` to ensure the setup is run as soon as `python.el` is loaded.

# Usage

Using python-MLS is as simple as entering the first of a multi-line statement at the (i)Python prompt, then editing as you would in a python buffer.  Hit `S-Ret` or enter two final blank lines to execute. You can disable python-MLS for _future_ shells in one Emacs session with `M-x python-mls-mode` in an enabled shell buffer. 

# Keys

- `Up`/`Down` (or `C-p`/`C-n`): when on the last line in that direction, navigate history, otherwise move _within_ multi-line statements. 
-  `S-Up`/`S-Down` (or `C-S-p`/`C-S-n`): skip through multi-line statements in command history without navigating inside them.  To configure the modifier used, see `python-mls-multiline-history-modifier`. 
- `S-Ret` or `M-Ret`: send a multi-line statement from anywhere within it.
-  Two blank lines at statement end: send the multi-line statement.
- `C-j`: break an initial line into a multi-line command anywhere. 
- `C-r`/`C-s`: Search backward/forward through command history. 
- `M-a`/`M-e`/`C-M-u`: (by default) python-nav bindings for backward-, forward-block and up-list are brought over from python mode.  See `python-mls-import-python-nav-command-list` to add more. 
- `C-d`: send EOF, if at a prompt at the end of the buffer. 

# Config

A few options are available for configuration, including whether and where to save multi-line command history, whether to kill the window after the Python process quits, and which navigation commands to bring over from python-mode.  To configure, use `M-x customize-group [Ret] python-mls`. 

# FAQs

- **How does Python-MLS work?** It looks for the normal continuation prompt (e.g. ` ...`) in (i)Python's output, and if found, quietly interrupts the process, removes the partial command from the history, and starts a "native" emacs continued statement. On continued lines, it uses a computed _line prefix_ which matches your prompt.  Importantly, the prefixes added are only _decoration_, and are not in the actual text, so it is easy to select, navigate, kill, yank and operate on multi-line statements just as you would in a normal python-mode buffer.

- **How else can you make a multi-line statement?** You can break any line at any time with `C-j`.  So a multi line-statement like the following is perfectly allowable:
  ```
  >>> x=1[C-j]
  ... y=2[C-j]
  ... z=3[S-Ret]
  ```

- **Copy and paste?** Just try it.  Python-MLS configures python-mode to strip out the `...` decorative line prefixes on yank, and it adds them back in automatically when copying text from elsewhere into the shell.

- **How does this differ from iPython's native multi-line input capability?** iPython's multi-line input was a source of inspiration for python-MLS, but it differs in a few ways: 

  1. iPython evaluates each line of a multi-line statement as it gets inputed, and checks for syntax errors line by line, whereas python-MLS lets you edit and then sends the complete block _en masse_ at the end, so any errors will appear at that point.  
  3. Python-MLS lets you move and edit right across lines using normal emacs commands (even python navigation commands), whereas iPython treats each line as a separate entity you can access via up/down arrow _only_.  As a result, editing python-MLS multi-line commands feels just about like editing code in a source buffer. 

- **How do you skip over entire multi-line statements in history without having to move all the way through them?** Try holding Shift: `S-up`,`S-down` (or `C-S-n/p`).

- **Why does python-MLS do fontification?** By default, inferior-python-mode copies the full input text at the prompt back and forth from the inferior shell to a hidden Python buffer, and fully refontifies it there _after every keypress_.  With short, single-line statements this isn't such a problem, but with long multi-line input this becomes highly inefficient. Python-MLS replaces this method entirely with a special in-buffer fontification function, that does double duty, both adding the continuation line prefix decorations (e.g. ` ...`) and performing default python-mode fontification on just the input, all in one efficient pass.  This also increases performance and saves the creation of random fontification buffers.

- **My py-XXX functions aren't working!** There are two popular packages providing python-mode for source buffers and the shell.  python-MLS makes use of the built-in python.el version, as opposed to the [MELPA package](https://melpa.org/#/python-mode).  Either can be used in the python _buffer_ with python-MLS. But if you use the non-builtin python-mode, you'll have to ensure the python.el shell is run.  

- **Any other tips?**  
   - Get to know the useful commands provided by `comint` (which most Emacs inferior shells use): 
     - `C-c C-u`: `comint-kill-input`
     - `C-c C-o`: `comint-kill-output`
     - `C-c M-o`: `comint-clear-buffer`
     - and more.
   - Python-MLS binds a few keyboard shortcuts for python navigation for use after the prompt. See `python-mls-import-python-nav-command-list` to add more. 
