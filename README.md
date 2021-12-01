# python-mls: multi-line Python shell commands in Emacs

Python-MLS (multi-line shell) is an Emacs minor mode for working directly with multi-line Python commands in the REPL. It builds on and automatically extends python-mode's inferior shell capabilities.

&nbsp;&nbsp;&nbsp;&nbsp;   ![python-mls_crop](https://user-images.githubusercontent.com/93749/134784188-7ac9ee9d-8e29-4c7f-82d7-2881e96d4bd2.gif)


# Features:

- Works with both python and ipython.
- Arbitrary command lengths.
- Auto-detects and handles native continuation prompts.
- Auto-indents multi-line commands.
- Normal python editing commands work (e.g. tab to cycle indent).
- Replaces python-mode's external buffer fontifications with in-buffer
  fontification for signficant speedup on long multi-line input statements.
- Up/Down arrow history browsing with and without block movement
  (try shift arrow, or `C-S-n/p`).
- Saves and restore (multi-line) command history.
- Directly kill and yank multi-line code blocks to & from Python
  buffers.
  
# Installation

Simply install python-mls from this repository or MELPA (TBA), and use `require` or `use-package` to load it.  It automatically enables itself in your python inferior shell, which you can start however you normally do (e.g. `M-x run-python`).  

# Usage

Using python-mls is as simple as entering the first multi-line statement at the (i)Python prompt, then editing as you would in a python buffer.  Hit `S-Ret` or enter two final blank lines to execute. 

# Keys

- `S-Ret` or `M-Ret`: send a multi-line statement from anywhere within it.
-  Enter two blank lines: send the multi-line statement.
- `C-j`: break an initial line into a multi-line command anywhere. 
- `Up`/`Down` (or `C-p`/`C-n`): navigate history, or move _within_ multi-line statements. 
-  `S-Up`/`S-Down` (or `C-S-p`/`C-S-n`): skip through multi-line statements in command history.
- `M-a`/`M-e`/`C-M-u`: (by default) python-nav bindings for backward-, forward-block and up-list are brought over from python mode.  See `python-mls-import-python-nav-command-list` to add more. 
- `C-d`: send EOF, if at a prompt at the end of the buffer. 

# Config

A few options are available for configuration, including whether and where to save multi-line command history, whether to kill the window after the Python process quits, and which navigation commands to bring over from python-mode.  To configure, use `M-x customize-group [Ret] python-mls`. 

# FAQs

- **How does Python-MLS work?** It looks for the normal continuation prompt (e.g. ` ...`) in (i)Python's output, and if found, quietly interrupts the process, removes the partial command from the history, and starts a "native" emacs continued statement. On continued lines, it uses a computed _line prefix_ which matches your prompt.  Importantly, the prefixes added are only _decoration_, and are not in the actual text, so it is easy to select, navigate, kill, yank and operate on it just as in a normal python-mode buffer.

- **How else can you make a multi-line statement?** You can break any line at any time with `C-j`.  So a multi line-statement like the following is perfectly allowable:
  ```
  >>> x=1[C-j]
  ... y=2[C-j]
  ... z=3[S-Ret]
  ```

- **Copy and paste?** Just try it.  Python-mls configures python-mode to strip out the `...` decorative line prefixes on yank, and it adds them back in automatically when copying text from elsewhere into the shell.

- **How does this differ from iPython's native multi-line input capability?** iPython's multi-line input was a source of inspiration for python-mls, but it differs in a few ways: iPython evaluates each line of a multi-line statement as it gets inputed, and checks for syntax errors line by line.  Python-mls lets you edit and then sends the complete block _en masse_ at the end, so any errors will appear at that point.  Python-mls also lets you move and edit right across lines using normal emacs commands (even python navigation commands), whereas iPython treats each line as a separate entity you can access via up/down arrow _only_.  Editing python-mls multi-line commands feels just like editing code in a source buffer. 

- **How do you skip over entire multi-line statements in history without having to move all the way through them?** Try holding Shift: `S-up`,`S-down` (or `C-S-n/p`).

- **Why does python-mls do fontification?** By default, inferior-python-mode copies the full input text at the prompt back and forth from the inferior shell to a hidden Python buffer, and fully refontifies it there _after every keypress_.  With short, single-line statements this isn't such a problem, but with long multi-line input this becomes highly inefficient. Python-mls replaces this method entirely with a special in-buffer fontification function, that does double duty, both adding the continuation line prefix decorations (e.g. ` ...`) and performing default python-mode fontification on just the input, all in one efficient pass.  This also increases performance and saves the creation of random fontification buffers.

- **Any other tips?**  
   - Get to know the useful commands provided by `comint` (which most Emacs inferior shells use): `C-c C-u`: `comint-kill-input`; `C-c C-o`: `comint-kill-output`; `C-c M-o`: `comint-clear-buffer`, and more.
   - Python-mls binds a few keyboard shortcuts for python navigation for use after the prompt. See `python-mls-import-python-nav-command-list` to add more. 
