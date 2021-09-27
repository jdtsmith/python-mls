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
- Directly kill/yank multi-line code blocks to & from Python
  buffers.
  
# Installation

Simply install python-mls from this repository or MELPA (TBA), and use `require` or `use-package` to load it.  It automatically enables itself in your python inferior shell, which you can start however you normally do (e.g. `M-x run-python`).  

# Usage

Using python-mls is as simple as entering the first multi-line statement at the (i)Python prompt, then editing as you would in a python buffer.  Hit `S-Ret` or enter two final blank lines to execute. 

# Keys

- `S-Ret` or `M-Ret`: send a multi-line statement, or enter two blank lines at end of statement.
- `C-j`: break the current line into a multi-line command anywhere with. 
- `Up`/`Down` (or `C-p`/`C-n`): navigate history, or within multi-line statements. Hold shift to skip past multi-line statements in history.
- `M-a`/`M-e`/`C-M-u`: (by default) python-nav bindings for backward-, forward-block and up-list are brought over from python mode.  See `python-mls-import-python-nav-command-list` to add more. 
- `C-d`: send EOF if at prompt at end.

# Config

A few options are available for configuration, including whether and where to save multi-line command history, whether to kill the window after the process quits, and which navigation commands to bring over from python-mode.  To configure, use `M-x customize-group [Ret] python-mls`. 

# FAQs

- **How does Python-MLS work?** It looks for the normal continuation prompt in (i)python's output, and if found, quietly interrupts the process, removes the partial command from the history, and starts a "native" emacs continued statement. On continued lines it computes and uses a _line prefix_ computed to match your prompt, like `  ...`.  Prefixes are only _decoration_, and are not in the text, so it is easy to select and operate on the text like normal. 

- **How else can you make a multi-line statement?** You can break any line at any time with `C-j`.  So a multi line-statement like the following is perfectly allowable:
  ```
  >>> x=1[C-j]
  ... y=2[C-j]
  ... z=3[S-Ret]
  ```

- **Copy and paste?** Just try it.  Python-mls configures python-mode to strip out the `...` line prefixes on yank, and it adds them back in automatically.

- **How does this differ from iPython's multi-line input capability?** In a few ways: iPython evaluates each line as it is inputed, and checks for syntax errors line by line.  Python-mls lets you edit and then sends the complete block en masse at the end.  Python-mls lets you move right across lines using normal emacs movement (even python navigation commands), whereas iPython treats each line as a separate entity you can access via up/down arrow only.

- **How do you skip over entire multi-line statements in history without having to move all the way through them?** Try holding Shift with arrows: `S-up`,`S-down` (or `C-S-n/p`).

- **Why does python-mls handle fontification?** By default, python-mode copies the full input text back and forth from the inferior shell to a hidden Python buffer, and fully refontifies it there _after every keypress_. With short, single-line statements this isn't a problem, but with long multi-line input this becomes highly inefficient. Python-mls replaces this with a special in-buffer fontification function which both adds the continuation line prefixes (e.g. ` ...`) and performs default python-mode fontification, all in one efficient pass.  This also saves the creation of random fontification buffers.

- **Any other tips?**  
   - Get to know the useful commands provided by `comint` (which most Emacs inferior shells use): `C-c C-u`: `comint-kill-input`; `C-c C-o`: `comint-kill-output`; `C-c M-o`: `comint-clear-buffer`, and more.
   - Python-mls binds a few keyboard shortcuts for python navigation for use after the prompt. See `python-mls-import-python-nav-command-list` to add more. 
