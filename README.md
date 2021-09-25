# python-mls

Python-MLS (multi-line shell) is an Emacs minor mode for working directly with multi-line Python statements in the REPL. It builds on and automatically extends python-mode's inferior shell capabilities.

&nbsp;&nbsp;&nbsp;&nbsp;   ![python-mls_crop](https://user-images.githubusercontent.com/93749/134784188-7ac9ee9d-8e29-4c7f-82d7-2881e96d4bd2.gif)


# Features:

- Works with both python and ipython.
- Arbitrary command lengths.
- Auto-detects and handles native continuation prompts.
- Auto-indents multi-line commands.
- Normal python editing commands work (e.g. tab to indent).
- Replaces buffer-based fontifications with in-buffer python-mode
  fontification for dramatic speedup.
- Up/Down arrow history browsing with and without block movement
  (try shift arrow, or `C-S-n/p`).
- Saves and restore (multi-line) command history.
- Directly kill/yank multi-line code blocks to & from Python
  buffers.
  
# Installation

Simply install python-mls from this repository or MELPA (TBA), and use `require` or `use-package` to load it.  It automatically enables itself in your python inferior shell, which you can start however you normally do (e.g. `M-x run-python`).  

# Usage

Using python-mls is as simple as entering the first multi-line statement at the (i)Python prompt, then editing as you would in a python buffer. 

Special keys:

- To send a multi-line statement, either enter two blank lines at the end, or use `S-Ret` or `M-Ret` at any position.  
- `C-j` breaks the current line into a multi-line command anywhere with. 
- `Up`/`Down` (or `C-p`/`C-n`) navigate history, or within multi-line statements.  Hold shift to skip multi-line statements. 

See below for tips in importing other python navigation commands for use after the prompt with python-mls. 

# Config

A few options are available for configuration, including saving command history, and killing the window after the process quits.  To configure, use `M-x customize-group [Ret] python-mls-command-setup`. 

# FAQs:

- **How does Python-MLS work**: It looks for the normal continuation prompt in (i)python's output, and if found, quietly interrupts the process, removes the partial command from the history, and starts a "native" emacs continued statement. On continued lines it computes and uses a _line prefix_ computed to match your prompt, like `  ...`.  Prefixes are only _decoration_, and are not in the text, so it is easy to select and operate on the text like normal. 
- **How else can you make a multi-line statement?** You can break any line at any time with `C-j`.  So a multi line statement like the following is perfectly allowable:
  ```
  >>> x=1[C-j]
  ... y=2[C-j]
  ... z=3[S-Ret]
  ```
- **Copy and paste?**: Just try it.  Python-mls configures python-mode to strip out the "..." line prefixes on yank.  It adds them back in as needed.
- **How does this differ from iPython's multi-line capability?** In a few ways: iPython evaluates each line of a multi-line shell and checks for syntax errors etc., as they are entered.  Python-mls lets you edit and then sends the full block en masse at the end.  Python-mls lets you move right across lines using normal emacs movement (even python navigation commands), whereas iPython treats each line as a separate block you can access via up/down arrow only. 
- **How do you skip over entire multi-line statements in history without moving all the way through them?** Try holding Shift with the arrows: `S-up`,`S-down` (or `C-S-n/p`).
- **Why does python-mls touch fontification?** By default, python-mode copies text back and forth from the inferior shell to a hidden Python buffer, and fully refontifies it _after every keypress_. With short single-line statements this isn't a problem, but with long multi-line input this becomes highly inefficient. Python-mls replaces this with a special fontification function which both adds the continuation line prefixes and performs default python-mode fontification all in  one efficient pass, directly in the shell buffer itself (after the prompt only).  This also saves the creation of random fontification buffers.
- **Any other useful tips?**:  
   - Get to know the useful commands provided by `comint` (which most Emacs inferior shells use): `C-c C-u` `comint-kill-input` `C-c C-o` `comint-kill-output` `C-c M-o` `comint-clear-buffer`, and more.
   - Python-mls does not bind any keyboard shortcuts for python navigation or editing. Feel free to bind your own. One small but improvement would be to limit them to act only on text after the prompt. An easy way to arrange this is like:
 ```elisp
 (defun my/narrowed-command (command)
  (lambda (&rest r)
    (interactive)
    (save-restriction
      (narrow-to-region (cdr-safe comint-last-prompt) (point-max))
      (apply command r))))

 (bind-key [remap backward-sentence]
	  (my/narrowed-command #'python-nav-backward-block)
	  'inferior-python-mode-map)
 ```
