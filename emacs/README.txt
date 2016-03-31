After first starting Emacs, add:

  (load "mrshpot-emacs.el")
  
to your ~/.emacs file and restart.


After Emacs prompts about packages & theme, edit ~/.emacs to put the
`load` after customizations.


The `emacs-w32.cmd' script will automatically set up $EMACSLOADPATH,
but under Linux you will need to set it from ~/.emacs like this:

  (add-to-list 'load-path "~/dotfiles/emacs/site-lisp")
  (add-to-list 'load-path "~/dotfiles/emacs")
  (load "mrshpot-emacs.el")

