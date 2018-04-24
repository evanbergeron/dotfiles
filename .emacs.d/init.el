(package-initialize)

(add-to-list 'load-path (expand-file-name "init" user-emacs-directory))

(require 'init-package)

(require 'init-evil)
(require 'init-ido)
(require 'init-windowing)
(require 'init-visual)
