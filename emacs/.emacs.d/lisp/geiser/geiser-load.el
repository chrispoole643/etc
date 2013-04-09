(setq geiser-elisp-dir (file-name-directory load-file-name))
(add-to-list 'load-path geiser-elisp-dir)

(require 'geiser)

(setq geiser-scheme-dir "/Users/Chris/Documents/inbox/geiser-0.3/scheme")

(provide 'geiser-load)
