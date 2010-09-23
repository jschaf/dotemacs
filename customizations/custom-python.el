
(defadvice pylint-python-hook (around pylint-python-hook-around activate)
  "Add `py-mode-map' to `pylint-python-hook' because it is
missing in the vanilla `python-mode'"
  (let ((py-mode-map))
    (setq py-mode-map python-mode-map)
    ad-do-it))
