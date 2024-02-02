;; -*- no-byte-compile: t; -*-
;;; tools/gptel/packages.el

;; (package! gptel :recipe (:host github :repo "karthink/gptel"))
(package! gptel :recipe (:host github :repo "akhil3417/gptel"))
(when (modulep! +extension)
  (package! gptel-extensions.el :recipe (:host github :repo "kamushadenes/gptel-extensions.el")))

(when (modulep! +aipihkal)
(package! AIPIHKAL
 :recipe (:host github :repo "gregoryg/AIPIHKAL"
          :files ("system-prompts/*.md")))) ;who would have thought
