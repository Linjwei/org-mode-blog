(let* ((dir "posts")
       (files (directory-files dir t "^[^\\.][^#].*\\.org$" t))
       entries)
  (dolist (file files)
    (catch 'stop
      (let* ((path (concat dir "/" (file-name-nondirectory file)))
             (git-date (date-to-time (or (magit-git-string "log" "-1" "--format=%ci" file) (throw 'stop nil))))
             (env (org-combine-plists (org-babel-with-temp-filebuffer file (org-export-get-environment)))))
        (plist-put env :path path)
        (plist-put env :git-date git-date)
        (push env entries))))
  (dolist (entry (sort entries (lambda (a b) (time-less-p (plist-get b :git-date) (plist-get a :git-date)))))
    (princ
     (format "* [[file:%s][%s]]
:PROPERTIES:
:PUBDATE: %s
:RSS_PERMALINK: %s
:END:
%s

Last update: %s

"
             (plist-get entry :path)
             (car (plist-get entry :title))
             (format-time-string (cdr org-time-stamp-formats) (plist-get entry :git-date))
             (concat (file-name-sans-extension (plist-get entry :path)) ".html")
             (plist-get entry :description)
             (format-time-string "%Y-%m-%d %H:%M" (plist-get entry :git-date))))))
