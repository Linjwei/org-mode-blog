(let* ((dir "posts")
       (files (directory-files dir t "^[^\\.][^#].*\\.org$" t))
       entries)
  (dolist (file files)
    (catch 'stop
      (let* ((path (concat dir "/" (file-name-nondirectory file)))
             (env (org-combine-plists (org-babel-with-temp-filebuffer file (org-export-get-environment))))
             (date (or (apply 'encode-time (org-parse-time-string
                                            (or (car (plist-get env :date)) (throw 'stop nil))))))
             (git-date (date-to-time (magit-git-string "log" "-1" "--format=%ci" file))))
        (plist-put env :path path)
        (plist-put env :parsed-date date)
        (plist-put env :git-date git-date)
        (push env entries))))
  (dolist (entry (sort entries (lambda (a b) (time-less-p (plist-get b :parsed-date) (plist-get a :parsed-date)))))
    (princ
     (format "* [[file:%s][%s]]
:PROPERTIES:
:PUBDATE: %s
:RSS_PERMALINK: %s
:END:
%s

Last update: %s\\\\
Published: %s

"
             (plist-get entry :path)
             (car (plist-get entry :title))
             (format-time-string (cdr org-time-stamp-formats) (plist-get entry :parsed-date))
             (concat (file-name-sans-extension (plist-get entry :path)) ".html")
             (plist-get entry :description)
             (format-time-string "%Y-%m-%d %H:%M" (plist-get entry :git-date))
             (format-time-string "%Y-%m-%d" (plist-get entry :parsed-date))))))
