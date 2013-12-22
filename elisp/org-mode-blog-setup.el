(setq org-publish-project-alist
      '(("blog"
         :components ("blog-content" "blog-static"))
        ("blog-content"
         :base-directory "~/org-mode-blog/"
         :base-extension "org"
         :publishing-directory org-mode-blog-publishing-directory
         :recursive t
         :publishing-function org-html-publish-to-html
         :export-with-tags nil
         :headline-levels 4
         :auto-sitemap t
         :sitemap-title "Sitemap"
         :section-numbers nil
         :with-toc nil
         :with-author nil
         :with-creator nil
         :html-doctype "html5"
         :html-preamble org-mode-blog-preamble
         :html-postamble "<hr><div id='comments'></div>"
         :html-head  "<link rel=\"stylesheet\" href=\"/css/style.css\" type=\"text/css\"/>\n"
         :html-head-extra "<script async=\"true\" src=\"/js/juvia.js\"></script>
         <link rel=\"shortcut icon\" href=\"/img/steckerhalter.ico\">
         <meta name=\"viewport\" content=\"width=device-width, initial-scale=1, maximum-scale=1\" />"
         :html-html5-fancy t
         :html-head-include-default-style nil
         )
        ("blog-static"
         :base-directory "~/org-mode-blog/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|ico"
         :publishing-directory org-mode-blog-publishing-directory
         :recursive t
         :publishing-function org-publish-attachment)))

(defun org-mode-blog-preamble (options)
  "The function that creates the preamble (sidebar) for the blog.
OPTIONS contains the property list from the org-mode export."
  (let ((base-directory (plist-get options :base-directory)))
    (org-babel-with-temp-filebuffer (expand-file-name "html/preamble.html" base-directory) (buffer-string))))
