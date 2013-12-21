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
         :html-preamble "
<div class=\"navbar navbar-static-top\">
    <div class=\"navbar-inner\">
        <div class=\"container\">
            <a class=\"brand\" href=\"/\">steckerhalter.co.vu</a>
            <a class=\"brand twitter right\" href=\"https://twitter.com/steckerhalter\">
                <img src=\"https://dev.twitter.com/sites/default/files/images_documentation/bird_blue_32.png\" width=\"32\" height=\"25\" alt=\"\" title=\"\">
                steckerhalter
            </a>
        </div>
    </div>
</div>
"
         :html-postamble "<hr><div id='comments'></div>"
         :html-head  "<link rel=\"stylesheet\" href=\"/css/style.css\" type=\"text/css\"/>\n"
         :html-head-extra "<script async=\"true\" src=\"/js/juvia.js\"></script>"
         :html-html5-fancy t
         :html-head-include-default-style nil
         )
        ("blog-static"
         :base-directory "~/org-mode-blog/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf"
         :publishing-directory org-mode-blog-publishing-directory
         :recursive t
         :publishing-function org-publish-attachment)))


