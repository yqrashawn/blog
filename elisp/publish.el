;;; publish --- Publish org files to GitLab Pages

;;; Commentary:

;; This file takes care of exporting org files to the public directory.
;; Images and such are also exported without any processing.

;;; Code:

(require 'package)
(package-initialize)
(unless package-archive-contents
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-refresh-contents))
(dolist (pkg '(org-plus-contrib htmlize))
  (unless (package-installed-p pkg)
    (package-install pkg)))

(require 'cl-lib)
(require 'org)
(require 'ox-publish)
(require 'ox-rss)

(defvar rw-url "https://yqrashawn.com/"
  "The URL where this site will be published.")

(defvar rw-title "infin | yqrashawn.com"
  "The title of this site.")

(defvar rw--root
  (locate-dominating-file default-directory
                          (lambda (dir)
                            (seq-every-p
                             (lambda (file) (file-exists-p (expand-file-name file dir)))
                             '(".git" "content" "css" "elisp" "favicon.ico" "layouts" "posts"))))
  "Root directory of this project.")

(defvar rw--layouts-directory
  (expand-file-name "layouts" rw--root)
  "Directory where layouts are found.")

(defvar rw--site-attachments
  (regexp-opt '("jpg" "jpeg" "gif" "png" "svg"
                "ico" "cur" "css" "js"
                "eot" "woff" "woff2" "ttf"
                "html" "pdf"))
  "File types that are published as static files.")

(defun rw--pre/postamble-format (type)
  "Return the content for the pre/postamble of TYPE."
  `(("en" ,(with-temp-buffer
             (insert-file-contents (expand-file-name (format "%s.html" type) rw--layouts-directory))
             (buffer-string)))))

(defun rw/format-date-subtitle (file project)
  "Format the date found in FILE of PROJECT."
  (format-time-string "posted on %Y-%m-%d" (org-publish-find-date file project)))

(defun rw/org-html-close-tag (tag &rest attrs)
  "Return close-tag for string TAG.
ATTRS specify additional attributes."
  (concat "<" tag " "
          (mapconcat (lambda (attr)
                       (format "%s=\"%s\"" (car attr) (cadr attr)))
                     attrs
                     " ")
          ">"))

(defun rw/html-head-extra (file project)
  "Return <meta> elements for nice unfurling on Twitter and Slack."
  (let* ((info (cdr project))
         (org-export-options-alist
          `((:title "TITLE" nil nil parse)
            (:date "DATE" nil nil parse)
            (:author "AUTHOR" nil ,(plist-get info :author) space)
            (:description "DESCRIPTION" nil nil newline)
            (:keywords "KEYWORDS" nil nil space)
            (:meta-image "META_IMAGE" nil ,(plist-get info :meta-image) nil)
            (:meta-type "META_TYPE" nil ,(plist-get info :meta-type) nil)))
         (title (org-publish-find-title file project))
         (date (org-publish-find-date file project))
         (author (org-publish-find-property file :author project))
         (description (org-publish-find-property file :description project))
         (link-home (file-name-as-directory (plist-get info :html-link-home)))
         (extension (or (plist-get info :html-extension) org-html-extension))
         (rel-file (org-publish-file-relative-name file info))
         (full-url (concat link-home (file-name-sans-extension rel-file) "." extension))
         (image (concat link-home (org-publish-find-property file :meta-image project)))
         (favicon (concat link-home "favicon.ico"))
         (type (org-publish-find-property file :meta-type project)))
    (mapconcat 'identity
               `(,(rw/org-html-close-tag "link" '(rel icon) '(type image/x-icon) `(href ,favicon))
                 ,(rw/org-html-close-tag "link" '(rel alternate) '(type application/rss+xml) '(href "rss.xml") '(title "RSS feed"))
                 ,(rw/org-html-close-tag "meta" '(property og:title) `(content ,title))
                 ,(rw/org-html-close-tag "meta" '(property og:url) `(content ,full-url))
                 ,(and description
                       (rw/org-html-close-tag "meta" '(property og:description) `(content ,description)))
                 ;; ,(rw/org-html-close-tag "meta" '(property og:image) `(content ,image))
                 ,(rw/org-html-close-tag "meta" '(property og:type) `(content ,type))
                 ,(and (equal type "article")
                       (rw/org-html-close-tag "meta" '(property article:author) `(content ,author)))
                 ,(and (equal type "article")
                       (rw/org-html-close-tag "meta" '(property article:published_time) `(content ,(format-time-string "%FT%T%z" date))))

                 ,(rw/org-html-close-tag "meta" '(property twitter:title) `(content ,title))
                 ,(rw/org-html-close-tag "meta" '(property twitter:url) `(content ,full-url))
                 ,(rw/org-html-close-tag "meta" '(property twitter:image) `(content ,image))
                 ,(and description
                       (rw/org-html-close-tag "meta" '(property twitter:description) `(content ,description)))
                 ,(and description
                       (rw/org-html-close-tag "meta" '(property twitter:card) '(content summary))))
               "\n")))

(defun rw/org-html-publish-to-html (plist filename pub-dir)
  "Wrapper function to publish an file to html.

PLIST contains the properties, FILENAME the source file and
  PUB-DIR the output directory."
  (let ((project (cons 'rw plist)))
    (plist-put plist :subtitle
               (rw/format-date-subtitle filename project))
    (plist-put plist :html-head-extra
               (rw/html-head-extra filename project))
    (org-html-publish-to-html plist filename pub-dir)))

(defun rw/org-html-format-headline-function (todo todo-type priority text tags info)
  "Format a headline with a link to itself.

This function takes six arguments:
TODO      the todo keyword (string or nil).
TODO-TYPE the type of todo (symbol: ‘todo’, ‘done’, nil)
PRIORITY  the priority of the headline (integer or nil)
TEXT      the main headline text (string).
TAGS      the tags (string or nil).
INFO      the export options (plist)."
  (let* ((headline (get-text-property 0 :parent text))
         (id (or (org-element-property :CUSTOM_ID headline)
                 (org-export-get-reference headline info)
                 (org-element-property :ID headline)))
         (link (if id
                   (format "<a href=\"#%s\">%s</a>" id text)
                 text)))
    (org-html-format-headline-default-function todo todo-type priority link tags info)))

(defun rw/org-publish-sitemap (title list)
  "Generate sitemap as a string, having TITLE.
LIST is an internal representation for the files to include, as
returned by `org-list-to-lisp'."
  (let ((filtered-list (cl-remove-if (lambda (x)
                                       (and (sequencep x) (null (car x))))
                                     list)))
    (concat "#+TITLE: " title "\n"
            "#+OPTIONS: title:nil\n"
            "#+META_TYPE: website\n"
            "#+DESCRIPTION: yqrashawn' personal blog\n"
            "\n#+ATTR_HTML: :class sitemap\n"
            ; TODO use org-list-to-subtree instead
            (org-list-to-org filtered-list))))

(defun rw/org-publish-sitemap-entry (entry style project)
  "Format for sitemap ENTRY, as a string.
ENTRY is a file name.  STYLE is the style of the sitemap.
PROJECT is the current project."
  (unless (equal entry "404.org")
    (format "[[file:%s][%s]] /%s/"
            entry
            (org-publish-find-title entry project)
            (rw/format-date-subtitle entry project))
    ;; (format "[[file:%s][%s]] /%s/"
    ;;         (format "%s-%s" (format-time-string "%Y-%m-%d" (org-publish-find-date entry project)) entry)
    ;;         (org-publish-find-title entry project)
    ;;         (rw/format-date-subtitle entry project))
    ))

(defun rw/format-rss-feed-entry (entry style project)
  "Format ENTRY for the RSS feed.
ENTRY is a file name.  STYLE is either 'list' or 'tree'.
PROJECT is the current project."
  (cond ((not (directory-name-p entry))
         (let* ((file (org-publish--expand-file-name entry project))
                (title (org-publish-find-title entry project))
                (date (format-time-string "%Y-%m-%d" (org-publish-find-date entry project)))
                (link (concat (file-name-sans-extension entry) ".html")))
           (with-temp-buffer
             (insert (format "* [[file:%s][%s]]\n" file title))
             (org-set-property "RSS_PERMALINK" link)
             (org-set-property "PUBDATE" date)
             (insert-file-contents file)
             (buffer-string))))
        ((eq style 'tree)
         ;; Return only last subdir.
         (file-name-nondirectory (directory-file-name entry)))
        (t entry)))

(defun rw/format-rss-feed (title list)
  "Generate RSS feed, as a string.
TITLE is the title of the RSS feed.  LIST is an internal
representation for the files to include, as returned by
`org-list-to-lisp'.  PROJECT is the current project."
  (concat "#+TITLE: " title "\n\n"
          (org-list-to-subtree list 1 '(:icount "" :istart ""))))

(defun rw/org-rss-publish-to-rss (plist filename pub-dir)
  "Publish RSS with PLIST, only when FILENAME is 'rss.org'.
PUB-DIR is when the output will be placed."
  (if (equal "rss.org" (file-name-nondirectory filename))
      (org-rss-publish-to-rss plist filename pub-dir)))

(defun rw/publish-redirect (plist filename pub-dir)
  "Generate redirect files from the old routes to the new.
PLIST contains the project info, FILENAME is the file to publish
and PUB-DIR the output directory."
  (let* ((regexp (org-make-options-regexp '("REDIRECT_FROM")))
         (from (with-temp-buffer
                 (insert-file-contents filename)
                 (if (re-search-forward regexp nil t)
                     (org-element-property :value (org-element-at-point))))))
    (when from
      (let* ((to-name (file-name-sans-extension (file-name-nondirectory filename)))
             (to-file (format "/%s.html" to-name))
             (from-dir (concat pub-dir from))
             (from-file (concat from-dir "index.html"))
             (other-dir (concat pub-dir to-name))
             (other-file (concat other-dir "/index.html"))
             (to (concat (file-name-sans-extension (file-name-nondirectory filename))
                         ".html"))
             (layout (plist-get plist :redirect-layout))
             (content (with-temp-buffer
                        (insert-file-contents layout)
                        (while (re-search-forward "REDIRECT_TO" nil t)
                          (replace-match to-file t t))
                        (buffer-string))))
        (make-directory from-dir t)
        (make-directory other-dir t)
        (with-temp-file from-file
          (insert content)
          (write-file other-file))))))


(defvar rw--publish-project-alist
      (list
       (list "blog-posts"
             :base-directory (expand-file-name "posts" rw--root)
             :base-extension "org"
             :recursive nil
             :exclude (regexp-opt '("rss.org" "index.org"))
             :publishing-function 'rw/org-html-publish-to-html
             :publishing-directory (expand-file-name "public" rw--root)
             :html-head-include-default-style nil
             :html-head-include-scripts nil
             :html-htmlized-css-url "css/style.css"
             :html-preamble-format (rw--pre/postamble-format 'preamble)
             :html-postamble t
             :html-postamble-format (rw--pre/postamble-format 'postamble)
             :html-format-headline-function 'rw/org-html-format-headline-function
             :html-link-home rw-url
             :html-home/up-format ""
             :auto-sitemap t
             :sitemap-filename "index.org"
             :sitemap-title rw-title
             :sitemap-style 'list
             :sitemap-sort-files 'anti-chronologically
             :sitemap-function 'rw/org-publish-sitemap
             :sitemap-format-entry 'rw/org-publish-sitemap-entry
             :author "yqrashawn"
             :email ""
             ;; :meta-image "content/rw-r--r--square.png"
             :meta-type "article")
       (list "blog-rss"
             :base-directory (expand-file-name "posts" rw--root)
             :base-extension "org"
             :recursive nil
             :exclude (regexp-opt '("rss.org" "index.org" "404.org"))
             :publishing-function 'rw/org-rss-publish-to-rss
             :publishing-directory (expand-file-name "public" rw--root)
             :rss-extension "xml"
             :html-link-home rw-url
             :html-link-use-abs-url t
             :html-link-org-files-as-html t
             :auto-sitemap t
             :sitemap-filename "rss.org"
             :sitemap-title rw-title
             :sitemap-style 'list
             :sitemap-sort-files 'anti-chronologically
             :sitemap-function 'rw/format-rss-feed
             :sitemap-format-entry 'rw/format-rss-feed-entry
             :author "yqrashawn"
             :email "")
       (list "blog-static"
             :base-directory rw--root
             :exclude (regexp-opt '("public/" "layouts/"))
             :base-extension rw--site-attachments
             :publishing-directory (expand-file-name "public" rw--root)
             :publishing-function 'org-publish-attachment
             :recursive t)
       (list "blog-acme"
             :base-directory (expand-file-name ".well-known" rw--root)
             :base-extension 'any
             :publishing-directory (expand-file-name "public/.well-known" rw--root)
             :publishing-function 'org-publish-attachment
             :recursive t)
       (list "blog-redirects"
             :base-directory (expand-file-name "posts" rw--root)
             :base-extension "org"
             :recursive nil
             :exclude (regexp-opt '("rss.org" "index.org" "404.org"))
             :publishing-function 'rw/publish-redirect
             :publishing-directory (expand-file-name "public" rw--root)
             :redirect-layout (expand-file-name "layouts/redirect.html" rw--root))
       (list "site"
             :components '("blog-posts" "blog-rss" "blog-static" "blog-acme" "blog-redirects"))
       ))

(defun rw-publish-all ()
  "Publish the blog to HTML."
  (interactive)
  (let ((org-publish-project-alist       rw--publish-project-alist)
        (org-publish-timestamp-directory "./.timestamps/")
        (org-export-with-section-numbers nil)
        (org-export-with-smart-quotes    t)
        (org-export-with-toc             nil)
        (org-export-with-sub-superscripts '{})
        (org-html-divs '((preamble  "header" "top")
                         (content   "main"   "content")
                         (postamble "footer" "postamble")))
        (org-html-container-element         "section")
        (org-html-metadata-timestamp-format "%Y-%m-%d")
        (org-html-checkbox-type             'html)
        (org-html-html5-fancy               t)
        (org-html-validation-link           nil)
        (org-html-doctype                   "html5")
        (org-html-htmlize-output-type       'css))
    (org-publish-all)))

;;; publish.el ends here
