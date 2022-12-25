(require 'test-simple)
(test-simple-start)

(note "Init")
(assert-t (load-file "../helm-find-project-files.el"))

(note "Test helm-find-project-files-project-root-p")
(assert-nil (helm-find-project-files-project-root-p
             "./" helm-find-project-files-project-root-symbols))
(assert-t (helm-find-project-files-project-root-p
           "../" helm-find-project-files-project-root-symbols))
(assert-t (helm-find-project-files-project-root-p
           ".." helm-find-project-files-project-root-symbols))
(assert-t (helm-find-project-files-project-root-p
           "./../" helm-find-project-files-project-root-symbols))
(assert-t (helm-find-project-files-project-root-p
           "./.." helm-find-project-files-project-root-symbols))

(note "Test helm-find-project-files-find-git-dir")
(assert-equal ""
              (helm-find-project-files-find-git-dir nil))
(assert-equal (substring default-directory 0 -6)
              (helm-find-project-files-find-git-dir default-directory))
(assert-equal (substring default-directory 0 -6)
              (helm-find-project-files-find-git-dir
               (substring default-directory 0 -6)))
(assert-equal (substring default-directory 0 -5)
              (helm-find-project-files-find-git-dir
               (substring default-directory 0 -5)))

(note "Test helm-find-project-files-construct-find-option")
(assert-equal ""
              (helm-find-project-files-construct-find-option nil))
(assert-equal ""
              (helm-find-project-files-construct-find-option '()))
(assert-equal "-not -path 'a' "
              (helm-find-project-files-construct-find-option '("a")))
(assert-equal "-not -path 'a' -not -path 'b' -not -path 'c' "
              (helm-find-project-files-construct-find-option '("a" "b" "c")))

(note "Test helm-find-project-files-normalize-dir")
(assert-equal "/home/foo/"
              (helm-find-project-files-normalize-dir "/home/foo"))
(assert-equal "/home/foo/"
              (helm-find-project-files-normalize-dir "/home/foo/"))
(assert-equal (format "%s/" (getenv "HOME"))
              (helm-find-project-files-normalize-dir "~/"))

(end-tests)