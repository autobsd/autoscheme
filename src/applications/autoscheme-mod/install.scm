;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2022 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(define rebuild-project
  (lambda ()
    (display "rebuilding project...")(newline)
    (let* ((ide-dir (path-make-absolute "ide/posix/" state-path))

	   (config-command (string-append prime-path " -i configure.scm --install-path=\"" install-path "\""))
	   (build-library-command (string-append "make -f gen/Makefile libexec_dir=" install-path "/libexec" " lib/libautoscheme.a"))
	   (build-application-command (string-append "make -f gen/Makefile libexec_dir=" install-path "/libexec" " bin/autoscheme"))
	   )

      (parameterize ((current-directory ide-dir))
		    (if (not (zero? (process-command config-command)))
			(error "Build error - unable to configure project"))
		    (if (not (zero? (process-command build-library-command)))
			(error "Build error - unable to make library"))
		    (if (not (zero? (process-command build-application-command)))
			(error "Build error - unable to make application"))
		    )
      )
    (display "rebuilding project...DONE")(newline)
    ))

(define reinstall-project
  (lambda ()
    (display "reinstalling project...")(newline)
    (let ((ide-dir (path-make-absolute "ide/posix/" state-path))

	  (install-library-command (string-append "make -f gen/Makefile install_path=" install-path  " install_libautoscheme"))
	  (install-application-command (string-append "make -f gen/Makefile install_path=" install-path  " install_autoscheme"))
	  )

      (parameterize ((current-directory ide-dir))
		    (if (not (zero? (process-command install-library-command)))
			(error "Install error - unable to install library"))
		    (if (not (zero? (process-command install-application-command)))
			(error "Install error - unable to install application"))
		    (display (directory-files (current-directory)))(newline)
		    )

      (copy-file (path-make-absolute "ide/posix/gen/lock.s" state-path) lock-path #t #t)

      )
    (display "reinstalling project...DONE")(newline)
    ))
  
(define install-modules
  (lambda (modules)
    (let ((modified #f))
      (for-each (lambda (module)
		  (cond ((member module (list-installed-modules))
			 (display "Module is already installed: ")(display module)(newline))

			(else (set! modified #t)
			      
			      (display "installing: ")(display module)(display "...")(newline)

			      (let* ((module-src-dir (path-make-absolute (string-append "src/modules/" module ) state-path))
				     (ide-dir (path-make-absolute "ide/posix/" state-path))

				     (modules-dir (path-make-absolute (string-append "rep/autoscheme-modules/")  state-path))
				     (rep-dir (string-append modules-dir module "/")  state-path)
				     (git-dir (string-append rep-dir ".git/"))

				     (clone-command (string-append "git clone https://github.com/autoscheme-modules/" module))
				     (pull-command "git fetch && git merge" module))

				(cond ((not (file-exists? git-dir))
				       (display "cloning repository...")(newline)
				       (delete-file rep-dir #t #t)
				       (parameterize ((current-directory modules-dir))
						     (process-command clone-command)
						     )
				       (display "cloning repository...DONE")(newline) ))

				(parameterize ((current-directory rep-dir))
					      (if (not (zero? (process-command pull-command)))
						  (error "Repository error - unable to pull sources for module" (string->symbol module)))
					      (copy-file "src" module-src-dir #t #t) ))

			      (display "installing: ")(display module)(display "...DONE")(newline) 
			      )))
		modules)

      (cond (modified (rebuild-project) (reinstall-project)))

      )))
  