;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(define install
  (lambda (module)
    (cond ((member module (list-installed-modules))
	   (display "Module is already installed: ")(display module)(newline)
	   (exit)))

    (display "installing: ")(display module)(display "...")(newline)
    (let* ((module-src-dir (path-make-absolute (string-append "src/modules/" module ) state-path))
	   (ide-dir (path-make-absolute (string-append "ide/posix/") state-path))

	   (modules-dir (path-make-absolute (string-append "rep/autoscheme-modules/")  state-path))
	   (rep-dir (string-append modules-dir module "/")  state-path)
	   (git-dir (string-append rep-dir ".git/"))

	   (clone-command (string-append "git clone https://github.com/autoscheme-modules/" module))
	   (pull-command "git fetch && git merge" module)
	   (config-command (string-append prime-path " -i configure.scm --install-path=\"" install-path "\""))
	   )


      (cond ((not (file-exists? git-dir))
	     (display "Cloning repository...")(newline)
	     (delete-file rep-dir #t #t)
	     (parameterize ((current-directory modules-dir))
			   (process-command clone-command)
			   )
	     (display "Cloning repository...DONE")(newline)
	     )
	    )

      (parameterize ((current-directory rep-dir))
		    (process-command pull-command)
		    (copy-file "src" module-src-dir #t #t)
		    )

      (parameterize ((current-directory ide-dir))
		    (process-command config-command)
		    (display (directory-files (current-directory)))(newline)
		    )

      
      )
    (display "installing: ")(display module)(display "...DONE")(newline)
    ))
  