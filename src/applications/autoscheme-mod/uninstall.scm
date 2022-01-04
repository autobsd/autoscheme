;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2022 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(define uninstall-module
  (lambda (module)
    (cond ((not (member module (list-installed-modules)))
	   (display "Module is not installed: ")(display module)(newline)
	   (exit)))

    (display "uninstalling: ")(display module)(display "...")(newline)
    (let* ((modules-src-dir (path-make-absolute "src/modules/" state-path))
    	   (ide-dir (path-make-absolute "ide/posix/" state-path))

    	   (config-command (string-append prime-path " -i configure.scm --install-path=\"" install-path "\""))
    	   (build-library-command (string-append "make -f gen/Makefile libexec_dir=" install-path "/libexec" " lib/libautoscheme.a"))
    	   (build-application-command (string-append "make -f gen/Makefile libexec_dir=" install-path "/libexec" " bin/autoscheme"))
    	   )

      (parameterize ((current-directory modules-src-dir))
		    (delete-file module #t #t)
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
    (install-project)
    (display "uninstalling: ")(display module)(display "...DONE")(newline)
    ))
  