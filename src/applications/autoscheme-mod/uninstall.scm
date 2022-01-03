;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2022 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(define uninstall
  (lambda (module)
    (cond ((not (member module (list-installed-modules)))
	   (display "Module is not installed: ")(display module)(newline)
	   (exit)))

    (display "uninstalling?: ")(display module)(display "...")(newline)
    (let* ((modules-src-dir (path-make-absolute "src/modules/" state-path))
    	   (ide-dir (path-make-absolute "ide/posix/" state-path))

    	   (config-command (string-append prime-path " -i configure.scm --install-path=\"" install-path "\""))
    	   (build-library-command (string-append "make -f gen/Makefile libexec_dir=" install-path "/libexec" " lib/libautoscheme.a"))
    	   (build-application-command (string-append "make -f gen/Makefile libexec_dir=" install-path "/libexec" " bin/autoscheme"))
    	   (install-library-command (string-append "make -f gen/Makefile install_path=" install-path  " install_libautoscheme"))
    	   (install-application-command (string-append "make -f gen/Makefile install_path=" install-path  " install_autoscheme"))
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
    		    (if (not (zero? (process-command install-library-command)))
    			(error "Install error - unable to install library"))
    		    (if (not (zero? (process-command install-application-command)))
    			(error "Install error - unable to install application"))
    		    (display (directory-files (current-directory)))(newline)
    		    )

      (copy-file (path-make-absolute "ide/posix/gen/lock.s" state-path) lock-path #t #t)

      )
    (display "uninstalling: ")(display module)(display "...DONE")(newline)
    ))
  