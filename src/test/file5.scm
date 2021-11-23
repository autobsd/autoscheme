;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

#!/Users/s.wiley/Workspace/org.autobsd/autoscheme/_/repository/ide/posix/bin/autoscheme -i
(import (auto scheme base)
	(auto scheme write)
	(scheme process-context)
	)
(display "inside file5...")(newline)
(display "command-line: ")(write (command-line))(newline)
