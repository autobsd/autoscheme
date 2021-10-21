;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(foreign-initialize "pointer library = make_environment( NIL );\n"
		    "pointer name = mk_symbol( \"(scheme cxr)\" );\n"

		    "car( environment ) = cons( cons( name, library ), car( environment ));\n"

		    "scheme_register_operation(LOC_CAAAR, \"caaar\", library);\n"
		    "scheme_register_operation(LOC_CAADR, \"caadr\", library);\n"
		    "scheme_register_operation(LOC_CADAR, \"cadar\", library);\n"
		    "scheme_register_operation(LOC_CADDR, \"caddr\", library);\n"
		    "scheme_register_operation(LOC_CDAAR, \"cdaar\", library);\n"
		    "scheme_register_operation(LOC_CDADR, \"cdadr\", library);\n"
		    "scheme_register_operation(LOC_CDDAR, \"cddar\", library);\n"
		    "scheme_register_operation(LOC_CDDDR, \"cdddr\", library);\n"
		    "scheme_register_operation(LOC_CAAAAR, \"caaaar\", library);\n"
		    "scheme_register_operation(LOC_CAAADR, \"caaadr\", library);\n"
		    "scheme_register_operation(LOC_CAADAR, \"caadar\", library);\n"
		    "scheme_register_operation(LOC_CAADDR, \"caaddr\", library);\n"
		    "scheme_register_operation(LOC_CADAAR, \"cadaar\", library);\n"
		    "scheme_register_operation(LOC_CADADR, \"cadadr\", library);\n"
		    "scheme_register_operation(LOC_CADDAR, \"caddar\", library);\n"
		    "scheme_register_operation(LOC_CADDDR, \"cadddr\", library);\n"
		    "scheme_register_operation(LOC_CDAAAR, \"cdaaar\", library);\n"
		    "scheme_register_operation(LOC_CDAADR, \"cdaadr\", library);\n"
		    "scheme_register_operation(LOC_CDADAR, \"cdadar\", library);\n"
		    "scheme_register_operation(LOC_CDADDR, \"cdaddr\", library);\n"
		    "scheme_register_operation(LOC_CDDAAR, \"cddaar\", library);\n"
		    "scheme_register_operation(LOC_CDDADR, \"cddadr\", library);\n"
		    "scheme_register_operation(LOC_CDDDAR, \"cdddar\", library);\n"
		    "scheme_register_operation(LOC_CDDDDR, \"cddddr\", library);\n"
		    )


