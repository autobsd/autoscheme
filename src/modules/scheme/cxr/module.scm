;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(foreign-initialize "pointer library = make_environment( NIL );\n"
		    "pointer name = mk_symbol( \"(auto scheme cxr)\" );\n"

		    "car( environment ) = cons( cons( name, library ), car( environment ));\n"

		    "scheme_register_proc(OP_CAAAR, \"caaar\", library);\n"
		    "scheme_register_proc(OP_CAADR, \"caadr\", library);\n"
		    "scheme_register_proc(OP_CADAR, \"cadar\", library);\n"
		    "scheme_register_proc(OP_CADDR, \"caddr\", library);\n"
		    "scheme_register_proc(OP_CDAAR, \"cdaar\", library);\n"
		    "scheme_register_proc(OP_CDADR, \"cdadr\", library);\n"
		    "scheme_register_proc(OP_CDDAR, \"cddar\", library);\n"
		    "scheme_register_proc(OP_CDDDR, \"cdddr\", library);\n"
		    "scheme_register_proc(OP_CAAAAR, \"caaaar\", library);\n"
		    "scheme_register_proc(OP_CAAADR, \"caaadr\", library);\n"
		    "scheme_register_proc(OP_CAADAR, \"caadar\", library);\n"
		    "scheme_register_proc(OP_CAADDR, \"caaddr\", library);\n"
		    "scheme_register_proc(OP_CADAAR, \"cadaar\", library);\n"
		    "scheme_register_proc(OP_CADADR, \"cadadr\", library);\n"
		    "scheme_register_proc(OP_CADDAR, \"caddar\", library);\n"
		    "scheme_register_proc(OP_CADDDR, \"cadddr\", library);\n"
		    "scheme_register_proc(OP_CDAAAR, \"cdaaar\", library);\n"
		    "scheme_register_proc(OP_CDAADR, \"cdaadr\", library);\n"
		    "scheme_register_proc(OP_CDADAR, \"cdadar\", library);\n"
		    "scheme_register_proc(OP_CDADDR, \"cdaddr\", library);\n"
		    "scheme_register_proc(OP_CDDAAR, \"cddaar\", library);\n"
		    "scheme_register_proc(OP_CDDADR, \"cddadr\", library);\n"
		    "scheme_register_proc(OP_CDDDAR, \"cdddar\", library);\n"
		    "scheme_register_proc(OP_CDDDDR, \"cddddr\", library);\n"
		    )


