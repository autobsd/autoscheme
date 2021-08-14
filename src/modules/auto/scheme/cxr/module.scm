;; (scheme cxr)
(define-library (auto scheme cxr)
  (export caaaar
	  caaar
	  caaddr
	  cadaar
	  cadar
	  cadddr
	  cdaaar
	  cdaar
	  cdaddr
	  cddaar
	  cddar
	  cddddr

	  caaadr
	  caadar
	  caadr
	  cadadr
	  caddar
	  caddr
	  cdaadr
	  cdadar
	  cdadr
	  cddadr
	  cdddar
	  cdddr
	  )
  (import (only (s7)
		caaaar
		caaar
		caaddr
		cadaar
		cadar
		cadddr
		cdaaar
		cdaar
		cdaddr
		cddaar
		cddar
		cddddr

		caaadr
		caadar
		caadr
		cadadr
		caddar
		caddr
		cdaadr
		cdadar
		cdadr
		cddadr
		cdddar
		cdddr
		)
	  )
  )