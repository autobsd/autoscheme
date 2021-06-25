static s7_pointer c_asin(s7_scheme *sc, s7_double x)
{
  s7_double absx, recip;
  s7_complex result;

  absx = fabs(x);
  if (absx <= 1.0)
    return(make_real(sc, asin(x)));

  /* otherwise use maxima code: */
  recip = 1.0 / absx;
  result = (M_PI / 2.0) - (_Complex_I * clog(absx * (1.0 + (sqrt(1.0 + recip) * csqrt(1.0 - recip)))));
  return((x < 0.0) ? c_complex_to_s7(sc, -result) : c_complex_to_s7(sc, result));
}

static s7_pointer asin_p_p(s7_scheme *sc, s7_pointer p)
{
  if (is_t_real(p)) return(c_asin(sc, real(p)));
  switch (type(p))
    {
    case T_INTEGER:
      if (integer(p) == 0) return(int_zero);                    /* (asin 0) -> 0 */
      /* in netBSD, (asin 2) returns 0.25383842987008+0.25383842987008i according to Peter Bex */
      return(c_asin(sc, (s7_double)integer(p)));

    case T_RATIO:
      return(c_asin(sc, fraction(p)));

    case T_COMPLEX:
#if HAVE_COMPLEX_NUMBERS
      /* if either real or imag part is very large, use explicit formula, not casin */
      /*   this code taken from sbcl's src/code/irrat.lisp; break is around x+70000000i */
      if ((fabs(real_part(p)) > 1.0e7) ||
	  (fabs(imag_part(p)) > 1.0e7))
	{
	  s7_complex sq1mz, sq1pz, z;
	  z = to_c_complex(p);
	  sq1mz = csqrt(1.0 - z);
	  sq1pz = csqrt(1.0 + z);
	  return(s7_make_complex(sc, atan(real_part(p) / creal(sq1mz * sq1pz)), asinh(cimag(sq1pz * conj(sq1mz)))));
	}
      return(c_complex_to_s7(sc, casin(to_c_complex(p))));
#else
      return(out_of_range(sc, sc->asin_symbol, int_one, p, no_complex_numbers_string));
#endif

#if WITH_GMP
    case T_BIG_INTEGER:
      mpfr_set_z(sc->mpfr_1, big_integer(p), MPFR_RNDN);
      goto ASIN_BIG_REAL;

    case T_BIG_RATIO:
      mpfr_set_q(sc->mpfr_1, big_ratio(p), MPFR_RNDN);
      goto ASIN_BIG_REAL;

    case T_BIG_REAL:
      if (mpfr_inf_p(big_real(p)))
	{
	  if (mpfr_cmp_ui(big_real(p), 0) < 0)
	    return(make_complex_unchecked(sc, NAN, INFINITY)); /* match non-bignum choice */
	  return(make_complex_unchecked(sc, NAN, -INFINITY));
	}
      mpfr_set(sc->mpfr_1, big_real(p), MPFR_RNDN);
    ASIN_BIG_REAL:
      mpfr_set_ui(sc->mpfr_2, 1, MPFR_RNDN);
      if (mpfr_cmpabs(sc->mpfr_1, sc->mpfr_2) <= 0)
	{
	  mpfr_asin(sc->mpfr_1, sc->mpfr_1, MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	}
      mpc_set_fr(sc->mpc_1, sc->mpfr_1, MPC_RNDNN);
      mpc_asin(sc->mpc_1, sc->mpc_1, MPC_RNDNN);
      return(mpc_to_number(sc, sc->mpc_1));

    case T_BIG_COMPLEX:
      mpc_asin(sc->mpc_1, big_complex(p), MPC_RNDNN);
      return(mpc_to_number(sc, sc->mpc_1));
#endif
    default:
      return(method_or_bust_with_type_one_arg_p(sc, p, sc->asin_symbol, a_number_string));
    }
}

static s7_pointer g_asin(s7_scheme *sc, s7_pointer args)
{
  #define H_asin "(asin z) returns asin(z); (sin (asin x)) = x"
  #define Q_asin sc->pl_nn
  return(asin_p_p(sc, car(args)));
}

