static s7_pointer c_acos(s7_scheme *sc, s7_double x)
{
  s7_double absx, recip;
  s7_complex result;

  absx = fabs(x);
  if (absx <= 1.0)
    return(make_real(sc, acos(x)));

  /* else follow maxima again: */
  recip = 1.0 / absx;
  if (x > 0.0)
    result = _Complex_I * clog(absx * (1.0 + (sqrt(1.0 + recip) * csqrt(1.0 - recip))));
  else result = M_PI - _Complex_I * clog(absx * (1.0 + (sqrt(1.0 + recip) * csqrt(1.0 - recip))));
  return(c_complex_to_s7(sc, result));
}

static s7_pointer acos_p_p(s7_scheme *sc, s7_pointer p)
{
  if (is_t_real(p)) return(c_acos(sc, real(p)));
  switch (type(p))
    {
    case T_INTEGER:
      return((integer(p) == 1) ? int_zero : c_acos(sc, (s7_double)integer(p)));

    case T_RATIO:
      return(c_acos(sc, fraction(p)));

    case T_COMPLEX:
#if HAVE_COMPLEX_NUMBERS
      /* if either real or imag part is very large, use explicit formula, not cacos */
      /*   this code taken from sbcl's src/code/irrat.lisp */

      if ((fabs(real_part(p)) > 1.0e7) ||
	  (fabs(imag_part(p)) > 1.0e7))
	{
	  s7_complex sq1mz, sq1pz, z;
	  z = to_c_complex(p);
	  sq1mz = csqrt(1.0 - z);
	  sq1pz = csqrt(1.0 + z);	  /* creal(sq1pz) can be 0.0 */
	  if (creal(sq1pz) == 0.0)        /* so the atan arg will be inf, so the real part will be pi/2(?) */
	    return(s7_make_complex(sc, M_PI / 2.0, asinh(cimag(sq1mz * conj(sq1pz)))));
	  return(s7_make_complex(sc, 2.0 * atan(creal(sq1mz) / creal(sq1pz)), asinh(cimag(sq1mz * conj(sq1pz)))));
	}
      return(c_complex_to_s7(sc, cacos(s7_to_c_complex(p))));
#else
      return(out_of_range(sc, sc->acos_symbol, int_one, p, no_complex_numbers_string));
#endif

#if WITH_GMP
    case T_BIG_INTEGER:
      mpfr_set_z(sc->mpfr_1, big_integer(p), MPFR_RNDN);
      goto ACOS_BIG_REAL;

    case T_BIG_RATIO:
      mpfr_set_q(sc->mpfr_1, big_ratio(p), MPFR_RNDN);
      goto ACOS_BIG_REAL;

    case T_BIG_REAL:
      if (mpfr_inf_p(big_real(p)))
	{
	  if (mpfr_cmp_ui(big_real(p), 0) < 0)
	    return(make_complex_unchecked(sc, -NAN, -INFINITY)); /* match non-bignum choice */
	  return(make_complex_unchecked(sc, -NAN, INFINITY));
	}
      mpfr_set(sc->mpfr_1, big_real(p), MPFR_RNDN);
    ACOS_BIG_REAL:
      mpfr_set_ui(sc->mpfr_2, 1, MPFR_RNDN);
      if (mpfr_cmpabs(sc->mpfr_1, sc->mpfr_2) <= 0)
	{
	  mpfr_acos(sc->mpfr_1, sc->mpfr_1, MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	}
      mpc_set_fr(sc->mpc_1, sc->mpfr_1, MPC_RNDNN);
      mpc_acos(sc->mpc_1, sc->mpc_1, MPC_RNDNN);
      return(mpc_to_number(sc, sc->mpc_1));

    case T_BIG_COMPLEX:
      mpc_acos(sc->mpc_1, big_complex(p), MPC_RNDNN);
      return(mpc_to_number(sc, sc->mpc_1));
#endif
    default:
      return(method_or_bust_with_type_one_arg_p(sc, p, sc->acos_symbol, a_number_string));
    }
}

static s7_pointer g_acos(s7_scheme *sc, s7_pointer args)
{
  #define H_acos "(acos z) returns acos(z); (cos (acos 1)) = 1"
  #define Q_acos sc->pl_nn
  return(acos_p_p(sc, car(args)));
}

