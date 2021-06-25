static s7_pointer sqrt_p_p(s7_scheme *sc, s7_pointer p)
{
  switch (type(p))
    {
    case T_INTEGER:
      {
	s7_double sqx;
	if (integer(p) >= 0)
	  {
	    s7_int ix;
#if WITH_GMP
	    mpz_set_si(sc->mpz_1, integer(p));
	    mpz_sqrtrem(sc->mpz_1, sc->mpz_2, sc->mpz_1);
	    if (mpz_cmp_ui(sc->mpz_2, 0) == 0)
	      return(make_integer(sc, mpz_get_si(sc->mpz_1)));
	    mpfr_set_si(sc->mpfr_1, integer(p), MPFR_RNDN);
	    mpfr_sqrt(sc->mpfr_1, sc->mpfr_1, MPFR_RNDN);
	    return(mpfr_to_big_real(sc, sc->mpfr_1));
#endif
	    sqx = sqrt((s7_double)integer(p));
	    ix = (s7_int)sqx;
	    return(((ix * ix) == integer(p)) ? make_integer(sc, ix) : make_real(sc, sqx));
	    /* Mark Weaver notes that (zero? (- (sqrt 9007199136250226) 94906265.0)) -> #t
	     * but (* 94906265 94906265) -> 9007199136250225 -- oops
	     * if we use bigfloats, we're ok:
	     *    (* (sqrt 9007199136250226.0) (sqrt 9007199136250226.0)) -> 9.007199136250226000000000000000000000026E15
	     * at least we return a real here, not an incorrect integer and (sqrt 9007199136250225) -> 94906265
	     */
	  }
#if HAVE_COMPLEX_NUMBERS
#if WITH_GMP
	mpc_set_si(sc->mpc_1, integer(p), MPC_RNDNN);
	mpc_sqrt(sc->mpc_1, sc->mpc_1, MPC_RNDNN);
	return(mpc_to_number(sc, sc->mpc_1));
#endif
	sqx = (s7_double)integer(p); /* we're trying to protect against (sqrt -9223372036854775808) where we can't negate the integer argument */
	return(s7_make_complex(sc, 0.0, sqrt((s7_double)(-sqx))));
#else
	return(out_of_range(sc, sc->sqrt_symbol, int_one, p, no_complex_numbers_string));
#endif
      }

    case T_RATIO:
      if (numerator(p) > 0) /* else it's complex, so it can't be a ratio */
	{
	  s7_int nm;
	  nm = (s7_int)sqrt(numerator(p));
	  if (nm * nm == numerator(p))
	    {
	      s7_int dn;
	      dn = (s7_int)sqrt(denominator(p));
	      if (dn * dn == denominator(p))
		return(s7_make_ratio(sc, nm, dn));
	    }
	  return(make_real(sc, sqrt((s7_double)fraction(p))));
	}
#if HAVE_COMPLEX_NUMBERS
      return(s7_make_complex(sc, 0.0, sqrt((s7_double)(-fraction(p)))));
#else
      return(out_of_range(sc, sc->sqrt_symbol, int_one, p, no_complex_numbers_string));
#endif

    case T_REAL:
      if (is_NaN(real(p)))
	return(real_NaN);
      if (real(p) >= 0.0)
	return(make_real(sc, sqrt(real(p))));
      return(s7_make_complex(sc, 0.0, sqrt(-real(p))));

    case T_COMPLEX:    /* (* inf.0 (sqrt -1)) -> -nan+infi, but (sqrt -inf.0) -> 0+infi */
#if HAVE_COMPLEX_NUMBERS
      return(c_complex_to_s7(sc, csqrt(to_c_complex(p)))); /* sqrt(+inf.0+1.0i) -> +inf.0 */
#else
      return(out_of_range(sc, sc->sqrt_symbol, int_one, p, no_complex_numbers_string));
#endif

#if WITH_GMP
    case T_BIG_INTEGER:
      if (mpz_cmp_ui(big_integer(p), 0) >= 0)
	{
	  mpz_sqrtrem(sc->mpz_1, sc->mpz_2, big_integer(p));
	  if (mpz_cmp_ui(sc->mpz_2, 0) == 0)
	    return(mpz_to_integer(sc, sc->mpz_1));
	  mpfr_set_z(sc->mpfr_1, big_integer(p), MPFR_RNDN);
	  mpfr_sqrt(sc->mpfr_1, sc->mpfr_1, MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	}
      mpc_set_z(sc->mpc_1, big_integer(p), MPC_RNDNN);
      mpc_sqrt(sc->mpc_1, sc->mpc_1, MPC_RNDNN);
      return(mpc_to_number(sc, sc->mpc_1));

    case T_BIG_RATIO: /* if big ratio, check both num and den for squares */
      if (mpq_cmp_ui(big_ratio(p), 0, 1) < 0)
	{
	  mpc_set_q(sc->mpc_1, big_ratio(p), MPC_RNDNN);
	  mpc_sqrt(sc->mpc_1, sc->mpc_1, MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	}
      mpz_sqrtrem(sc->mpz_1, sc->mpz_2, mpq_numref(big_ratio(p)));
      if (mpz_cmp_ui(sc->mpz_2, 0) == 0)
	{
	  mpz_sqrtrem(sc->mpz_3, sc->mpz_2, mpq_denref(big_ratio(p)));
	  if (mpz_cmp_ui(sc->mpz_2, 0) == 0)
	    {
	      mpq_set_num(sc->mpq_1, sc->mpz_1);
	      mpq_set_den(sc->mpq_1, sc->mpz_3);
	      return(mpq_to_canonicalized_rational(sc, sc->mpq_1));
	    }}
      mpfr_set_q(sc->mpfr_1, big_ratio(p), MPFR_RNDN);
      mpfr_sqrt(sc->mpfr_1, sc->mpfr_1, MPFR_RNDN);
      return(mpfr_to_big_real(sc, sc->mpfr_1));

    case T_BIG_REAL:
      if (mpfr_cmp_ui(big_real(p), 0) < 0)
	{
	  mpc_set_fr(sc->mpc_1, big_real(p), MPC_RNDNN);
	  mpc_sqrt(sc->mpc_1, sc->mpc_1, MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	}
      mpfr_sqrt(sc->mpfr_1, big_real(p), MPFR_RNDN);
      return(mpfr_to_big_real(sc, sc->mpfr_1));

    case T_BIG_COMPLEX:
      mpc_sqrt(sc->mpc_1, big_complex(p), MPC_RNDNN);
      return(mpc_to_number(sc, sc->mpc_1));
#endif
    default:
      return(method_or_bust_with_type_one_arg_p(sc, p, sc->sqrt_symbol, a_number_string));
    }
}

static s7_pointer g_sqrt(s7_scheme *sc, s7_pointer args)
{
  #define H_sqrt "(sqrt z) returns the square root of z"
  #define Q_sqrt sc->pl_nn
  return(sqrt_p_p(sc, car(args)));
}

