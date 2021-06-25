#if WITH_GMP
static s7_pointer big_mod_or_rem(s7_scheme *sc, s7_pointer x, s7_pointer y, bool use_floor)
{
  if ((s7_is_real(x)) && (s7_is_real(y)))
    {
      if ((s7_is_integer(x)) && (s7_is_integer(y)))
	{
	  if (is_t_integer(x)) mpz_set_si(sc->mpz_1, integer(x)); else mpz_set(sc->mpz_1, big_integer(x));
	  if (is_t_integer(y)) mpz_set_si(sc->mpz_2, integer(y)); else mpz_set(sc->mpz_2, big_integer(y));
	  if (use_floor)
	    mpz_fdiv_q(sc->mpz_3, sc->mpz_1, sc->mpz_2);
	  else mpz_tdiv_q(sc->mpz_3, sc->mpz_1, sc->mpz_2);
	  mpz_mul(sc->mpz_3, sc->mpz_3, sc->mpz_2);
	  mpz_sub(sc->mpz_1, sc->mpz_1, sc->mpz_3);
	  return(mpz_to_integer(sc, sc->mpz_1));
	}
      if ((!is_rational(x)) || (!is_rational(y)))
	{
	  any_real_to_mpfr(sc, x, sc->mpfr_1);
	  any_real_to_mpfr(sc, y, sc->mpfr_2);
	  mpfr_div(sc->mpfr_3, sc->mpfr_1, sc->mpfr_2, MPFR_RNDN);
	  if (use_floor)
	    mpfr_get_z(sc->mpz_1, sc->mpfr_3, MPFR_RNDD);
	  else mpfr_get_z(sc->mpz_1, sc->mpfr_3, MPFR_RNDZ);
	  mpfr_mul_z(sc->mpfr_2, sc->mpfr_2, sc->mpz_1, MPFR_RNDN);
	  mpfr_sub(sc->mpfr_1, sc->mpfr_1, sc->mpfr_2, MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	}
      any_rational_to_mpq(sc, x, sc->mpq_1);
      any_rational_to_mpq(sc, y, sc->mpq_2);
      mpq_div(sc->mpq_3, sc->mpq_1, sc->mpq_2);
      if (use_floor)
	mpz_fdiv_q(sc->mpz_1, mpq_numref(sc->mpq_3), mpq_denref(sc->mpq_3));
      else mpz_tdiv_q(sc->mpz_1, mpq_numref(sc->mpq_3), mpq_denref(sc->mpq_3));
      mpz_mul(mpq_numref(sc->mpq_2), sc->mpz_1, mpq_numref(sc->mpq_2));
      mpq_sub(sc->mpq_1, sc->mpq_1, sc->mpq_2);
      return(mpq_to_canonicalized_rational(sc, sc->mpq_1));
    }
  return(method_or_bust_pp(sc, (s7_is_real(x)) ? y : x, (use_floor) ? sc->modulo_symbol : sc->remainder_symbol, x, y, T_REAL, (s7_is_real(x)) ? 2 : 1));
}
#endif

#define REMAINDER_FLOAT_LIMIT 1e13

static inline s7_int remainder_i_7ii(s7_scheme *sc, s7_int x, s7_int y)
{
  if ((y > 1) || (y < -1)) return(x % y);
  if (y == 0)
    division_by_zero_error(sc, sc->remainder_symbol, set_elist_2(sc, wrap_integer1(sc, x), wrap_integer2(sc, y)));
  return(0);
}

static s7_double c_rem_dbl(s7_scheme *sc, s7_double x, s7_double y)
{
  s7_int quo;
  s7_double pre_quo;
  if ((is_inf(y)) || (is_NaN(y)))
    return(NAN);
  pre_quo = x / y;
  if (fabs(pre_quo) > REMAINDER_FLOAT_LIMIT)
    simple_out_of_range(sc, sc->remainder_symbol, set_elist_2(sc, wrap_real1(sc, x), wrap_real2(sc, y)), its_too_large_string);
  quo = (pre_quo > 0.0) ? (s7_int)floor(pre_quo) : (s7_int)ceil(pre_quo);
  return(x - (y * quo));
}

static s7_int remainder_i_ii_unchecked(s7_int i1, s7_int i2) {return(i1 % i2);} /* i2 > 1 */
static s7_double remainder_d_7dd(s7_scheme *sc, s7_double x1, s7_double x2)
{
  if (x2 == 0.0)
    division_by_zero_error(sc, sc->remainder_symbol, set_elist_2(sc, wrap_real1(sc, x1), wrap_real2(sc, x2)));
  if ((is_inf(x1)) || (is_NaN(x1))) /* match remainder_p_pp */
    return(NAN);
  return(c_rem_dbl(sc, x1, x2));
}

static s7_pointer remainder_p_pp(s7_scheme *sc, s7_pointer x, s7_pointer y)
{
#if WITH_GMP
  if (s7_is_zero(y))
    division_by_zero_error(sc, sc->remainder_symbol, set_elist_2(sc, x, y));
  return(big_mod_or_rem(sc, x, y, false));
#else
  s7_int quo, d1, d2, n1, n2;
  s7_double pre_quo;

  if ((is_t_integer(x)) && (is_t_integer(y)))
    return(make_integer(sc, remainder_i_7ii(sc, integer(x), integer(y))));

  switch (type(x))
    {
    case T_INTEGER:
      switch (type(y))
	{
	case T_INTEGER:
	  return(make_integer(sc, remainder_i_7ii(sc, integer(x), integer(y))));

	case T_RATIO:
	  n1 = integer(x);
	  d1 = 1;
	  n2 = numerator(y);
	  d2 = denominator(y);
	  goto RATIO_REM_RATIO;

	case T_REAL:
	  if (real(y) == 0.0)
	    return(division_by_zero_error(sc, sc->remainder_symbol, set_elist_2(sc, x, y)));
	  if ((is_inf(real(y))) || (is_NaN(real(y))))
	    return(real_NaN);
	  pre_quo = (long_double)integer(x) / (long_double)real(y);
	  if (fabs(pre_quo) > REMAINDER_FLOAT_LIMIT)
	    return(simple_out_of_range(sc, sc->remainder_symbol, set_elist_2(sc, x, y), its_too_large_string));
	  if (pre_quo > 0.0) quo = (s7_int)floor(pre_quo); else quo = (s7_int)ceil(pre_quo);
	  return(make_real(sc, integer(x) - real(y) * quo));

	default:
	  return(method_or_bust_pp(sc, y, sc->remainder_symbol, x, y, T_REAL, 2));
	}

    case T_RATIO:
      switch (type(y))
	{
	case T_INTEGER:
	  n2 = integer(y);
 	  if (n2 == 0)
 	    return(division_by_zero_error(sc, sc->remainder_symbol, set_elist_2(sc, x, y)));
	  n1 = numerator(x);
	  d1 = denominator(x);
	  d2 = 1;
	  goto RATIO_REM_RATIO;

	case T_RATIO:
	  parcel_out_fractions(x, y);
	RATIO_REM_RATIO:
	  if (d1 == d2)
	    quo = (s7_int)(n1 / n2);
	  else
	    {
	      if (n1 == n2)
		quo = (s7_int)(d2 / d1);
	      else
		{
#if HAVE_OVERFLOW_CHECKS
		  s7_int n1d2, n2d1;
		  if ((multiply_overflow(n1, d2, &n1d2)) ||
		      (multiply_overflow(n2, d1, &n2d1)))
		    {
		      pre_quo = ((long_double)n1 / (long_double)n2) * ((long_double)d2 / (long_double)d1);
		      if (fabs(pre_quo) > REMAINDER_FLOAT_LIMIT)
			return(simple_out_of_range(sc, sc->remainder_symbol, set_elist_2(sc, x, y), its_too_large_string));
		      if (pre_quo > 0.0) quo = (s7_int)floor(pre_quo); else quo = (s7_int)ceil(pre_quo);
		    }
		  else quo = n1d2 / n2d1;
#else
		  quo = (n1 * d2) / (n2 * d1);
#endif
		}}
	  if (quo == 0)
	    return(x);

#if HAVE_OVERFLOW_CHECKS
	  {
	    s7_int dn, nq;
	    if (!multiply_overflow(n2, quo, &nq))
	      {
		if ((d1 == d2) &&
		    (!subtract_overflow(n1, nq, &dn)))
		  return(s7_make_ratio(sc, dn, d1));

		if ((!multiply_overflow(n1, d2, &dn)) &&
		    (!multiply_overflow(nq, d1, &nq)) &&
		    (!subtract_overflow(dn, nq, &nq)) &&
		    (!multiply_overflow(d1, d2, &d1)))
		  return(s7_make_ratio(sc, nq, d1));
	      }}
#else
	  if (d1 == d2)
	    return(s7_make_ratio(sc, n1 - n2 * quo, d1));

	  return(s7_make_ratio(sc, n1 * d2 - n2 * d1 * quo, d1 * d2));
#endif
	  return(simple_out_of_range(sc, sc->remainder_symbol, set_elist_2(sc, x, y), wrap_string(sc, "intermediate (a/b) is too large", 31)));

	case T_REAL:
	  {
	    s7_double frac;
	    if (real(y) == 0.0)
	      return(division_by_zero_error(sc, sc->remainder_symbol, set_elist_2(sc, x, y)));
	    if ((is_inf(real(y))) || (is_NaN(real(y))))
	      return(real_NaN);
	    if (s7_int_abs(numerator(x)) > QUOTIENT_INT_LIMIT)
	      return(subtract_p_pp(sc, x, multiply_p_pp(sc, y, quotient_p_pp(sc, x, y))));
	    frac = (s7_double)fraction(x);
	    pre_quo = frac / real(y);
	    if (fabs(pre_quo) > REMAINDER_FLOAT_LIMIT)
	      return(simple_out_of_range(sc, sc->remainder_symbol, set_elist_2(sc, x, y), its_too_large_string));
	    if (pre_quo > 0.0) quo = (s7_int)floor(pre_quo); else quo = (s7_int)ceil(pre_quo);
	    return(make_real(sc, frac - real(y) * quo));
	  }

	default:
	  return(method_or_bust_pp(sc, y, sc->remainder_symbol, x, y, T_REAL, 2));
	}

    case T_REAL:
      if (((is_inf(real(x))) || (is_NaN(real(x)))) && (s7_is_real(y)))
	{
	  if (s7_is_zero(y))
	    return(division_by_zero_error(sc, sc->remainder_symbol, set_elist_2(sc, x, y)));
	  return(real_NaN);
	}
      switch (type(y))
	{
	case T_INTEGER:
	  if (integer(y) == 0)
	    return(division_by_zero_error(sc, sc->remainder_symbol, set_elist_2(sc, x, y)));
	  /* actually here (and elsewhere) if y > INT64_TO_DOUBLE_LIMIT, the result is probably wrong */
	  pre_quo = (long_double)real(x) / (long_double)integer(y);
	  if (fabs(pre_quo) > REMAINDER_FLOAT_LIMIT)
	    return(simple_out_of_range(sc, sc->remainder_symbol, set_elist_2(sc, x, y), its_too_large_string));
	  if (pre_quo > 0.0) quo = (s7_int)floor(pre_quo); else quo = (s7_int)ceil(pre_quo);
	  return(make_real(sc, real(x) - integer(y) * quo));
	  /* but... (remainder 1e+18 9223372036854775807) -> 1e+18 */

	case T_RATIO:
	  if (s7_int_abs(numerator(y)) > QUOTIENT_INT_LIMIT)
	    return(subtract_p_pp(sc, x, multiply_p_pp(sc, y, quotient_p_pp(sc, x, y))));
	  {
	    s7_double frac;
	    frac = (s7_double)fraction(y);
	    pre_quo = real(x) / frac;
	    if (fabs(pre_quo) > REMAINDER_FLOAT_LIMIT)
	      return(simple_out_of_range(sc, sc->remainder_symbol, set_elist_2(sc, x, y), its_too_large_string));
	    if (pre_quo > 0.0) quo = (s7_int)floor(pre_quo); else quo = (s7_int)ceil(pre_quo);
	    return(make_real(sc, real(x) - frac * quo));
	  }

	case T_REAL:
	  if (real(y) == 0.0)
	    return(division_by_zero_error(sc, sc->remainder_symbol, set_elist_2(sc, x, y)));
	  return(make_real(sc, c_rem_dbl(sc, real(x), real(y))));
	  /* see under sin -- this calculation is completely bogus if "a" is large
	   * (quotient 1e22 (* 2 pi)) -> -9223372036854775808 but it should be 1591549430918953357688,
	   * (remainder 1e22 (* 2 pi)) -> 1.0057952155665e+22 -- the "remainder" is greater than the original argument!
	   * Clisp gives 0.0 here, as does sbcl, currently s7 throws an error (out-of-range).
	   */

	default:
	  return(method_or_bust_pp(sc, y, sc->remainder_symbol, x, y, T_REAL, 2));
	}

    default:
      return(method_or_bust_pp(sc, x, sc->remainder_symbol, x, y, T_REAL, 1));
    }
#endif
}

static s7_pointer g_remainder(s7_scheme *sc, s7_pointer args)
{
  #define H_remainder "(remainder x1 x2) returns the remainder of x1/x2; (remainder 10 3) = 1"
  #define Q_remainder sc->pcl_r
  /* (define (rem x1 x2) (- x1 (* x2 (quo x1 x2)))) ; slib, if x2 is an integer (- x1 (truncate x1 x2)), fractional part: (remainder x 1) */

  s7_pointer x, y;
  x = car(args);
  y = cadr(args);
  if ((is_t_integer(x)) && (is_t_integer(y)))
    return(make_integer(sc, remainder_i_7ii(sc, integer(x), integer(y))));
  return(remainder_p_pp(sc, x, y));
}

