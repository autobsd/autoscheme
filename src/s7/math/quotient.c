static inline s7_int quotient_i_7ii(s7_scheme *sc, s7_int x, s7_int y)
{
  if ((y > 0) || (y < -1)) return(x / y);
  if (y == 0)
    division_by_zero_error(sc, sc->quotient_symbol, set_elist_2(sc, wrap_integer1(sc, x), wrap_integer2(sc, y)));
  if ((y == -1) && (x == S7_INT64_MIN))   /* (quotient most-negative-fixnum -1) */
    simple_out_of_range(sc, sc->quotient_symbol, set_elist_2(sc, wrap_integer1(sc, x), wrap_integer2(sc, y)), its_too_large_string);
  return(x / y);
}

#if (!WITH_GMP)
static s7_pointer s7_truncate(s7_scheme *sc, s7_pointer caller, s7_double xf)   /* can't use "truncate" -- it's in unistd.h */
{
  if (fabs(xf) > QUOTIENT_FLOAT_LIMIT)
    return(simple_out_of_range(sc, caller, wrap_real1(sc, xf), its_too_large_string));
  return((xf > 0.0) ? make_integer(sc, (s7_int)floor(xf)) : make_integer(sc, (s7_int)ceil(xf)));
}

static s7_int c_quo_dbl(s7_scheme *sc, s7_double x, s7_double y)
{
  s7_double xf;
  if (y == 0.0)
    division_by_zero_error(sc, sc->quotient_symbol, set_elist_2(sc, wrap_real1(sc, x), wrap_real2(sc, y)));
  if ((is_inf(y)) || (is_NaN(y))) /* here we can't return NAN so I guess we should signal an error */
    wrong_type_argument_with_type(sc, sc->quotient_symbol, 2, wrap_real1(sc, y), a_normal_real_string);
  xf = x / y;
  if (fabs(xf) > QUOTIENT_FLOAT_LIMIT)
    simple_out_of_range(sc, sc->quotient_symbol, wrap_real1(sc, xf), its_too_large_string);
  return((xf > 0.0) ? (s7_int)floor(xf) : (s7_int)ceil(xf));
}
#endif

static s7_int quotient_i_ii_unchecked(s7_int i1, s7_int i2) {return(i1 / i2);} /* i2 > 0 */

static s7_pointer quotient_p_pp(s7_scheme *sc, s7_pointer x, s7_pointer y)
{
#if WITH_GMP
  if ((s7_is_real(x)) && (s7_is_real(y)))
    {
      if (s7_is_zero(y))
	division_by_zero_error(sc, sc->quotient_symbol, set_elist_2(sc, x, y));
      if ((s7_is_integer(x)) && (s7_is_integer(y)))
	{
	  if (is_t_integer(x)) mpz_set_si(sc->mpz_1, integer(x)); else mpz_set(sc->mpz_1, big_integer(x));
	  if (is_t_integer(y)) mpz_set_si(sc->mpz_2, integer(y)); else mpz_set(sc->mpz_2, big_integer(y));
	  mpz_tdiv_q(sc->mpz_1, sc->mpz_1, sc->mpz_2);
	}
      else
	if ((!is_rational(x)) || (!is_rational(y)))
	  {
	    if (any_real_to_mpfr(sc, x, sc->mpfr_1)) return(real_NaN);
	    if (any_real_to_mpfr(sc, y, sc->mpfr_2)) return(real_NaN);
	    mpfr_div(sc->mpfr_3, sc->mpfr_1, sc->mpfr_2, MPFR_RNDN);
	    mpfr_get_z(sc->mpz_1, sc->mpfr_3, MPFR_RNDZ);
	  }
	else
	  {
	    any_rational_to_mpq(sc, x, sc->mpq_1);
	    any_rational_to_mpq(sc, y, sc->mpq_2);
	    mpq_div(sc->mpq_3, sc->mpq_1, sc->mpq_2);
	    mpz_tdiv_q(sc->mpz_1, mpq_numref(sc->mpq_3), mpq_denref(sc->mpq_3));
	  }
      return(mpz_to_integer(sc, sc->mpz_1));
    }
  return(method_or_bust_pp(sc, (s7_is_real(x)) ? y : x, sc->quotient_symbol, x, y, T_REAL, (s7_is_real(x)) ? 2 : 1));
#else

  s7_int d1, d2, n1, n2;
  if ((is_t_integer(x)) && (is_t_integer(y)))
    return(make_integer(sc, quotient_i_7ii(sc, integer(x), integer(y))));

  switch (type(x))
    {
    case T_INTEGER:
      switch (type(y))
	{
	case T_INTEGER:
	  return(make_integer(sc, quotient_i_7ii(sc, integer(x), integer(y))));

	case T_RATIO:
	  n1 = integer(x);
	  d1 = 1;
	  n2 = numerator(y);
	  d2 = denominator(y);
	  /* (quotient -9223372036854775808 -1/9223372036854775807): arithmetic exception in the no-overflow-checks case */
	  goto RATIO_QUO_RATIO;

	case T_REAL:
	  if (real(y) == 0.0)
	    return(division_by_zero_error(sc, sc->quotient_symbol, set_elist_2(sc, x, y)));
	  if ((is_inf(real(y))) || (is_NaN(real(y))))
	    return(real_NaN);
	  return(s7_truncate(sc, sc->quotient_symbol, (s7_double)integer(x) / real(y))); /* s7_truncate returns an integer */

	default:
	  return(method_or_bust_pp(sc, y, sc->quotient_symbol, x, y, T_REAL, 2));
	}

    case T_RATIO:
      switch (type(y))
	{
	case T_INTEGER:
	  if (integer(y) == 0)
	    return(division_by_zero_error(sc, sc->quotient_symbol, set_elist_2(sc, x, y)));
	  n1 = numerator(x);
	  d1 = denominator(x);
	  n2 = integer(y);
	  d2 = 1;
	  goto RATIO_QUO_RATIO;
	  /* this can lose:
	   *   (quotient 1 2305843009213693952/4611686018427387903) -> 2, not 1
	   *   (quotient 21053343141/6701487259 3587785776203/1142027682075) -> 1, not 0
	   */

	case T_RATIO:
	  parcel_out_fractions(x, y);
	RATIO_QUO_RATIO:
	  if (d1 == d2)
	    return(make_integer(sc, n1 / n2));              /* (quotient 3/9223372036854775807 1/9223372036854775807) */
	  if (n1 == n2)
	    return(make_integer(sc, d2 / d1));              /* (quotient 9223372036854775807/2 9223372036854775807/8) */
#if HAVE_OVERFLOW_CHECKS
	  {
	    s7_int n1d2, n2d1;
	    if ((multiply_overflow(n1, d2, &n1d2)) ||
		(multiply_overflow(n2, d1, &n2d1)))
	      return(s7_truncate(sc, sc->quotient_symbol, ((long_double)n1 / (long_double)n2) * ((long_double)d2 / (long_double)d1)));
	    return(make_integer(sc, n1d2 / n2d1));
	  }
#else
	  return(make_integer(sc, (n1 * d2) / (n2 * d1)));
#endif

	case T_REAL:
	  if (real(y) == 0.0)
	    return(division_by_zero_error(sc, sc->quotient_symbol, set_elist_2(sc, x, y)));
	  if ((is_inf(real(y))) || (is_NaN(real(y))))
	    return(real_NaN);
	  return(s7_truncate(sc, sc->quotient_symbol, (s7_double)fraction(x) / real(y)));

	default:
	  return(method_or_bust_pp(sc, y, sc->quotient_symbol, x, y, T_REAL, 2));
	}

    case T_REAL:
      if (((is_inf(real(x))) || (is_NaN(real(x)))) && (s7_is_real(y)))
	return(real_NaN);
      /* if infs allowed we need to return infs/nans, else:
       *    (quotient inf.0 1e-309) -> -9223372036854775808
       *    (quotient inf.0 inf.0) -> -9223372036854775808
       */

      switch (type(y))
	{
	case T_INTEGER:
	  if (integer(y) == 0)
	    return(division_by_zero_error(sc, sc->quotient_symbol, set_elist_2(sc, x, y)));
	  return(s7_truncate(sc, sc->quotient_symbol, (long_double)real(x) / (long_double)integer(y)));

	case T_RATIO: return(s7_truncate(sc, sc->quotient_symbol, real(x) / (s7_double)fraction(y)));
	case T_REAL:  return(make_integer(sc, c_quo_dbl(sc, real(x), real(y)))); /* c_quo_dbl returns an integer */
	default:      return(method_or_bust_pp(sc, y, sc->quotient_symbol, x, y, T_REAL, 2));
	}

    default:
      return(method_or_bust_pp(sc, x, sc->quotient_symbol, x, y, T_REAL, 2));
    }
#endif
}

static s7_pointer g_quotient(s7_scheme *sc, s7_pointer args)
{
  #define H_quotient "(quotient x1 x2) returns the integer quotient of x1 and x2; (quotient 4 3) = 1"
  #define Q_quotient sc->pcl_r
  /* sig was '(integer? ...) but quotient can return NaN */
  /* (define (quo x1 x2) (truncate (/ x1 x2))) ; slib */
  return(quotient_p_pp(sc, car(args), cadr(args)));
}

