static s7_pointer negate_p_p(s7_scheme *sc, s7_pointer p)     /* can't use "negate" because it confuses C++! */
{
  switch (type(p))
    {
    case T_INTEGER:
      if (integer(p) == S7_INT64_MIN)
#if WITH_GMP
	{
	  mpz_set_si(sc->mpz_1, S7_INT64_MIN);
	  mpz_neg(sc->mpz_1, sc->mpz_1);
	  return(mpz_to_big_integer(sc, sc->mpz_1));
	}
#else
	return(simple_out_of_range(sc, sc->subtract_symbol, p, wrap_string(sc, "most-negative-fixnum can't be negated", 37)));
#endif
      return(make_integer(sc, -integer(p)));

    case T_RATIO:   return(make_simple_ratio(sc, -numerator(p), denominator(p)));
    case T_REAL:    return(make_real(sc, -real(p)));
    case T_COMPLEX: return(s7_make_complex(sc, -real_part(p), -imag_part(p)));

#if WITH_GMP
    case T_BIG_INTEGER:
      mpz_neg(sc->mpz_1, big_integer(p));
      return(mpz_to_integer(sc, sc->mpz_1));
    case T_BIG_RATIO:
      mpq_neg(sc->mpq_1, big_ratio(p));
      return(mpq_to_canonicalized_rational(sc, sc->mpq_1));
    case T_BIG_REAL:
      mpfr_neg(sc->mpfr_1, big_real(p), MPFR_RNDN);
      return(mpfr_to_big_real(sc, sc->mpfr_1));
    case T_BIG_COMPLEX:
      mpc_neg(sc->mpc_1, big_complex(p), MPC_RNDNN);
      return(mpc_to_number(sc, sc->mpc_1));
#endif
    default:
      return(method_or_bust_with_type_one_arg_p(sc, p, sc->subtract_symbol, a_number_string));
    }
}

static inline s7_pointer subtract_if_overflow_to_real_or_big_integer(s7_scheme *sc, s7_int x, s7_int y)
{
#if HAVE_OVERFLOW_CHECKS
  s7_int val;
  if (subtract_overflow(x, y, &val))
#if WITH_GMP
    {
      mpz_set_si(sc->mpz_1, x);
      mpz_set_si(sc->mpz_2, y);
      mpz_sub(sc->mpz_1, sc->mpz_1, sc->mpz_2);
      return(mpz_to_big_integer(sc, sc->mpz_1));
    }
#else
  return(make_real(sc, (double)x - (double)y));
#endif
  return(make_integer(sc, val));
#else
  return(make_integer(sc, x - y));
#endif
}

static s7_pointer subtract_p_pp(s7_scheme *sc, s7_pointer x, s7_pointer y)
{
  switch (type(x))
    {
    case T_INTEGER:
      if (integer(x) == 0)
	return(negate_p_p(sc, y));
      switch (type(y))
	{
	case T_INTEGER:
	  return(subtract_if_overflow_to_real_or_big_integer(sc, integer(x), integer(y)));

	case T_RATIO:
	  {
#if HAVE_OVERFLOW_CHECKS
	    s7_int z;
	    if ((multiply_overflow(integer(x), denominator(y), &z)) ||
		(subtract_overflow(z, numerator(y), &z)))
#if WITH_GMP
	      {
		mpz_set_si(sc->mpz_1, integer(x));
		mpz_mul_si(sc->mpz_1, sc->mpz_1, denominator(y));
		mpz_set_si(sc->mpz_2, numerator(y));
		mpz_sub(mpq_numref(sc->mpq_1), sc->mpz_1, sc->mpz_2);
		mpz_set_si(mpq_denref(sc->mpq_1), denominator(y));
		return(mpq_to_rational(sc, sc->mpq_1));
	      }
#else
	      return(make_real(sc, (long_double)integer(x) - fraction(y)));
#endif
	      return(s7_make_ratio(sc, z, denominator(y)));
#else
	    return(s7_make_ratio(sc, integer(x) * denominator(y) - numerator(y), denominator(y)));
#endif
	  }
	case T_REAL:
#if WITH_GMP
	  if (s7_int_abs(integer(x)) >= INT64_TO_DOUBLE_LIMIT) /* (- 9223372036854775807 .1) */
	    {
	      mpfr_set_si(sc->mpfr_1, integer(x), MPFR_RNDN);
	      mpfr_sub_d(sc->mpfr_1, sc->mpfr_1, real(y), MPFR_RNDN);
	      return(mpfr_to_big_real(sc, sc->mpfr_1));
	    }
#endif
	  return(make_real(sc, (long_double)integer(x) - real(y)));
	case T_COMPLEX:
	  return(s7_make_complex(sc, (long_double)integer(x) - real_part(y), -imag_part(y)));
#if WITH_GMP
	case T_BIG_INTEGER:
	  mpz_set_si(sc->mpz_1, integer(x));
	  mpz_sub(sc->mpz_1, sc->mpz_1, big_integer(y));
	  return(mpz_to_integer(sc, sc->mpz_1));
	case T_BIG_RATIO:
	  mpq_set_si(sc->mpq_1, integer(x), 1);
	  mpq_sub(sc->mpq_1, sc->mpq_1, big_ratio(y));
	  return(mpq_to_rational(sc, sc->mpq_1));
	case T_BIG_REAL:
	  mpfr_si_sub(sc->mpfr_1, integer(x), big_real(y), MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	case T_BIG_COMPLEX:
	  mpc_set_si(sc->mpc_1, integer(x), MPC_RNDNN);
	  mpc_sub(sc->mpc_1, sc->mpc_1, big_complex(y), MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
#endif
	default:
	  return(method_or_bust_with_type_pp(sc, y, sc->subtract_symbol, x, y, a_number_string, 2));
	}

    case T_RATIO:
      switch (type(y))
	{
	case T_INTEGER:
	  {
#if HAVE_OVERFLOW_CHECKS
	    s7_int z;
	    if ((multiply_overflow(integer(y), denominator(x), &z)) ||
		(subtract_overflow(numerator(x), z, &z)))
#if WITH_GMP
	      {
		mpz_set_si(sc->mpz_1, integer(y));
		mpz_mul_si(sc->mpz_1, sc->mpz_1, denominator(x));
		mpz_set_si(sc->mpz_2, numerator(x));
		mpz_sub(mpq_numref(sc->mpq_1), sc->mpz_2, sc->mpz_1);
		mpz_set_si(mpq_denref(sc->mpq_1), denominator(x));
		return(mpq_to_rational(sc, sc->mpq_1));
	      }
#else
	    return(make_real(sc, fraction(x) - (long_double)integer(y)));
#endif
	    return(s7_make_ratio(sc, z, denominator(x)));
#else
	    return(s7_make_ratio(sc, numerator(x) - (integer(y) * denominator(x)), denominator(x)));
#endif
	  }
	case T_RATIO:
	  {
	    s7_int d1, d2, n1, n2;
	    parcel_out_fractions(x, y);
	    if (d1 == d2)
	      {
#if HAVE_OVERFLOW_CHECKS
		s7_int q;
		if (subtract_overflow(n1, n2, &q))
#if WITH_GMP
		  {
		    mpq_set_si(sc->mpq_1, n1, d1);
		    mpq_set_si(sc->mpq_2, n2, d2);
		    mpq_sub(sc->mpq_1, sc->mpq_1, sc->mpq_2);
		    return(mpq_to_rational(sc, sc->mpq_1));
		  }
#else
		return(make_real(sc, ((long_double)n1 - (long_double)n2) / (long_double)d1));
#endif
	        return(s7_make_ratio(sc, q, d1));
#else
		return(s7_make_ratio(sc, numerator(x) - numerator(y), denominator(x)));
#endif
	      }

#if HAVE_OVERFLOW_CHECKS
	    {
	      s7_int n1d2, n2d1, d1d2, q;
	      if ((multiply_overflow(d1, d2, &d1d2)) ||
		  (multiply_overflow(n1, d2, &n1d2)) ||
		  (multiply_overflow(n2, d1, &n2d1)) ||
		  (subtract_overflow(n1d2, n2d1, &q)))
#if WITH_GMP
		{
		  mpq_set_si(sc->mpq_1, n1, d1);
		  mpq_set_si(sc->mpq_2, n2, d2);
		  mpq_sub(sc->mpq_1, sc->mpq_1, sc->mpq_2);
		  return(mpq_to_rational(sc, sc->mpq_1));
		}
#else
	      return(make_real(sc, ((long_double)n1 / (long_double)d1) - ((long_double)n2 / (long_double)d2)));
#endif
	      return(s7_make_ratio(sc, q, d1d2));
	    }
#else
	    return(s7_make_ratio(sc, n1 * d2 - n2 * d1, d1 * d2));
#endif
	  }
	case T_REAL:
	  return(make_real(sc, fraction(x) - real(y)));
	case T_COMPLEX:
	  return(s7_make_complex(sc, fraction(x) - real_part(y), -imag_part(y)));
#if WITH_GMP
	case T_BIG_INTEGER:
	  mpq_set_si(sc->mpq_1, numerator(x), denominator(x));
	  mpq_set_z(sc->mpq_2, big_integer(y));
	  mpq_sub(sc->mpq_1, sc->mpq_1, sc->mpq_2);
	  return(mpq_to_rational(sc, sc->mpq_1));
	case T_BIG_RATIO:
	  mpq_set_si(sc->mpq_1, numerator(x), denominator(x));
	  mpq_sub(sc->mpq_1, sc->mpq_1, big_ratio(y));
	  return(mpq_to_rational(sc, sc->mpq_1));
	case T_BIG_REAL:
	  mpq_set_si(sc->mpq_1, numerator(x), denominator(x));
	  mpfr_set_q(sc->mpfr_1, sc->mpq_1, MPFR_RNDN);
	  mpfr_sub(sc->mpfr_1, sc->mpfr_1, big_real(y), MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	case T_BIG_COMPLEX:
	  mpq_set_si(sc->mpq_1, numerator(x), denominator(x));
	  mpc_set_q(sc->mpc_1, sc->mpq_1, MPC_RNDNN);
	  mpc_sub(sc->mpc_1, sc->mpc_1, big_complex(y), MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
#endif
	default:
	  return(method_or_bust_with_type_pp(sc, y, sc->subtract_symbol, x, y, a_number_string, 2));
	}

    case T_REAL:
      switch (type(y))
	{
	case T_INTEGER:
#if WITH_GMP
	  if (s7_int_abs(integer(y)) >= INT64_TO_DOUBLE_LIMIT) /* (- .1 92233720368547758071) */
	    {
	      mpfr_set_d(sc->mpfr_1, real(x), MPFR_RNDN);
	      mpfr_sub_si(sc->mpfr_1, sc->mpfr_1, integer(y), MPFR_RNDN);
	      return(mpfr_to_big_real(sc, sc->mpfr_1));
	    }
#endif
	  return(make_real(sc, real(x) - (long_double)integer(y))); /* long_double saves (- 9007199254740996.0 9007199254740995): 1.0 */
	case T_RATIO:
	  return(make_real(sc, real(x) - fraction(y)));
	case T_REAL:
	  return(make_real(sc, real(x) - real(y)));
	case T_COMPLEX:
	  return(s7_make_complex(sc, real(x) - real_part(y), -imag_part(y)));
#if WITH_GMP
	case T_BIG_INTEGER:
	  mpfr_set_d(sc->mpfr_1, real(x), MPFR_RNDN);
	  mpfr_sub_z(sc->mpfr_1, sc->mpfr_1, big_integer(y), MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	case T_BIG_RATIO:
	  mpfr_set_d(sc->mpfr_1, real(x), MPFR_RNDN);
	  mpfr_sub_q(sc->mpfr_1, sc->mpfr_1, big_ratio(y), MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	case T_BIG_REAL:
	  mpfr_d_sub(sc->mpfr_1, real(x), big_real(y), MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	case T_BIG_COMPLEX:
	  mpc_set_d_d(sc->mpc_1, real(x), 0.0, MPC_RNDNN);
	  mpc_sub(sc->mpc_1, sc->mpc_1, big_complex(y), MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
#endif
	default:
	  return(method_or_bust_with_type_pp(sc, y, sc->subtract_symbol, x, y, a_number_string, 2));
	}

    case T_COMPLEX:
      switch (type(y))
	{
	case T_INTEGER:
	  return(s7_make_complex(sc, real_part(x) - integer(y), imag_part(x)));
	case T_RATIO:
	  return(s7_make_complex(sc, real_part(x) - fraction(y), imag_part(x)));
	case T_REAL:
	  return(s7_make_complex(sc, real_part(x) - real(y), imag_part(x)));
	case T_COMPLEX:
	  return(make_complex(sc, real_part(x) - real_part(y), imag_part(x) - imag_part(y)));
#if WITH_GMP
	case T_BIG_INTEGER:
	  mpc_set_d_d(sc->mpc_1, real_part(x), imag_part(x), MPC_RNDNN);
	  mpc_set_z(sc->mpc_2, big_integer(y), MPC_RNDNN);
	  mpc_sub(sc->mpc_1, sc->mpc_1, sc->mpc_2, MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	case T_BIG_RATIO:
	  mpc_set_d_d(sc->mpc_1, real_part(x), imag_part(x), MPC_RNDNN);
	  mpc_set_q(sc->mpc_2, big_ratio(y), MPC_RNDNN);
	  mpc_sub(sc->mpc_1, sc->mpc_1, sc->mpc_2, MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	case T_BIG_REAL:
	  mpc_set_d_d(sc->mpc_1, real_part(x), imag_part(x), MPC_RNDNN);
	  mpc_sub_fr(sc->mpc_1, sc->mpc_1, big_real(y), MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	case T_BIG_COMPLEX:
	  mpc_set_d_d(sc->mpc_1, real_part(x), imag_part(x), MPC_RNDNN);
	  mpc_sub(sc->mpc_1, sc->mpc_1, big_complex(y), MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
#endif
	default:
	  return(method_or_bust_with_type_pp(sc, y, sc->subtract_symbol, x, y, a_number_string, 2));
	}

#if WITH_GMP
    case T_BIG_INTEGER:
      switch (type(y))
	{
	case T_INTEGER:
	  mpz_set_si(sc->mpz_1, integer(y));
	  mpz_sub(sc->mpz_1, big_integer(x), sc->mpz_1);
	  return(mpz_to_integer(sc, sc->mpz_1));
	case T_RATIO:
	  mpq_set_z(sc->mpq_2, big_integer(x));
	  mpq_set_si(sc->mpq_1, numerator(y), denominator(y));
	  mpq_sub(sc->mpq_1, sc->mpq_2, sc->mpq_1);
	  return(mpq_to_canonicalized_rational(sc, sc->mpq_1));
	case T_REAL:
	  if (is_NaN(real(y))) return(real_NaN);
	  mpfr_set_z(sc->mpfr_1, big_integer(x), MPFR_RNDN);
	  mpfr_sub_d(sc->mpfr_1, sc->mpfr_1, real(y), MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	case T_COMPLEX:
	  mpc_set_d_d(sc->mpc_1, real_part(y), imag_part(y), MPC_RNDNN);
	  mpc_set_z(sc->mpc_2, big_integer(x), MPC_RNDNN);
	  mpc_sub(sc->mpc_1, sc->mpc_2, sc->mpc_1, MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	case T_BIG_INTEGER:
	  mpz_sub(sc->mpz_1, big_integer(x), big_integer(y));
	  return(mpz_to_integer(sc, sc->mpz_1));
	case T_BIG_RATIO:
	  mpq_set_z(sc->mpq_1, big_integer(x));
	  mpq_sub(sc->mpq_1, sc->mpq_1, big_ratio(y));
	  return(mpq_to_rational(sc, sc->mpq_1));
	case T_BIG_REAL:
	  mpfr_set_z(sc->mpfr_1, big_integer(x), MPFR_RNDN);
	  mpfr_sub(sc->mpfr_1, sc->mpfr_1, big_real(y), MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	case T_BIG_COMPLEX:
	  mpc_set_z(sc->mpc_1, big_integer(x), MPC_RNDNN);
	  mpc_sub(sc->mpc_1, sc->mpc_1, big_complex(y), MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	default:
	  return(method_or_bust_with_type_pp(sc, y, sc->subtract_symbol, x, y, a_number_string, 2));
	}

    case T_BIG_RATIO:
      switch (type(y))
	{
	case T_INTEGER:
	  mpq_set_si(sc->mpq_1, integer(y), 1);
	  mpq_sub(sc->mpq_1, big_ratio(x), sc->mpq_1);
	  return(mpq_to_rational(sc, sc->mpq_1));
	case T_RATIO:
	  mpq_set_si(sc->mpq_1, numerator(y), denominator(y));
	  mpq_sub(sc->mpq_1, big_ratio(x), sc->mpq_1);
	  return(mpq_to_rational(sc, sc->mpq_1));
	case T_REAL:
	  if (is_NaN(real(y))) return(real_NaN);
	  mpfr_set_q(sc->mpfr_1, big_ratio(x), MPFR_RNDN);
	  mpfr_sub_d(sc->mpfr_1, sc->mpfr_1, real(y), MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	case T_COMPLEX:
	  mpc_set_q(sc->mpc_1, big_ratio(x), MPC_RNDNN);
	  mpc_set_d_d(sc->mpc_2, real_part(y), imag_part(y), MPC_RNDNN);
	  mpc_sub(sc->mpc_1, sc->mpc_1, sc->mpc_2, MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	case T_BIG_INTEGER:
	  mpq_set_z(sc->mpq_1, big_integer(y));
	  mpq_sub(sc->mpq_1, big_ratio(x), sc->mpq_1);
	  return(mpq_to_canonicalized_rational(sc, sc->mpq_1));
	case T_BIG_RATIO:
	  mpq_sub(sc->mpq_1, big_ratio(x), big_ratio(y));
	  return(mpq_to_canonicalized_rational(sc, sc->mpq_1));
	case T_BIG_REAL:
	  mpfr_set_q(sc->mpfr_1, big_ratio(x), MPFR_RNDN);
	  mpfr_sub(sc->mpfr_1, sc->mpfr_1, big_real(y), MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	case T_BIG_COMPLEX:
	  mpc_set_q(sc->mpc_1, big_ratio(x), MPC_RNDNN);
	  mpc_sub(sc->mpc_1, sc->mpc_1, big_complex(y), MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	default:
	  return(method_or_bust_with_type_pp(sc, y, sc->subtract_symbol, x, y, a_number_string, 2));
	}

    case T_BIG_REAL:
      switch (type(y))
	{
	case T_INTEGER:
	  mpfr_sub_si(sc->mpfr_1, big_real(x), integer(y), MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	case T_RATIO:
	  mpq_set_si(sc->mpq_1, numerator(y), denominator(y));
	  mpfr_sub_q(sc->mpfr_1, big_real(x), sc->mpq_1, MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	case T_REAL:
	  if (is_NaN(real(y))) return(real_NaN);
	  mpfr_sub_d(sc->mpfr_1, big_real(x), real(y), MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	case T_COMPLEX:
	  mpc_set_d_d(sc->mpc_1, real_part(y), imag_part(y), MPC_RNDNN);
	  mpc_fr_sub(sc->mpc_1, big_real(x), sc->mpc_1, MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	case T_BIG_INTEGER:
	  mpfr_sub_z(sc->mpfr_1, big_real(x), big_integer(y), MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	case T_BIG_RATIO:
	  mpfr_sub_q(sc->mpfr_1, big_real(x), big_ratio(y), MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	case T_BIG_REAL:
	  mpfr_sub(sc->mpfr_1, big_real(x), big_real(y), MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	case T_BIG_COMPLEX:
	  mpc_fr_sub(sc->mpc_1, big_real(x), big_complex(y), MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	default:
	  return(method_or_bust_with_type_pp(sc, y, sc->subtract_symbol, x, y, a_number_string, 2));
	}
    case T_BIG_COMPLEX:
      switch (type(y))
	{
	case T_INTEGER:
	  mpc_set_si(sc->mpc_2, integer(y), MPC_RNDNN);
	  mpc_sub(sc->mpc_1, big_complex(x), sc->mpc_2, MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	case T_RATIO:
	  mpq_set_si(sc->mpq_1, numerator(y), denominator(y));
	  mpc_set_q(sc->mpc_1, sc->mpq_1, MPC_RNDNN);
	  mpc_sub(sc->mpc_1, big_complex(x), sc->mpc_1, MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	case T_REAL:
	  /* if (is_NaN(real(y))) return(real_NaN); */
	  mpc_set_d_d(sc->mpc_1, real(y), 0.0, MPC_RNDNN);
	  mpc_sub(sc->mpc_1, big_complex(x), sc->mpc_1, MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	case T_COMPLEX:
	  mpc_set_d_d(sc->mpc_1, real_part(y), imag_part(y), MPC_RNDNN);
	  mpc_sub(sc->mpc_1, big_complex(x), sc->mpc_1, MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	case T_BIG_INTEGER:
	  mpc_set_z(sc->mpc_1, big_integer(y), MPC_RNDNN);
	  mpc_sub(sc->mpc_1, big_complex(x), sc->mpc_1, MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	case T_BIG_RATIO:
	  mpc_set_q(sc->mpc_1, big_ratio(y), MPC_RNDNN);
	  mpc_sub(sc->mpc_1, big_complex(x), sc->mpc_1, MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	case T_BIG_REAL:
	  mpc_sub_fr(sc->mpc_1, big_complex(x), big_real(y), MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	case T_BIG_COMPLEX:
	  mpc_sub(sc->mpc_1, big_complex(x), big_complex(y), MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	default:
	  return(method_or_bust_with_type_pp(sc, y, sc->subtract_symbol, x, y, a_number_string, 2));
	}
#endif
      default:
	return(method_or_bust_with_type_pp(sc, x, sc->subtract_symbol, x, y, a_number_string, 1));
    }
}

static s7_pointer g_subtract(s7_scheme *sc, s7_pointer args)
{
  #define H_subtract "(- x1 ...) subtracts its trailing arguments from the first, or negates the first if only one it is given"
  #define Q_subtract sc->pcl_n

  s7_pointer x, p;
  x = car(args);
  p = cdr(args);
  if (is_null(p))
    return(negate_p_p(sc, x));
  return((is_null(cddr(args))) ? subtract_p_pp(sc, x, cadr(args)) : subtract_p_pp(sc, x, g_add(sc, cdr(args))));
}

static s7_pointer g_subtract_1(s7_scheme *sc, s7_pointer args) {return(negate_p_p(sc, car(args)));}
static s7_pointer g_subtract_2(s7_scheme *sc, s7_pointer args) {return(subtract_p_pp(sc, car(args), cadr(args)));}
/* static s7_pointer g_subtract_3(s7_scheme *sc, s7_pointer args) {return(subtract_p_pp(sc, subtract_p_pp(sc, car(args), cadr(args)), caddr(args)));} */
static s7_pointer g_subtract_3(s7_scheme *sc, s7_pointer args) {return(subtract_p_pp(sc, car(args), add_p_pp(sc, cadr(args), caddr(args))));}

static s7_pointer minus_c1(s7_scheme *sc, s7_pointer x)
{
  switch (type(x))
    {
    case T_INTEGER: return(subtract_if_overflow_to_real_or_big_integer(sc, integer(x), 1));
    case T_RATIO:   return(subtract_p_pp(sc, x, int_one));
    case T_REAL:    return(make_real(sc, real(x) - 1.0));
    case T_COMPLEX: return(s7_make_complex(sc, real_part(x) - 1.0, imag_part(x)));
#if WITH_GMP
    case T_BIG_INTEGER: case T_BIG_RATIO: case T_BIG_REAL: case T_BIG_COMPLEX:
      return(subtract_p_pp(sc, x, int_one));
#endif
    default:
      return(method_or_bust_with_type_pp(sc, x, sc->subtract_symbol, x, int_one, a_number_string, 1));
    }
  return(x);
}

static s7_pointer g_subtract_x1(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p;
  p = car(args);
#if WITH_GMP
  return(subtract_p_pp(sc, p, int_one));
#endif
  return((is_t_integer(p)) ? make_integer(sc, integer(p) - 1) : minus_c1(sc, p));
}

static s7_pointer g_subtract_2f(s7_scheme *sc, s7_pointer args) /* (- x f) */
{
  s7_pointer x;
  s7_double n;
  x = car(args);
  n = real(cadr(args)); /* checked below is_t_real */
  if (is_t_real(x)) return(make_real(sc, real(x) - n));
  switch (type(x))
    {
    case T_INTEGER: return(make_real(sc, integer(x) - n));
    case T_RATIO:   return(make_real(sc, fraction(x) - n));
    case T_COMPLEX: return(s7_make_complex(sc, real_part(x) - n, imag_part(x)));
#if WITH_GMP
    case T_BIG_INTEGER: case T_BIG_RATIO: case T_BIG_REAL: case T_BIG_COMPLEX:
      return(subtract_p_pp(sc, x, cadr(args)));
#endif
    default:
      return(method_or_bust_with_type(sc, x, sc->subtract_symbol, args, a_number_string, 1));
    }
  return(x);
}

static s7_pointer g_subtract_f2(s7_scheme *sc, s7_pointer args) /* (- f x) */
{
  s7_pointer x;
  s7_double n;
  x = cadr(args);
  n = real(car(args)); /* checked below is_t_real */
  if (is_t_real(x)) return(make_real(sc, n - real(x)));
  switch (type(x))
    {
    case T_INTEGER: return(make_real(sc, n - integer(x)));
    case T_RATIO:   return(make_real(sc, n - fraction(x)));
    case T_COMPLEX: return(s7_make_complex(sc, n - real_part(x), -imag_part(x)));
#if WITH_GMP
    case T_BIG_INTEGER: case T_BIG_RATIO: case T_BIG_REAL: case T_BIG_COMPLEX:
      return(subtract_p_pp(sc, car(args), x));
#endif
    default:
      return(method_or_bust_with_type(sc, x, sc->subtract_symbol, args, a_number_string, 1));
    }
  return(x);
}

static s7_int subtract_i_ii(s7_int i1, s7_int i2) {return(i1 - i2);}
static s7_int subtract_i_i(s7_int x) {return(-x);}
static s7_int subtract_i_iii(s7_int i1, s7_int i2, s7_int i3) {return(i1 - i2 - i3);}

static s7_double subtract_d_d(s7_double x) {return(-x);}
static s7_double subtract_d_dd(s7_double x1, s7_double x2) {return(x1 - x2);}
static s7_double subtract_d_ddd(s7_double x1, s7_double x2, s7_double x3) {return(x1 - x2 - x3);}
static s7_double subtract_d_dddd(s7_double x1, s7_double x2, s7_double x3, s7_double x4) {return(x1 - x2 - x3 - x4);}

static s7_pointer subtract_p_dd(s7_scheme *sc, s7_double x1, s7_double x2) {return(make_real(sc, x1 - x2));}
static s7_pointer subtract_p_ii(s7_scheme *sc, s7_int i1, s7_int i2) {return(make_integer(sc, i1 - i2));}

static s7_pointer g_sub_xi(s7_scheme *sc, s7_pointer x, s7_int y)
{
  if (is_t_integer(x))
    return(subtract_if_overflow_to_real_or_big_integer(sc, integer(x), y));

  switch (type(x))
    {
    case T_RATIO:   return(s7_make_ratio(sc, numerator(x) - (y * denominator(x)), denominator(x)));
    case T_REAL:    return(make_real(sc, real(x) - y));
    case T_COMPLEX: return(s7_make_complex(sc, real_part(x) - y, imag_part(x)));
#if WITH_GMP
    case T_BIG_INTEGER:
      mpz_set_si(sc->mpz_1, y);
      mpz_sub(sc->mpz_1, big_integer(x), sc->mpz_1);
      return(mpz_to_integer(sc, sc->mpz_1));
    case T_BIG_RATIO:
    case T_BIG_REAL:
    case T_BIG_COMPLEX:
      return(subtract_p_pp(sc, x, wrap_integer1(sc, y)));
#endif
    default: return(method_or_bust_with_type_pi(sc, x, sc->subtract_symbol, x, y, a_number_string));
    }
  return(x);
}

static s7_pointer subtract_chooser(s7_scheme *sc, s7_pointer f, int32_t args, s7_pointer expr, bool ops)
{
  if (args == 1)
    return(sc->subtract_1);
  if (args == 2)
    {
      if (ops)
	{
	  s7_pointer arg1, arg2;
	  arg1 = cadr(expr);
	  arg2 = caddr(expr);
	  if (arg2 == int_one) return(sc->subtract_x1);
	  if (is_t_real(arg1)) return(sc->subtract_f2);
	  if (is_t_real(arg2)) return(sc->subtract_2f);
	}
      return(sc->subtract_2);
    }
  return((args == 3) ? sc->subtract_3 : f);
}

