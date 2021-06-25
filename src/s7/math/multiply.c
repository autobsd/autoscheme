#define QUOTIENT_FLOAT_LIMIT 1e13
#define QUOTIENT_INT_LIMIT 10000000000000
/* fraction(x) is not accurate enough if it involves numbers over e18 even when done with long_doubles */

static inline s7_pointer multiply_if_overflow_to_real_or_big_integer(s7_scheme *sc, s7_int x, s7_int y)
{
#if HAVE_OVERFLOW_CHECKS
  s7_int val;
  if (multiply_overflow(x, y, &val))
#if WITH_GMP
    {
      mpz_set_si(sc->mpz_1, x);
      mpz_mul_si(sc->mpz_1, sc->mpz_1, y);
      return(mpz_to_big_integer(sc, sc->mpz_1));
    }
#else
    return(make_real(sc, (double)x * (double)y));
#endif
    return(make_integer(sc, val));
#else
  return(make_integer(sc, x * y));
#endif
}

static s7_pointer integer_ratio_multiply_if_overflow_to_real_or_ratio(s7_scheme *sc, s7_int x, s7_pointer y)
{
#if HAVE_OVERFLOW_CHECKS
  s7_int z;
  if (multiply_overflow(x, numerator(y), &z))
#if WITH_GMP
    {
      mpz_set_si(sc->mpz_1, x);
      mpz_mul_si(sc->mpz_1, sc->mpz_1, numerator(y));
      mpq_set_si(sc->mpq_1, 1, denominator(y));
      mpq_set_num(sc->mpq_1, sc->mpz_1);
      return(mpq_to_canonicalized_rational(sc, sc->mpq_1));
    }
#else
    return(make_real(sc, (double)x * fraction(y)));
#endif
    return(s7_make_ratio(sc, z, denominator(y)));
#else
  return(s7_make_ratio(sc, x * numerator(y), denominator(y)));
#endif
}

static s7_pointer multiply_p_pp(s7_scheme *sc, s7_pointer x, s7_pointer y)
{
  switch (type(x))
    {
    case T_INTEGER:
      switch (type(y))
	{
	case T_INTEGER:
	  return(multiply_if_overflow_to_real_or_big_integer(sc, integer(x), integer(y)));
	case T_RATIO:
	  return(integer_ratio_multiply_if_overflow_to_real_or_ratio(sc, integer(x), y));
	case T_REAL:
#if WITH_GMP
	  if (s7_int_abs(integer(x)) > QUOTIENT_INT_LIMIT)
	    {
	      mpfr_set_si(sc->mpfr_1, integer(x), MPFR_RNDN);
	      mpfr_set_d(sc->mpfr_2, real(y), MPFR_RNDN);
	      mpfr_mul(sc->mpfr_1, sc->mpfr_1, sc->mpfr_2, MPFR_RNDN);
	      return(mpfr_to_big_real(sc, sc->mpfr_1));
	    }
#endif
	  return(make_real(sc, (long_double)integer(x) * real(y)));
	case T_COMPLEX:
	  return(s7_make_complex(sc, (long_double)integer(x) * real_part(y), (long_double)integer(x) * imag_part(y)));
#if WITH_GMP
	case T_BIG_INTEGER:
	  mpz_mul_si(sc->mpz_1, big_integer(y), integer(x));
	  return(mpz_to_integer(sc, sc->mpz_1));
	case T_BIG_RATIO:
	  mpq_set_si(sc->mpq_1, integer(x), 1);
	  mpq_mul(sc->mpq_1, sc->mpq_1, big_ratio(y));
	  return(mpq_to_rational(sc, sc->mpq_1));
	case T_BIG_REAL:
	  mpfr_mul_si(sc->mpfr_1, big_real(y), integer(x), MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	case T_BIG_COMPLEX:
	  mpc_mul_si(sc->mpc_1, big_complex(y), integer(x), MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1)); /* x might be 0 */
#endif
	default:
	  return(method_or_bust_with_type_pp(sc, y, sc->multiply_symbol, x, y, a_number_string, 2));
	}

    case T_RATIO:
      switch (type(y))
	{
	case T_INTEGER:
	  return(integer_ratio_multiply_if_overflow_to_real_or_ratio(sc, integer(y), x));
	case T_RATIO:
	  {
	    s7_int d1, d2, n1, n2;
	    parcel_out_fractions(x, y);
#if HAVE_OVERFLOW_CHECKS
	    {
	      s7_int n1n2, d1d2;
	      if ((multiply_overflow(d1, d2, &d1d2)) ||
		  (multiply_overflow(n1, n2, &n1n2)))
#if WITH_GMP
		{
		  mpq_set_si(sc->mpq_1, n1, d1);
		  mpq_set_si(sc->mpq_2, n2, d2);
		  mpq_mul(sc->mpq_1, sc->mpq_1, sc->mpq_2);
		  return(mpq_to_canonicalized_rational(sc, sc->mpq_1));
		}
#else
	      return(make_real(sc, fraction(x) * fraction(y)));
#endif
	      return(s7_make_ratio(sc, n1n2, d1d2));
	    }
#else
	    return(s7_make_ratio(sc, n1 * n2, d1 * d2));
#endif
	  }
	case T_REAL:
#if WITH_GMP
	  if (numerator(x) > QUOTIENT_INT_LIMIT)
	    {
	      mpq_set_si(sc->mpq_1, numerator(x), denominator(x));
	      mpfr_set_q(sc->mpfr_1, sc->mpq_1, MPFR_RNDN);
	      mpfr_mul_d(sc->mpfr_1, sc->mpfr_1, real(y), MPFR_RNDN);
	      return(mpfr_to_big_real(sc, sc->mpfr_1));
	    }
#endif
	  return(make_real(sc, fraction(x) * real(y)));
	case T_COMPLEX:
	  return(s7_make_complex(sc, fraction(x) * real_part(y), fraction(x) * imag_part(y)));
#if WITH_GMP
	case T_BIG_INTEGER:
	  mpq_set_si(sc->mpq_1, numerator(x), denominator(x));
	  mpq_set_z(sc->mpq_2, big_integer(y));
	  mpq_mul(sc->mpq_1, sc->mpq_1, sc->mpq_2);
	  return(mpq_to_canonicalized_rational(sc, sc->mpq_1));
	case T_BIG_RATIO:
	  mpq_set_si(sc->mpq_1, numerator(x), denominator(x));
	  mpq_mul(sc->mpq_1, sc->mpq_1, big_ratio(y));
	  return(mpq_to_canonicalized_rational(sc, sc->mpq_1));
	case T_BIG_REAL:
	  mpq_set_si(sc->mpq_1, numerator(x), denominator(x));
	  mpfr_mul_q(sc->mpfr_1, big_real(y), sc->mpq_1, MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	case T_BIG_COMPLEX:
	  mpq_set_si(sc->mpq_1, numerator(x), denominator(x));
	  mpc_set_q(sc->mpc_1, sc->mpq_1, MPC_RNDNN);
	  mpc_mul(sc->mpc_1, sc->mpc_1, big_complex(y), MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
#endif
	default:
	  return(method_or_bust_with_type_pp(sc, y, sc->multiply_symbol, x, y, a_number_string, 2));
	}

    case T_REAL:
      switch (type(y))
	{
	case T_INTEGER:
#if WITH_GMP
	  if (s7_int_abs(integer(y)) > QUOTIENT_INT_LIMIT)
	    {
	      mpfr_set_si(sc->mpfr_1, integer(y), MPFR_RNDN);
	      mpfr_set_d(sc->mpfr_2, real(x), MPFR_RNDN);
	      mpfr_mul(sc->mpfr_1, sc->mpfr_1, sc->mpfr_2, MPFR_RNDN);
	      return(mpfr_to_big_real(sc, sc->mpfr_1));
	    }
#endif
	  return(make_real(sc, real(x) * (long_double)integer(y)));
	case T_RATIO:
#if WITH_GMP
	  if (numerator(y) > QUOTIENT_INT_LIMIT)
	    {
	      mpq_set_si(sc->mpq_1, numerator(y), denominator(y));
	      mpfr_set_q(sc->mpfr_1, sc->mpq_1, MPFR_RNDN);
	      mpfr_mul_d(sc->mpfr_1, sc->mpfr_1, real(x), MPFR_RNDN);
	      return(mpfr_to_big_real(sc, sc->mpfr_1));
	    }
#endif
	  return(make_real(sc, fraction(y) * real(x)));
	case T_REAL:
	  return(make_real(sc, real(x) * real(y)));
	case T_COMPLEX:
	  return(make_complex(sc, real(x) * real_part(y), real(x) * imag_part(y)));
#if WITH_GMP
	case T_BIG_INTEGER:
	  mpfr_set_d(sc->mpfr_1, real(x), MPFR_RNDN);
	  mpfr_mul_z(sc->mpfr_1, sc->mpfr_1, big_integer(y), MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	case T_BIG_RATIO:
	  mpfr_set_d(sc->mpfr_1, real(x), MPFR_RNDN);
	  mpfr_mul_q(sc->mpfr_1, sc->mpfr_1, big_ratio(y), MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	case T_BIG_REAL:
	  mpfr_mul_d(sc->mpfr_1, big_real(y), real(x), MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	case T_BIG_COMPLEX:
	  mpc_set_d_d(sc->mpc_1, real(x), 0.0, MPC_RNDNN);
	  mpc_mul(sc->mpc_1, sc->mpc_1, big_complex(y), MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1)); /* x might = 0.0 */
#endif
	default:
	  return(method_or_bust_with_type_pp(sc, y, sc->multiply_symbol, x, y, a_number_string, 2));
	}

    case T_COMPLEX:
      switch (type(y))
	{
	case T_INTEGER:
	  return(make_complex(sc, real_part(x) * integer(y), imag_part(x) * integer(y)));
	case T_RATIO:
	  return(s7_make_complex(sc, real_part(x) * fraction(y), imag_part(x) * fraction(y)));
	case T_REAL:
	  return(make_complex(sc, real_part(x) * real(y), imag_part(x) * real(y)));
	case T_COMPLEX:
	  {
	    s7_double r1, r2, i1, i2;
	    r1 = real_part(x);
	    r2 = real_part(y);
	    i1 = imag_part(x);
	    i2 = imag_part(y);
	    return(make_complex(sc, r1 * r2 - i1 * i2, r1 * i2 + r2 * i1));
	  }
#if WITH_GMP
	case T_BIG_INTEGER:
	  mpc_set_d_d(sc->mpc_1, real_part(x), imag_part(x), MPC_RNDNN);
	  mpc_set_z(sc->mpc_2, big_integer(y), MPC_RNDNN);
	  mpc_mul(sc->mpc_1, sc->mpc_1, sc->mpc_2, MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	case T_BIG_RATIO:
	  mpc_set_d_d(sc->mpc_1, real_part(x), imag_part(x), MPC_RNDNN);
	  mpc_set_q(sc->mpc_2, big_ratio(y), MPC_RNDNN);
	  mpc_mul(sc->mpc_1, sc->mpc_1, sc->mpc_2, MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	case T_BIG_REAL:
	  mpc_set_d_d(sc->mpc_1, real_part(x), imag_part(x), MPC_RNDNN);
	  mpc_mul_fr(sc->mpc_1, sc->mpc_1, big_real(y), MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	case T_BIG_COMPLEX:
	  mpc_set_d_d(sc->mpc_1, real_part(x), imag_part(x), MPC_RNDNN);
	  mpc_mul(sc->mpc_1, sc->mpc_1, big_complex(y), MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
#endif
	default:
	  return(method_or_bust_with_type_pp(sc, y, sc->multiply_symbol, x, y, a_number_string, 2));
	}

#if WITH_GMP
    case T_BIG_INTEGER:
      switch (type(y))
	{
	case T_INTEGER:
	  mpz_mul_si(sc->mpz_1, big_integer(x), integer(y));
	  return(mpz_to_integer(sc, sc->mpz_1));
	case T_RATIO:
	  mpq_set_z(sc->mpq_2, big_integer(x));
	  mpq_set_si(sc->mpq_1, numerator(y), denominator(y));
	  mpq_mul(sc->mpq_1, sc->mpq_2, sc->mpq_1);
	  return(mpq_to_canonicalized_rational(sc, sc->mpq_1));
	case T_REAL:
	  if (is_NaN(real(y))) return(real_NaN);
	  mpfr_set_d(sc->mpfr_1, real(y), MPFR_RNDN);
	  mpfr_mul_z(sc->mpfr_1, sc->mpfr_1, big_integer(x), MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	case T_COMPLEX:
	  mpc_set_d_d(sc->mpc_1, real_part(y), imag_part(y), MPC_RNDNN);
	  mpc_set_z(sc->mpc_2, big_integer(x), MPC_RNDNN);
	  mpc_mul(sc->mpc_1, sc->mpc_2, sc->mpc_1, MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	case T_BIG_INTEGER:
	  mpz_mul(sc->mpz_1, big_integer(x), big_integer(y));
	  return(mpz_to_integer(sc, sc->mpz_1));
	case T_BIG_RATIO:
	  mpq_set_z(sc->mpq_1, big_integer(x));
	  mpq_mul(sc->mpq_1, sc->mpq_1, big_ratio(y));
	  return(mpq_to_rational(sc, sc->mpq_1));
	case T_BIG_REAL:
	  mpfr_mul_z(sc->mpfr_1, big_real(y), big_integer(x), MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	case T_BIG_COMPLEX:
	  mpc_set_z(sc->mpc_1, big_integer(x), MPC_RNDNN);
	  mpc_mul(sc->mpc_1, sc->mpc_1, big_complex(y), MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	default:
	  return(method_or_bust_with_type_pp(sc, y, sc->multiply_symbol, x, y, a_number_string, 2));
	}

    case T_BIG_RATIO:
      switch (type(y))
	{
	case T_INTEGER:
	  mpq_set_si(sc->mpq_1, integer(y), 1);
	  mpq_mul(sc->mpq_1, big_ratio(x), sc->mpq_1);
	  return(mpq_to_rational(sc, sc->mpq_1));
	case T_RATIO:
	  mpq_set_si(sc->mpq_1, numerator(y), denominator(y));
	  mpq_mul(sc->mpq_1, big_ratio(x), sc->mpq_1);
	  return(mpq_to_rational(sc, sc->mpq_1));
	case T_REAL:
	  if (is_NaN(real(y))) return(real_NaN);
	  mpfr_set_d(sc->mpfr_1, real(y), MPFR_RNDN);
	  mpfr_mul_q(sc->mpfr_1, sc->mpfr_1, big_ratio(x), MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	case T_COMPLEX:
	  mpc_set_q(sc->mpc_1, big_ratio(x), MPC_RNDNN);
	  mpc_set_d_d(sc->mpc_2, real_part(y), imag_part(y), MPC_RNDNN);
	  mpc_mul(sc->mpc_1, sc->mpc_1, sc->mpc_2, MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	case T_BIG_INTEGER:
	  mpq_set_z(sc->mpq_1, big_integer(y));
	  mpq_mul(sc->mpq_1, big_ratio(x), sc->mpq_1);
	  return(mpq_to_canonicalized_rational(sc, sc->mpq_1));
	case T_BIG_RATIO:
	  mpq_mul(sc->mpq_1, big_ratio(x), big_ratio(y));
	  return(mpq_to_canonicalized_rational(sc, sc->mpq_1));
	case T_BIG_REAL:
	  mpfr_mul_q(sc->mpfr_1, big_real(y), big_ratio(x), MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	case T_BIG_COMPLEX:
	  mpc_set_q(sc->mpc_1, big_ratio(x), MPC_RNDNN);
	  mpc_mul(sc->mpc_1, sc->mpc_1, big_complex(y), MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	default:
	  return(method_or_bust_with_type_pp(sc, y, sc->multiply_symbol, x, y, a_number_string, 2));
	}

    case T_BIG_REAL:
      switch (type(y))
	{
	case T_INTEGER:
	  mpfr_mul_si(sc->mpfr_1, big_real(x), integer(y), MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	case T_RATIO:
	  mpq_set_si(sc->mpq_1, numerator(y), denominator(y));
	  mpfr_mul_q(sc->mpfr_1, big_real(x), sc->mpq_1, MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	case T_REAL:
	  if (is_NaN(real(y))) return(real_NaN);
	  mpfr_mul_d(sc->mpfr_1, big_real(x), real(y), MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	case T_COMPLEX:
	  mpc_set_d_d(sc->mpc_1, real_part(y), imag_part(y), MPC_RNDNN);
	  mpc_mul_fr(sc->mpc_1, sc->mpc_1, big_real(x), MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	case T_BIG_INTEGER:
	  mpfr_mul_z(sc->mpfr_1, big_real(x), big_integer(y), MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	case T_BIG_RATIO:
	  mpfr_mul_q(sc->mpfr_1, big_real(x), big_ratio(y), MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	case T_BIG_REAL:
	  mpfr_mul(sc->mpfr_1, big_real(x), big_real(y), MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	case T_BIG_COMPLEX:
	  mpc_mul_fr(sc->mpc_1, big_complex(y), big_real(x), MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1)); /* 0.0? */
	default:
	  return(method_or_bust_with_type_pp(sc, y, sc->multiply_symbol, x, y, a_number_string, 2));
	}
    case T_BIG_COMPLEX:
      switch (type(y))
	{
	case T_INTEGER:
	  mpc_mul_si(sc->mpc_1, big_complex(x), integer(y), MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	case T_RATIO:
	  mpq_set_si(sc->mpq_1, numerator(y), denominator(y));
	  mpc_set_q(sc->mpc_1, sc->mpq_1, MPC_RNDNN);
	  mpc_mul(sc->mpc_1, big_complex(x), sc->mpc_1, MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	case T_REAL:
	  /* if (is_NaN(real(y))) return(real_NaN); */
	  mpc_set_d_d(sc->mpc_1, real(y), 0.0, MPC_RNDNN);
	  mpc_mul(sc->mpc_1, big_complex(x), sc->mpc_1, MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	case T_COMPLEX:
	  mpc_set_d_d(sc->mpc_1, real_part(y), imag_part(y), MPC_RNDNN);
	  mpc_mul(sc->mpc_1, big_complex(x), sc->mpc_1, MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	case T_BIG_INTEGER:
	  mpc_set_z(sc->mpc_1, big_integer(y), MPC_RNDNN);
	  mpc_mul(sc->mpc_1, big_complex(x), sc->mpc_1, MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	case T_BIG_RATIO:
	  mpc_set_q(sc->mpc_1, big_ratio(y), MPC_RNDNN);
	  mpc_mul(sc->mpc_1, big_complex(x), sc->mpc_1, MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	case T_BIG_REAL:
	  mpc_mul_fr(sc->mpc_1, big_complex(x), big_real(y), MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	case T_BIG_COMPLEX:
	  mpc_mul(sc->mpc_1, big_complex(x), big_complex(y), MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	default:
	  return(method_or_bust_with_type_pp(sc, y, sc->multiply_symbol, x, y, a_number_string, 2));
	}
#endif
      default:
	return(method_or_bust_with_type_pp(sc, x, sc->multiply_symbol, x, y, a_number_string, 1));
    }
}

static s7_pointer multiply_p_ppp(s7_scheme *sc, s7_pointer x, s7_pointer y, s7_pointer z) {return(multiply_p_pp(sc, multiply_p_pp(sc, x, y), z));}

static s7_pointer multiply_method_or_bust(s7_scheme *sc, s7_pointer obj, s7_pointer caller, s7_pointer args, s7_pointer typ, int32_t num)
{
  if (has_active_methods(sc, obj))
    return(find_and_apply_method(sc, obj, sc->multiply_symbol, args));
  if (num == 0)
    return(simple_wrong_type_argument_with_type(sc, sc->multiply_symbol, obj, typ));
  return(wrong_type_argument_with_type(sc, sc->multiply_symbol, num, obj, typ));
}

static s7_pointer g_multiply(s7_scheme *sc, s7_pointer args)
{
  #define H_multiply "(* ...) multiplies its arguments"
  #define Q_multiply sc->pcl_n

  s7_pointer x, p;
  if (is_null(args))
    return(int_one);
  x = car(args);
  p = cdr(args);
  if (is_null(p))
    {
      if (!is_number(x))
	return(multiply_method_or_bust(sc, x, sc->multiply_symbol, args, a_number_string, 0));
      return(x);
    }
  if (is_null(cdr(p)))
    return(multiply_p_pp(sc, x, car(p)));
  for (; is_pair(p); p = cdr(p))
    x = multiply_p_pp(sc, x, car(p));
  return(x);
}

static s7_pointer g_multiply_2(s7_scheme *sc, s7_pointer args) {return(multiply_p_pp(sc, car(args), cadr(args)));}

static s7_pointer g_mul_xi(s7_scheme *sc, s7_pointer x, s7_int n)
{
  switch (type(x))
    {
    case T_INTEGER: return(multiply_if_overflow_to_real_or_big_integer(sc, integer(x), n));
    case T_RATIO:   return(integer_ratio_multiply_if_overflow_to_real_or_ratio(sc, n, x));
    case T_REAL:    return(make_real(sc, real(x) * n));
    case T_COMPLEX: return(s7_make_complex(sc, real_part(x) * n, imag_part(x) * n));
#if WITH_GMP
    case T_BIG_INTEGER:
      mpz_mul_si(sc->mpz_1, big_integer(x), n);
      return(mpz_to_integer(sc, sc->mpz_1));
    case T_BIG_RATIO:
    case T_BIG_REAL:
    case T_BIG_COMPLEX:
      return(multiply_p_pp(sc, x, wrap_integer1(sc, n)));
#endif
    default:
      /* we can get here from mul_2_xi for example so the non-integer argument might not be a symbol */
      return(method_or_bust_with_type_pi(sc, x, sc->multiply_symbol, x, n, a_number_string));
    }
  return(x);
}

static s7_pointer g_mul_xf(s7_scheme *sc, s7_pointer x, s7_double y)
{
  switch (type(x))
    {
    case T_INTEGER: return(make_real(sc, integer(x) * y));
    case T_RATIO:   return(make_real(sc, numerator(x) * y / denominator(x)));
    case T_REAL:    return(make_real(sc, real(x) * y));
    case T_COMPLEX: return(s7_make_complex(sc, real_part(x) * y, imag_part(x) * y));
#if WITH_GMP
    case T_BIG_INTEGER:
      mpfr_set_d(sc->mpfr_1, y, MPFR_RNDN);
      mpfr_mul_z(sc->mpfr_1, sc->mpfr_1, big_integer(x), MPFR_RNDN);
      return(mpfr_to_big_real(sc, sc->mpfr_1));
    case T_BIG_RATIO:
      mpfr_set_d(sc->mpfr_1, y, MPFR_RNDN);
      mpfr_mul_q(sc->mpfr_1, sc->mpfr_1, big_ratio(x), MPFR_RNDN);
      return(mpfr_to_big_real(sc, sc->mpfr_1));
    case T_BIG_REAL:
      mpfr_mul_d(sc->mpfr_1, big_real(x), y, MPFR_RNDN);
      return(mpfr_to_big_real(sc, sc->mpfr_1));
    case T_BIG_COMPLEX:
      mpfr_set_d(sc->mpfr_1, y, MPFR_RNDN);
      mpc_mul_fr(sc->mpc_1, big_complex(x), sc->mpfr_1, MPC_RNDNN);
      return(mpc_to_number(sc, sc->mpc_1));
#endif
    default: return(method_or_bust_with_type_pf(sc, x, sc->multiply_symbol, x, y, a_number_string));
    }
  return(x);
}

#if WITH_GMP
static s7_pointer g_mul_2_if(s7_scheme *sc, s7_pointer args) {
  if ((is_t_integer(car(args))) && (is_t_real(cadr(args))))
    return(make_real(sc, integer(car(args)) * real(cadr(args))));
  return(multiply_p_pp(sc, car(args), cadr(args)));
}
static s7_pointer g_mul_2_fi(s7_scheme *sc, s7_pointer args)
{
  if ((is_t_integer(cadr(args))) && (is_t_real(car(args))))
    return(make_real(sc, real(car(args)) * integer(cadr(args))));
  return(multiply_p_pp(sc, car(args), cadr(args)));
}
static s7_pointer g_mul_2_xi(s7_scheme *sc, s7_pointer args) {if (is_t_integer(cadr(args))) return(g_mul_xi(sc, car(args), integer(cadr(args)))); return(g_multiply(sc, args));}
static s7_pointer g_mul_2_ix(s7_scheme *sc, s7_pointer args) {if (is_t_integer(car(args))) return(g_mul_xi(sc, cadr(args), integer(car(args)))); return(g_multiply(sc, args));}
static s7_pointer g_mul_2_xf(s7_scheme *sc, s7_pointer args) {if (is_t_real(cadr(args))) return(g_mul_xf(sc, car(args), real(cadr(args)))); return(g_multiply(sc, args));}
static s7_pointer g_mul_2_fx(s7_scheme *sc, s7_pointer args) {if (is_t_real(car(args))) return(g_mul_xf(sc, cadr(args), real(car(args)))); return(g_multiply(sc, args));}
static s7_pointer g_mul_2_ff(s7_scheme *sc, s7_pointer args) {return(multiply_p_pp(sc, car(args), cadr(args)));}
static s7_pointer g_mul_2_ii(s7_scheme *sc, s7_pointer args) {return(multiply_p_pp(sc, car(args), cadr(args)));}
#else
static s7_pointer g_mul_2_if(s7_scheme *sc, s7_pointer args) {return(make_real(sc, integer(car(args)) * real(cadr(args))));}
static s7_pointer g_mul_2_fi(s7_scheme *sc, s7_pointer args) {return(make_real(sc, real(car(args)) * integer(cadr(args))));}
static s7_pointer g_mul_2_xi(s7_scheme *sc, s7_pointer args) {return(g_mul_xi(sc, car(args), integer(cadr(args))));}
static s7_pointer g_mul_2_ix(s7_scheme *sc, s7_pointer args) {return(g_mul_xi(sc, cadr(args), integer(car(args))));}
static s7_pointer g_mul_2_xf(s7_scheme *sc, s7_pointer args) {return(g_mul_xf(sc, car(args), real(cadr(args))));}
static s7_pointer g_mul_2_fx(s7_scheme *sc, s7_pointer args) {return(g_mul_xf(sc, cadr(args), real(car(args))));}
static s7_pointer g_mul_2_ff(s7_scheme *sc, s7_pointer args) {return(make_real(sc, real(car(args)) * real(cadr(args))));}

static s7_pointer g_mul_2_ii(s7_scheme *sc, s7_pointer args)
{
#if HAVE_OVERFLOW_CHECKS
  s7_int val, x, y;
  x = integer(car(args));
  y = integer(cadr(args));
  if (multiply_overflow(x, y, &val))
    return(make_real(sc, (double)x * (double)y));
  return(make_integer(sc, val));
#else
  return(make_integer(sc, integer(car(args)) * integer(cadr(args))));
#endif
}
#endif

static s7_int multiply_i_ii(s7_int i1, s7_int i2)
{
#if HAVE_OVERFLOW_CHECKS
  s7_int val;
  if (multiply_overflow(i1, i2, &val))
    return(S7_INT64_MAX); /* this is inconsistent with other unopt cases where an overflow -> double result */
  /* (let () (define (func) (do ((i 0 (+ i 1))) ((= i 1)) (do ((j 0 (+ j 1))) ((= j 1)) (even? (* (ash 1 43) (ash 1 43)))))) (define (hi) (func)) (hi)) */
  return(val);
#else
  return(i1 * i2);
#endif
}

static s7_int multiply_i_iii(s7_int i1, s7_int i2, s7_int i3)
{
#if HAVE_OVERFLOW_CHECKS
  s7_int val1, val2;
  if (multiply_overflow(i1, i2, &val1))
    return(S7_INT64_MAX);
  if (multiply_overflow(val1, i3, &val2))
    return(S7_INT64_MAX);
  return(val2);
#else
  return(i1 * i2 * i3);
#endif
}

static s7_double multiply_d_d(s7_double x) {return(x);}
static s7_double multiply_d_dd(s7_double x1, s7_double x2) {return(x1 * x2);}
static s7_double multiply_d_ddd(s7_double x1, s7_double x2, s7_double x3) {return(x1 * x2 * x3);}
static s7_double multiply_d_dddd(s7_double x1, s7_double x2, s7_double x3, s7_double x4) {return(x1 * x2 * x3 * x4);}
static s7_pointer mul_p_dd(s7_scheme *sc, s7_double x1, s7_double x2) {return(make_real(sc, x1 * x2));}

static s7_pointer multiply_chooser(s7_scheme *sc, s7_pointer f, int32_t args, s7_pointer expr, bool ops)
{
  if (args == 2)
    {
      if (ops)
	return(chooser_check_arg_types(sc, cadr(expr), caddr(expr), sc->multiply_2,
				       sc->mul_2_ff, sc->mul_2_ii, sc->mul_2_if, sc->mul_2_fi,
				       sc->mul_2_xi, sc->mul_2_ix, sc->mul_2_fx, sc->mul_2_xf));
      return(sc->multiply_2);
    }
  return(f);
}

