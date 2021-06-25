static inline s7_pointer add_if_overflow_to_real_or_big_integer(s7_scheme *sc, s7_int x, s7_int y)
{
#if HAVE_OVERFLOW_CHECKS
  s7_int val;
  if (add_overflow(x, y, &val))
#if WITH_GMP
    {
      mpz_set_si(sc->mpz_1, x);
      mpz_set_si(sc->mpz_2, y);
      mpz_add(sc->mpz_1, sc->mpz_1, sc->mpz_2);
      return(mpz_to_big_integer(sc, sc->mpz_1));
    }
#else
  return(make_real(sc, (long_double)x + (long_double)y));
#endif
  return(make_integer(sc, val));
#else
  return(make_integer(sc, x + y));
#endif
}

static s7_pointer integer_ratio_add_if_overflow_to_real_or_rational(s7_scheme *sc, s7_pointer x, s7_pointer y) /* x: int, y:ratio */
{
#if HAVE_OVERFLOW_CHECKS
  s7_int z;
  if ((multiply_overflow(integer(x), denominator(y), &z)) ||
      (add_overflow(z, numerator(y), &z)))
#if WITH_GMP
    {
      mpz_set_si(sc->mpz_1, integer(x));
      mpz_mul_si(sc->mpz_1, sc->mpz_1, denominator(y));
      mpz_set_si(sc->mpz_2, numerator(y));
      mpz_add(mpq_numref(sc->mpq_1), sc->mpz_2, sc->mpz_1);
      mpz_set_si(mpq_denref(sc->mpq_1), denominator(y));
      return(mpq_to_rational(sc, sc->mpq_1));
    }
#else
    return(make_real(sc, (long_double)integer(x) + fraction(y)));
#endif
    return(s7_make_ratio(sc, z, denominator(y)));
#else
  return(s7_make_ratio(sc, integer(x) * denominator(y) + numerator(y), denominator(y)));
#endif
}

#define parcel_out_fractions(X, Y) do {d1 = denominator(x); n1 = numerator(x); d2 = denominator(y); n2 = numerator(y);} while (0)
/* add_out_x|y here (as in lt_out_x|y) gives a small speed-up, say 3-7 callgrind units, about 2% */

static s7_pointer add_p_pp(s7_scheme *sc, s7_pointer x, s7_pointer y)
{
  /* an experiment: try to avoid the switch statement */
  /* this wins in most s7 cases, not so much elsewhere? parallel subtract/multiply code is slower */
  if (is_t_integer(x))
    {
      if (is_t_integer(y))
	return(add_if_overflow_to_real_or_big_integer(sc, integer(x), integer(y)));
    }
  else
    if (is_t_real(x))
      {
	if (is_t_real(y))
	  return(make_real(sc, real(x) + real(y)));
      }
    else
      if ((is_t_complex(x)) && (is_t_complex(y)))
	return(make_complex(sc, real_part(x) + real_part(y), imag_part(x) + imag_part(y)));

  switch (type(x))
    {
    case T_INTEGER:
      switch (type(y))
	{
	case T_INTEGER:
	  return(add_if_overflow_to_real_or_big_integer(sc, integer(x), integer(y)));
	case T_RATIO:
	  return(integer_ratio_add_if_overflow_to_real_or_rational(sc, x, y));
	case T_REAL:
#if WITH_GMP
	  if (s7_int_abs(integer(x)) >= INT64_TO_DOUBLE_LIMIT) /* (+ 9223372036854775807 .1), >= needed for (+ 9007199254740992 1.0) */
	    {
	      mpfr_set_si(sc->mpfr_1, integer(x), MPFR_RNDN);
	      mpfr_add_d(sc->mpfr_1, sc->mpfr_1, real(y), MPFR_RNDN);
	      return(mpfr_to_big_real(sc, sc->mpfr_1));
	    }
#endif
	  return(make_real(sc, (long_double)integer(x) + real(y)));
	case T_COMPLEX:
	  return(s7_make_complex(sc, (long_double)integer(x) + (long_double)real_part(y), imag_part(y)));
#if WITH_GMP
	case T_BIG_INTEGER:
	  mpz_set_si(sc->mpz_1, integer(x));
	  mpz_add(sc->mpz_1, sc->mpz_1, big_integer(y));
	  return(mpz_to_integer(sc, sc->mpz_1));
	case T_BIG_RATIO:
	  mpq_set_si(sc->mpq_1, integer(x), 1);
	  mpq_add(sc->mpq_1, sc->mpq_1, big_ratio(y));
	  return(mpq_to_rational(sc, sc->mpq_1));
	case T_BIG_REAL:
	  mpfr_add_si(sc->mpfr_1, big_real(y), integer(x), MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	case T_BIG_COMPLEX:
	  mpc_set_si(sc->mpc_1, integer(x), MPC_RNDNN);
	  mpc_add(sc->mpc_1, sc->mpc_1, big_complex(y), MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
#endif
	default:
	  return(method_or_bust_with_type_pp(sc, y, sc->add_symbol, x, y, a_number_string, 2));
	}

    case T_RATIO:
      switch (type(y))
	{
	case T_INTEGER:
	  return(integer_ratio_add_if_overflow_to_real_or_rational(sc, y, x));
	case T_RATIO:
	  {
	    s7_int d1, d2, n1, n2;
	    parcel_out_fractions(x, y);
	    if (d1 == d2)
	      {
#if HAVE_OVERFLOW_CHECKS
		s7_int q;
		if (add_overflow(n1, n2, &q))
#if WITH_GMP
		  {
		    mpq_set_si(sc->mpq_1, n1, d1);
		    mpq_set_si(sc->mpq_2, n2, d2);
		    mpq_add(sc->mpq_1, sc->mpq_1, sc->mpq_2);
		    return(mpq_to_rational(sc, sc->mpq_1));
		  }
#else
		return(make_real(sc, ((long_double)n1 + (long_double)n2) / (long_double)d1));
#endif
	        return(s7_make_ratio(sc, q, d1));
#else
		return(s7_make_ratio(sc, n1 + n2, d1));
#endif
	      }

#if HAVE_OVERFLOW_CHECKS
	    {
	      s7_int n1d2, n2d1, d1d2, q;
	      if ((multiply_overflow(d1, d2, &d1d2)) ||
		  (multiply_overflow(n1, d2, &n1d2)) ||
		  (multiply_overflow(n2, d1, &n2d1)) ||
		  (add_overflow(n1d2, n2d1, &q)))
#if WITH_GMP
		{
		  mpq_set_si(sc->mpq_1, n1, d1);
		  mpq_set_si(sc->mpq_2, n2, d2);
		  mpq_add(sc->mpq_1, sc->mpq_1, sc->mpq_2);
		  return(mpq_to_rational(sc, sc->mpq_1));
		}
#else
	      return(make_real(sc, ((long_double)n1 / (long_double)d1) + ((long_double)n2 / (long_double)d2)));
#endif
	      return(s7_make_ratio(sc, q, d1d2));
	    }
#else
	    return(s7_make_ratio(sc, n1 * d2 + n2 * d1, d1 * d2));
#endif
	  }
	case T_REAL:
	  return(make_real(sc, fraction(x) + real(y)));
	case T_COMPLEX:
	  return(s7_make_complex(sc, fraction(x) + real_part(y), imag_part(y)));
#if WITH_GMP
	case T_BIG_INTEGER:
	  mpq_set_si(sc->mpq_1, numerator(x), denominator(x));
	  mpq_set_z(sc->mpq_2, big_integer(y));
	  mpq_add(sc->mpq_1, sc->mpq_1, sc->mpq_2);
	  return(mpq_to_rational(sc, sc->mpq_1));
	case T_BIG_RATIO:
	  mpq_set_si(sc->mpq_1, numerator(x), denominator(x));
	  mpq_add(sc->mpq_1, sc->mpq_1, big_ratio(y));
	  return(mpq_to_rational(sc, sc->mpq_1));
	case T_BIG_REAL:
	  mpq_set_si(sc->mpq_1, numerator(x), denominator(x));
	  mpfr_add_q(sc->mpfr_1, big_real(y), sc->mpq_1, MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	case T_BIG_COMPLEX:
	  mpq_set_si(sc->mpq_1, numerator(x), denominator(x));
	  mpc_set_q(sc->mpc_1, sc->mpq_1, MPC_RNDNN);
	  mpc_add(sc->mpc_1, sc->mpc_1, big_complex(y), MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
#endif
	default:
	  return(method_or_bust_with_type_pp(sc, y, sc->add_symbol, x, y, a_number_string, 2));
	}

    case T_REAL:
      switch (type(y))
	{
	case T_INTEGER:
#if WITH_GMP
	  if (s7_int_abs(integer(y)) >= INT64_TO_DOUBLE_LIMIT) /* (+ .1 9223372036854775807) */
	    {
	      mpfr_set_si(sc->mpfr_1, integer(y), MPFR_RNDN);
	      mpfr_add_d(sc->mpfr_1, sc->mpfr_1, real(x), MPFR_RNDN);
	      return(mpfr_to_big_real(sc, sc->mpfr_1));
	    }
#endif
	  return(make_real(sc, real(x) + (long_double)integer(y)));
	case T_RATIO:
	  return(make_real(sc, real(x) + fraction(y)));
	case T_REAL:
	  return(make_real(sc, real(x) + real(y)));
	case T_COMPLEX:
	  return(s7_make_complex(sc, real(x) + real_part(y), imag_part(y)));
#if WITH_GMP
	case T_BIG_INTEGER:
	  mpfr_set_d(sc->mpfr_1, real(x), MPFR_RNDN);
	  mpfr_add_z(sc->mpfr_1, sc->mpfr_1, big_integer(y), MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	case T_BIG_RATIO:
	  mpfr_set_d(sc->mpfr_1, real(x), MPFR_RNDN);
	  mpfr_add_q(sc->mpfr_1, sc->mpfr_1, big_ratio(y), MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	case T_BIG_REAL:
	  mpfr_add_d(sc->mpfr_1, big_real(y), real(x), MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	case T_BIG_COMPLEX:
	  mpc_set_d_d(sc->mpc_1, real(x), 0.0, MPC_RNDNN);
	  mpc_add(sc->mpc_1, sc->mpc_1, big_complex(y), MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
#endif
	default:
	  return(method_or_bust_with_type_pp(sc, y, sc->add_symbol, x, y, a_number_string, 2));
	}

    case T_COMPLEX:
      switch (type(y))
	{
	case T_INTEGER:
	  return(s7_make_complex(sc, real_part(x) + integer(y), imag_part(x)));
	case T_RATIO:
	  return(s7_make_complex(sc, real_part(x) + fraction(y), imag_part(x)));
	case T_REAL:
	  return(s7_make_complex(sc, real_part(x) + real(y), imag_part(x)));
	case T_COMPLEX:
	  return(make_complex(sc, real_part(x) + real_part(y), imag_part(x) + imag_part(y)));
#if WITH_GMP
	case T_BIG_INTEGER:
	  mpc_set_d_d(sc->mpc_1, real_part(x), imag_part(x), MPC_RNDNN);
	  mpc_set_z(sc->mpc_2, big_integer(y), MPC_RNDNN);
	  mpc_add(sc->mpc_1, sc->mpc_1, sc->mpc_2, MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	case T_BIG_RATIO:
	  mpc_set_d_d(sc->mpc_1, real_part(x), imag_part(x), MPC_RNDNN);
	  mpc_set_q(sc->mpc_2, big_ratio(y), MPC_RNDNN);
	  mpc_add(sc->mpc_1, sc->mpc_1, sc->mpc_2, MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	case T_BIG_REAL:
	  mpc_set_d_d(sc->mpc_1, real_part(x), imag_part(x), MPC_RNDNN);
	  mpc_add_fr(sc->mpc_1, sc->mpc_1, big_real(y), MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	case T_BIG_COMPLEX:
	  mpc_set_d_d(sc->mpc_1, real_part(x), imag_part(x), MPC_RNDNN);
	  mpc_add(sc->mpc_1, sc->mpc_1, big_complex(y), MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
#endif
	default:
	  return(method_or_bust_with_type_pp(sc, y, sc->add_symbol, x, y, a_number_string, 2));
	}

#if WITH_GMP
    case T_BIG_INTEGER:
      switch (type(y))
	{
	case T_INTEGER:
	  mpz_set_si(sc->mpz_1, integer(y));
	  mpz_add(sc->mpz_1, big_integer(x), sc->mpz_1);
	  return(mpz_to_integer(sc, sc->mpz_1));
	case T_RATIO:
	  mpq_set_z(sc->mpq_2, big_integer(x));
	  mpq_set_si(sc->mpq_1, numerator(y), denominator(y));
	  mpq_add(sc->mpq_1, sc->mpq_2, sc->mpq_1);
	  return(mpq_to_canonicalized_rational(sc, sc->mpq_1));
	case T_REAL:
	  if (is_NaN(real(y))) return(real_NaN);
	  mpfr_set_d(sc->mpfr_1, real(y), MPFR_RNDN);
	  mpfr_add_z(sc->mpfr_1, sc->mpfr_1, big_integer(x), MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	case T_COMPLEX:
	  mpc_set_d_d(sc->mpc_1, real_part(y), imag_part(y), MPC_RNDNN);
	  mpc_set_z(sc->mpc_2, big_integer(x), MPC_RNDNN);
	  mpc_add(sc->mpc_1, sc->mpc_2, sc->mpc_1, MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	case T_BIG_INTEGER:
	  mpz_add(sc->mpz_1, big_integer(x), big_integer(y));
	  return(mpz_to_integer(sc, sc->mpz_1));
	case T_BIG_RATIO:
	  mpq_set_z(sc->mpq_1, big_integer(x));
	  mpq_add(sc->mpq_1, sc->mpq_1, big_ratio(y));
	  return(mpq_to_rational(sc, sc->mpq_1));
	case T_BIG_REAL:
	  mpfr_add_z(sc->mpfr_1, big_real(y), big_integer(x), MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	case T_BIG_COMPLEX:
	  mpc_set_z(sc->mpc_1, big_integer(x), MPC_RNDNN);
	  mpc_add(sc->mpc_1, sc->mpc_1, big_complex(y), MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	default:
	  return(method_or_bust_with_type_pp(sc, y, sc->add_symbol, x, y, a_number_string, 2));
	}

    case T_BIG_RATIO:
      switch (type(y))
	{
	case T_INTEGER:
	  mpq_set_si(sc->mpq_1, integer(y), 1);
	  mpq_add(sc->mpq_1, big_ratio(x), sc->mpq_1);
	  return(mpq_to_rational(sc, sc->mpq_1));
	case T_RATIO:
	  mpq_set_si(sc->mpq_1, numerator(y), denominator(y));
	  mpq_add(sc->mpq_1, big_ratio(x), sc->mpq_1);
	  return(mpq_to_rational(sc, sc->mpq_1));
	case T_REAL:
	  if (is_NaN(real(y))) return(real_NaN);
	  mpfr_set_d(sc->mpfr_1, real(y), MPFR_RNDN);
	  mpfr_add_q(sc->mpfr_1, sc->mpfr_1, big_ratio(x), MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	case T_COMPLEX:
	  mpc_set_q(sc->mpc_1, big_ratio(x), MPC_RNDNN);
	  mpc_set_d_d(sc->mpc_2, real_part(y), imag_part(y), MPC_RNDNN);
	  mpc_add(sc->mpc_1, sc->mpc_1, sc->mpc_2, MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	case T_BIG_INTEGER:
	  mpq_set_z(sc->mpq_1, big_integer(y));
	  mpq_add(sc->mpq_1, big_ratio(x), sc->mpq_1);
	  return(mpq_to_canonicalized_rational(sc, sc->mpq_1));
	case T_BIG_RATIO:
	  mpq_add(sc->mpq_1, big_ratio(x), big_ratio(y));
	  return(mpq_to_canonicalized_rational(sc, sc->mpq_1));
	case T_BIG_REAL:
	  mpfr_add_q(sc->mpfr_1, big_real(y), big_ratio(x), MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	case T_BIG_COMPLEX:
	  mpc_set_q(sc->mpc_1, big_ratio(x), MPC_RNDNN);
	  mpc_add(sc->mpc_1, sc->mpc_1, big_complex(y), MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	default:
	  return(method_or_bust_with_type_pp(sc, y, sc->add_symbol, x, y, a_number_string, 2));
	}

    case T_BIG_REAL:
      switch (type(y))
	{
	case T_INTEGER:
	  mpfr_add_si(sc->mpfr_1, big_real(x), integer(y), MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	case T_RATIO:
	  mpq_set_si(sc->mpq_1, numerator(y), denominator(y));
	  mpfr_add_q(sc->mpfr_1, big_real(x), sc->mpq_1, MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	case T_REAL:
	  if (is_NaN(real(y))) return(real_NaN);
	  mpfr_add_d(sc->mpfr_1, big_real(x), real(y), MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	case T_COMPLEX:
	  mpc_set_d_d(sc->mpc_1, real_part(y), imag_part(y), MPC_RNDNN);
	  mpc_add_fr(sc->mpc_1, sc->mpc_1, big_real(x), MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	case T_BIG_INTEGER:
	  mpfr_add_z(sc->mpfr_1, big_real(x), big_integer(y), MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	case T_BIG_RATIO:
	  mpfr_add_q(sc->mpfr_1, big_real(x), big_ratio(y), MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	case T_BIG_REAL:
	  mpfr_add(sc->mpfr_1, big_real(x), big_real(y), MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	case T_BIG_COMPLEX:
	  mpc_add_fr(sc->mpc_1, big_complex(y), big_real(x), MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	default:
	  return(method_or_bust_with_type_pp(sc, y, sc->add_symbol, x, y, a_number_string, 2));
	}
    case T_BIG_COMPLEX:
      switch (type(y))
	{
	case T_INTEGER:
	  mpc_set_si(sc->mpc_1, integer(y), MPC_RNDNN);
	  mpc_add(sc->mpc_1, big_complex(x), sc->mpc_1, MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	case T_RATIO:
	  mpq_set_si(sc->mpq_1, numerator(y), denominator(y));
	  mpc_set_q(sc->mpc_1, sc->mpq_1, MPC_RNDNN);
	  mpc_add(sc->mpc_1, big_complex(x), sc->mpc_1, MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	case T_REAL:
	  /* if (is_NaN(real(y))) return(real_NaN); */
	  mpc_set_d_d(sc->mpc_1, real(y), 0.0, MPC_RNDNN);
	  mpc_add(sc->mpc_1, big_complex(x), sc->mpc_1, MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	case T_COMPLEX:
	  mpc_set_d_d(sc->mpc_1, real_part(y), imag_part(y), MPC_RNDNN);
	  mpc_add(sc->mpc_1, big_complex(x), sc->mpc_1, MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	case T_BIG_INTEGER:
	  mpc_set_z(sc->mpc_1, big_integer(y), MPC_RNDNN);
	  mpc_add(sc->mpc_1, big_complex(x), sc->mpc_1, MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	case T_BIG_RATIO:
	  mpc_set_q(sc->mpc_1, big_ratio(y), MPC_RNDNN);
	  mpc_add(sc->mpc_1, big_complex(x), sc->mpc_1, MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	case T_BIG_REAL:
	  mpc_add_fr(sc->mpc_1, big_complex(x), big_real(y), MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	case T_BIG_COMPLEX:
	  mpc_add(sc->mpc_1, big_complex(x), big_complex(y), MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	default:
	  return(method_or_bust_with_type_pp(sc, y, sc->add_symbol, x, y, a_number_string, 2));
	}
#endif
      default:
	return(method_or_bust_with_type_pp(sc, x, sc->add_symbol, x, y, a_number_string, 1));
    }
}

static s7_pointer add_p_ppp(s7_scheme *sc, s7_pointer x, s7_pointer y, s7_pointer z) {return(add_p_pp(sc, add_p_pp(sc, x, y), z));}

static s7_pointer g_add(s7_scheme *sc, s7_pointer args)
{
  #define H_add "(+ ...) adds its arguments"
  #define Q_add sc->pcl_n

  s7_pointer x, p;
  if (is_null(args))
    return(int_zero);
  x = car(args);
  p = cdr(args);
  if (is_null(p))
    {
      if (!is_number(x))
	return(method_or_bust_with_type_one_arg(sc, x, sc->add_symbol, args, a_number_string));
      return(x);
    }
  if (is_null(cdr(p)))
    return(add_p_pp(sc, x, car(p)));
  for (; is_pair(p); p = cdr(p))
    x = add_p_pp(sc, x, car(p));
  return(x);
}

static s7_pointer g_add_2(s7_scheme *sc, s7_pointer args) {return(add_p_pp(sc, car(args), cadr(args)));}

static s7_pointer g_add_3(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p0, p1, p2;
  p0 = car(args);
  p1 = cadr(args);
  p2 = caddr(args);
  if ((is_t_integer(p0)) && (is_t_integer(p1)) && (is_t_integer(p2)))
    {
#if HAVE_OVERFLOW_CHECKS
      s7_int val;
      if ((!add_overflow(integer(p0), integer(p1), &val)) &&
	  (!add_overflow(val, integer(p2), &val)))
	return(make_integer(sc, val));
#if WITH_GMP
      mpz_set_si(sc->mpz_1, integer(p0));
      mpz_set_si(sc->mpz_2, integer(p1));
      mpz_add(sc->mpz_1, sc->mpz_1, sc->mpz_2);
      mpz_set_si(sc->mpz_2, integer(p2));
      mpz_add(sc->mpz_1, sc->mpz_1, sc->mpz_2);
      return(mpz_to_integer(sc, sc->mpz_1));
#else
      return(make_real(sc, (long_double)integer(p0) + (long_double)integer(p1) + (long_double)integer(p2)));
#endif
#else
      return(make_integer(sc, integer(p0) + integer(p1) + integer(p2)));
#endif
    }
  if ((is_t_real(p0)) && (is_t_real(p1)) && (is_t_real(p2)))
    return(make_real(sc, real(p0) + real(p1) + real(p2)));
  return(add_p_pp(sc, add_p_pp(sc, p0, p1), p2));
}
/* trade-off in add_3: time saved by using add_p_pp, but it conses up a new number cell, so subsequent gc can overwhelm the gains, and add add_p_pp overhead
 *   need int wrap as output or reuse-if-known-temp, or perhaps free if not permanent
 */

static s7_pointer g_add_x1_1(s7_scheme *sc, s7_pointer x, int pos)
{
  if (is_t_integer(x))
    return(add_if_overflow_to_real_or_big_integer(sc, integer(x), 1));

  switch (type(x))
    {
    case T_RATIO:   return(add_p_pp(sc, x, int_one));
    case T_REAL:    return(make_real(sc, real(x) + 1.0));
    case T_COMPLEX: return(s7_make_complex(sc, real_part(x) + 1.0, imag_part(x)));
#if WITH_GMP
    case T_BIG_INTEGER:
      mpz_set_si(sc->mpz_1, 1);
      mpz_add(sc->mpz_1, big_integer(x), sc->mpz_1);
      return(mpz_to_integer(sc, sc->mpz_1));
    case T_BIG_RATIO:
    case T_BIG_REAL:
    case T_BIG_COMPLEX:
      return(add_p_pp(sc, x, int_one));
#endif
    default:
      return(method_or_bust_with_type(sc, x, sc->add_symbol,
				      (pos == 1) ? set_plist_2(sc, x, int_one) : set_plist_2(sc, int_one, x),
				      a_number_string, pos));
    }
  return(x);
}

#if WITH_GMP
static s7_pointer g_add_x1(s7_scheme *sc, s7_pointer args) {return(g_add_x1_1(sc, car(args), 1));}
#else
static s7_pointer g_add_x1(s7_scheme *sc, s7_pointer args)
{
  s7_pointer x;
  x = car(args);
  if (is_t_integer(x)) return(make_integer(sc, integer(x) + 1));
  if (is_t_real(x)) return(make_real(sc, real(x) + 1.0));
  if (is_t_complex(x)) return(s7_make_complex(sc, real_part(x) + 1.0, imag_part(x)));
  return(add_p_pp(sc, x, int_one));
}
#endif
static s7_pointer g_add_1x(s7_scheme *sc, s7_pointer args) {return(g_add_x1_1(sc, cadr(args), 2));}

static s7_pointer g_add_xi(s7_scheme *sc, s7_pointer x, s7_int y)
{
  if (is_t_integer(x))
    return(add_if_overflow_to_real_or_big_integer(sc, integer(x), y));

  switch (type(x))
    {
    case T_RATIO:   return(add_p_pp(sc, x, wrap_integer1(sc, y)));
    case T_REAL:    return(make_real(sc, real(x) + y));
    case T_COMPLEX: return(s7_make_complex(sc, real_part(x) + y, imag_part(x)));
#if WITH_GMP
    case T_BIG_INTEGER:
      mpz_set_si(sc->mpz_1, y);
      mpz_add(sc->mpz_1, big_integer(x), sc->mpz_1);
      return(mpz_to_integer(sc, sc->mpz_1));
    case T_BIG_RATIO:
    case T_BIG_REAL:
    case T_BIG_COMPLEX:
      return(add_p_pp(sc, x, wrap_integer1(sc, y)));
#endif
    default: return(method_or_bust_with_type_pi(sc, x, sc->add_symbol, x, y, a_number_string));
    }
  return(x);
}

static s7_pointer g_add_xf(s7_scheme *sc, s7_pointer x, s7_double y)
{
  if (is_t_real(x)) return(make_real(sc, real(x) + y));
  switch (type(x))
    {
    case T_INTEGER: return(make_real(sc, integer(x) + y));
    case T_RATIO:   return(make_real(sc, fraction(x) + y));
    case T_COMPLEX: return(s7_make_complex(sc, real_part(x) + y, imag_part(x)));
#if WITH_GMP
    case T_BIG_INTEGER: case T_BIG_RATIO: case T_BIG_REAL: case T_BIG_COMPLEX:
      return(add_p_pp(sc, x, wrap_real1(sc, y)));
#endif
    default: return(method_or_bust_with_type_pf(sc, x, sc->add_symbol, x, y, a_number_string));
    }
  return(x);
}

static s7_pointer g_add_2_ff(s7_scheme *sc, s7_pointer args)
{
#if WITH_GMP
  if ((is_t_real(car(args))) && (is_t_real(cadr(args))))
    return(make_real(sc, real(car(args)) + real(cadr(args))));
  return(add_p_pp(sc, car(args), cadr(args)));
#else
  return(make_real(sc, real(car(args)) + real(cadr(args))));
#endif
}

static s7_pointer g_add_2_ii(s7_scheme *sc, s7_pointer args)
{
#if WITH_GMP
  if ((is_t_integer(car(args))) && (is_t_integer(cadr(args))))
#endif
    return(add_if_overflow_to_real_or_big_integer(sc, integer(car(args)), integer(cadr(args))));
#if WITH_GMP
  return(g_add(sc, args)); /* possibly bigint? */
#endif
}

#if WITH_GMP
static s7_pointer add_2_if(s7_scheme *sc, s7_pointer x, s7_pointer y)
{
  if ((is_t_integer(x)) && (is_t_real(y)))
    {
      if (s7_int_abs(integer(x)) >= INT64_TO_DOUBLE_LIMIT)
	{
	  mpfr_set_si(sc->mpfr_1, integer(x), MPFR_RNDN);
	  mpfr_add_d(sc->mpfr_1, sc->mpfr_1, real(y), MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	}
      return(make_real(sc, integer(x) + real(y)));
    }
  return(add_p_pp(sc, x, y));
}

static s7_pointer g_add_2_if(s7_scheme *sc, s7_pointer args) {return(add_2_if(sc, car(args), cadr(args)));}
static s7_pointer g_add_2_fi(s7_scheme *sc, s7_pointer args) {return(add_2_if(sc, cadr(args), car(args)));}

static s7_pointer g_add_2_xi(s7_scheme *sc, s7_pointer args) {if (is_t_integer(cadr(args))) return(g_add_xi(sc, car(args), integer(cadr(args)))); return(g_add(sc, args));}
static s7_pointer g_add_2_ix(s7_scheme *sc, s7_pointer args) {if (is_t_integer(car(args))) return(g_add_xi(sc, cadr(args), integer(car(args)))); return(g_add(sc, args));}
static s7_pointer g_add_2_xf(s7_scheme *sc, s7_pointer args) {if (is_t_real(cadr(args))) return(g_add_xf(sc, car(args), real(cadr(args)))); return(g_add(sc, args));}
static s7_pointer g_add_2_fx(s7_scheme *sc, s7_pointer args) {if (is_t_real(car(args))) return(g_add_xf(sc, cadr(args), real(car(args)))); return(g_add(sc, args));}

#else

static s7_pointer g_add_2_if(s7_scheme *sc, s7_pointer args) {return(make_real(sc, integer(car(args)) + real(cadr(args))));}
static s7_pointer g_add_2_fi(s7_scheme *sc, s7_pointer args) {return(make_real(sc, real(car(args)) + integer(cadr(args))));}
static s7_pointer g_add_2_xi(s7_scheme *sc, s7_pointer args) {return(g_add_xi(sc, car(args), integer(cadr(args))));}
static s7_pointer g_add_2_ix(s7_scheme *sc, s7_pointer args) {return(g_add_xi(sc, cadr(args), integer(car(args))));}
static s7_pointer g_add_2_xf(s7_scheme *sc, s7_pointer args) {return(g_add_xf(sc, car(args), real(cadr(args))));}
static s7_pointer g_add_2_fx(s7_scheme *sc, s7_pointer args) {return(g_add_xf(sc, cadr(args), real(car(args))));}
#endif

static s7_pointer add_p_dd(s7_scheme *sc, s7_double x1, s7_double x2) {return(make_real(sc, x1 + x2));}
/* add_p_ii and add_d_id unhittable apparently -- this (d_id) is due to the order of d_dd_ok and d_id_ok in float_optimize,
 *   but d_dd is much more often hit, and the int arg (if constant) is turned into a float in d_dd
 */
static s7_double add_d_d(s7_double x) {return(x);}
static s7_double add_d_dd(s7_double x1, s7_double x2) {return(x1 + x2);}
static s7_double add_d_ddd(s7_double x1, s7_double x2, s7_double x3) {return(x1 + x2 + x3);}
static s7_double add_d_dddd(s7_double x1, s7_double x2, s7_double x3, s7_double x4) {return(x1 + x2 + x3 + x4);}

static s7_int add_i_ii(s7_int i1, s7_int i2) {return(i1 + i2);}
static s7_int add_i_iii(s7_int i1, s7_int i2, s7_int i3) {return(i1 + i2 + i3);}

static s7_pointer argument_type(s7_scheme *sc, s7_pointer arg1)
{
  if (is_pair(arg1))
    {
      if (car(arg1) == sc->quote_symbol)
	return((is_pair(cdr(arg1))) ? s7_type_of(sc, cadr(arg1)) : NULL);    /* arg1 = (quote) */

      if ((is_h_optimized(arg1)) &&
	  (is_safe_c_op(optimize_op(arg1))) &&
	  (is_c_function(opt1_cfunc(arg1))))
	{
	  s7_pointer sig;
	  sig = c_function_signature(opt1_cfunc(arg1));
	  if ((sig) &&
	      (is_pair(sig)) &&
	      (is_symbol(car(sig))))
	    return(car(sig));
	}
      /* perhaps add closure sig if we can depend on it (immutable func etc) */
    }
  else
    if (!is_symbol(arg1))
      return(s7_type_of(sc, arg1));
  return(NULL);
}

static s7_pointer chooser_check_arg_types(s7_scheme *sc, s7_pointer arg1, s7_pointer arg2, s7_pointer fallback,
					  s7_pointer f_2_ff, s7_pointer f_2_ii, s7_pointer f_2_if, s7_pointer f_2_fi,
					  s7_pointer f_2_xi, s7_pointer f_2_ix, s7_pointer f_2_fx, s7_pointer f_2_xf)
{
  s7_pointer arg1_type, arg2_type;
  arg1_type = argument_type(sc, arg1);
  arg2_type = argument_type(sc, arg2);
  if ((arg1_type) || (arg2_type))
    {
      if (arg1_type == sc->is_float_symbol)
	{
	  if (arg2_type == sc->is_float_symbol)
	    return(f_2_ff);
	  return((arg2_type == sc->is_integer_symbol) ? f_2_fi : f_2_fx);
	}
      if (arg1_type == sc->is_integer_symbol)
	{
	  if (arg2_type == sc->is_float_symbol)
	    return(f_2_if);
	  return((arg2_type == sc->is_integer_symbol) ? f_2_ii : f_2_ix);
	}
      if (arg2_type == sc->is_float_symbol)
	return(f_2_xf);
      if (arg2_type == sc->is_integer_symbol)
	return(f_2_xi);
    }
  return(fallback);
}

static s7_pointer g_random_i(s7_scheme *sc, s7_pointer args);

static s7_pointer add_chooser(s7_scheme *sc, s7_pointer f, int32_t args, s7_pointer expr, bool ops)
{
  /* (+ s f) (+ (* s s) s) (+ s s) (+ s (* s s)) */
  if (args == 2)
    {
      if (ops)
	{
	  s7_pointer arg1, arg2;
	  arg1 = cadr(expr);
	  arg2 = caddr(expr);
	  if (arg2 == int_one)                          /* (+ ... 1) */
	    return(sc->add_x1);
	  if ((is_t_integer(arg1)) && ((is_pair(arg2)) && (is_optimized(arg2)) && (is_h_safe_c_d(arg2)) && (fn_proc(arg2) == g_random_i)))
	    {
	      set_opt3_int(cdr(expr), cadr(arg2));
	      set_safe_optimize_op(expr, HOP_SAFE_C_NC); /* op if r op? */
	      return(sc->add_i_random);
	    }
	  if (arg1 == int_one)
	    return(sc->add_1x);
	  return(chooser_check_arg_types(sc, arg1, arg2, sc->add_2,
					 sc->add_2_ff, sc->add_2_ii, sc->add_2_if, sc->add_2_fi,
					 sc->add_2_xi, sc->add_2_ix, sc->add_2_fx, sc->add_2_xf));
	}
      return(sc->add_2);
    }
  return((args == 3) ? sc->add_3 : f);
}
