static s7_pointer g_ceiling(s7_scheme *sc, s7_pointer args)
{
  #define H_ceiling "(ceiling x) returns the integer closest to x toward inf"
  #define Q_ceiling s7_make_signature(sc, 2, sc->is_integer_symbol, sc->is_real_symbol)

  s7_pointer x;
  x = car(args);
  switch (type(x))
    {
    case T_INTEGER:
      return(x);

    case T_RATIO:
      {
	s7_int val;
	val = numerator(x) / denominator(x);
	return((numerator(x) < 0) ? make_integer(sc, val) : make_integer(sc, val + 1));
      }

    case T_REAL:
      {
	s7_double z;
	z = real(x);
	if (is_NaN(z))
	  return(simple_out_of_range(sc, sc->ceiling_symbol, x, its_nan_string));
	if (is_inf(z))
	  return(simple_out_of_range(sc, sc->ceiling_symbol, x, its_infinite_string));
#if WITH_GMP
	if (fabs(z) > DOUBLE_TO_INT64_LIMIT)
	  {
	    mpfr_set_d(sc->mpfr_1, z, MPFR_RNDN);
	    mpfr_get_z(sc->mpz_1, sc->mpfr_1, MPFR_RNDU);
	    return(mpz_to_integer(sc, sc->mpz_1));
	  }
#else
	if (fabs(z) > DOUBLE_TO_INT64_LIMIT)
	  return(simple_out_of_range(sc, sc->ceiling_symbol, x, its_too_large_string));
#endif
	return(make_integer(sc, (s7_int)ceil(real(x))));
      }

#if WITH_GMP
    case T_BIG_INTEGER:
      return(x);

    case T_BIG_RATIO:
      mpz_cdiv_q(sc->mpz_1, mpq_numref(big_ratio(x)), mpq_denref(big_ratio(x)));
      return(mpz_to_integer(sc, sc->mpz_1));

    case T_BIG_REAL:
      if (mpfr_nan_p(big_real(x)))
	return(simple_out_of_range(sc, sc->ceiling_symbol, x, its_nan_string));
      if (mpfr_inf_p(big_real(x)))
	return(simple_out_of_range(sc, sc->ceiling_symbol, x, its_infinite_string));
      mpfr_get_z(sc->mpz_1, big_real(x), MPFR_RNDU);
      return(mpz_to_integer(sc, sc->mpz_1));

    case T_BIG_COMPLEX:
#endif
    case T_COMPLEX:
    default:
      return(method_or_bust_one_arg(sc, x, sc->ceiling_symbol, args, T_REAL));
    }
}

static s7_int ceiling_i_i(s7_int i) {return(i);}

#if (!WITH_GMP)
static s7_int ceiling_i_7d(s7_scheme *sc, s7_double x)
{
  if (is_NaN(x))
    simple_out_of_range(sc, sc->ceiling_symbol, wrap_real1(sc, x), its_nan_string);
  if ((is_inf(x)) ||
      (x > DOUBLE_TO_INT64_LIMIT) || (x < -DOUBLE_TO_INT64_LIMIT))
    simple_out_of_range(sc, sc->ceiling_symbol, wrap_real1(sc, x), its_too_large_string);
  return((s7_int)ceil(x));
}

static s7_int ceiling_i_7p(s7_scheme *sc, s7_pointer p)
{
  if (is_t_integer(p)) return(integer(p));
  if (is_t_real(p)) return(ceiling_i_7d(sc, real(p)));
  if (is_t_ratio(p)) return((s7_int)(ceil(fraction(p))));
  return(s7_integer_checked(sc, method_or_bust_p(sc, p, sc->ceiling_symbol, T_REAL)));
}
#endif

