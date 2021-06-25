static s7_pointer truncate_p_p(s7_scheme *sc, s7_pointer x)
{
  switch (type(x))
    {
    case T_INTEGER:
      return(x);

    case T_RATIO:
      return(make_integer(sc, (s7_int)(numerator(x) / denominator(x)))); /* C "/" already truncates (but this divide is not accurate over e13) */

    case T_REAL:
      {
	s7_double z;
	z = real(x);
	if (is_NaN(z))
	  return(simple_out_of_range(sc, sc->truncate_symbol, x, its_nan_string));
	if (is_inf(z))
	  return(simple_out_of_range(sc, sc->truncate_symbol, x, its_infinite_string));
#if WITH_GMP
	if (fabs(z) > DOUBLE_TO_INT64_LIMIT)
	  {
	    mpfr_set_d(sc->mpfr_1, real(x), MPFR_RNDN);
	    mpfr_get_z(sc->mpz_1, sc->mpfr_1, MPFR_RNDZ);
	    return(mpz_to_integer(sc, sc->mpz_1));
	  }
#else
	if (fabs(z) > DOUBLE_TO_INT64_LIMIT)
	  return(simple_out_of_range(sc, sc->truncate_symbol, x, its_too_large_string));
#endif
	return((z > 0.0) ? make_integer(sc, (s7_int)floor(z)) : make_integer(sc, (s7_int)ceil(z)));
      }

#if WITH_GMP
    case T_BIG_INTEGER:
      return(x);

    case T_BIG_RATIO:
      mpz_tdiv_q(sc->mpz_1, mpq_numref(big_ratio(x)), mpq_denref(big_ratio(x)));
      return(mpz_to_integer(sc, sc->mpz_1));

    case T_BIG_REAL:
      if (mpfr_nan_p(big_real(x)))
	return(simple_out_of_range(sc, sc->truncate_symbol, x, its_nan_string));
      if (mpfr_inf_p(big_real(x)))
	return(simple_out_of_range(sc, sc->truncate_symbol, x, its_infinite_string));
      mpfr_get_z(sc->mpz_1, big_real(x), MPFR_RNDZ);
      return(mpz_to_integer(sc, sc->mpz_1));

    case T_BIG_COMPLEX:
#endif
    case T_COMPLEX:
    default:
      return(method_or_bust_one_arg_p(sc, x, sc->truncate_symbol, T_REAL));
    }
}

static s7_pointer g_truncate(s7_scheme *sc, s7_pointer args)
{
  #define H_truncate "(truncate x) returns the integer closest to x toward 0"
  #define Q_truncate s7_make_signature(sc, 2, sc->is_integer_symbol, sc->is_real_symbol)
  return(truncate_p_p(sc, car(args)));
}

static s7_int truncate_i_i(s7_int i) {return(i);}

#if (!WITH_GMP)
static s7_int truncate_i_7d(s7_scheme *sc, s7_double x)
{
  if (is_NaN(x))
    simple_out_of_range(sc, sc->truncate_symbol, wrap_real1(sc, x), its_nan_string);
  if (is_inf(x))
    simple_out_of_range(sc, sc->truncate_symbol, wrap_real1(sc, x), its_infinite_string);
  if (fabs(x) > DOUBLE_TO_INT64_LIMIT)
    simple_out_of_range(sc, sc->truncate_symbol, wrap_real1(sc, x), its_too_large_string);
  return((x > 0.0) ? (s7_int)floor(x) : (s7_int)ceil(x));
}
#endif
