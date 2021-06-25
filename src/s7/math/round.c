static s7_double r5rs_round(s7_double x)
{
  s7_double fl, ce, dfl, dce;
  fl = floor(x);
  ce = ceil(x);
  dfl = x - fl;
  dce = ce - x;
  if (dfl > dce) return(ce);
  if (dfl < dce) return(fl);
  return((fmod(fl, 2.0) == 0.0) ? fl : ce);
}

static s7_pointer g_round(s7_scheme *sc, s7_pointer args)
{
  #define H_round "(round x) returns the integer closest to x"
  #define Q_round s7_make_signature(sc, 2, sc->is_integer_symbol, sc->is_real_symbol)

  s7_pointer x;
  x = car(args);
  switch (type(x))
    {
    case T_INTEGER:
      return(x);

    case T_RATIO:
      {
	s7_int truncated, remains;
	long_double frac;

	truncated = numerator(x) / denominator(x);
	remains = numerator(x) % denominator(x);
	frac = s7_fabsl((long_double)remains / (long_double)denominator(x));

	if ((frac > 0.5) ||
	    ((frac == 0.5) &&
	     (truncated % 2 != 0)))
	  return((numerator(x) < 0) ? make_integer(sc, truncated - 1) : make_integer(sc, truncated + 1));
	return(make_integer(sc, truncated));
      }

    case T_REAL:
      {
	s7_double z;
	z = real(x);
	if (is_NaN(z))
	  return(simple_out_of_range(sc, sc->round_symbol, x, its_nan_string));
	if (is_inf(z))
	  return(simple_out_of_range(sc, sc->round_symbol, x, its_infinite_string));
#if WITH_GMP
	if (fabs(z) > DOUBLE_TO_INT64_LIMIT)
	  {
	    mpfr_set_d(sc->mpfr_1, z, MPFR_RNDN);
	    mpfr_rint(sc->mpfr_2, sc->mpfr_1, MPFR_RNDN); /* mpfr_roundeven in mpfr 4.0.0 */
	    mpfr_get_z(sc->mpz_3, sc->mpfr_2, MPFR_RNDN);
	    return(mpz_to_integer(sc, sc->mpz_3));
	  }
#else
	if (fabs(z) > DOUBLE_TO_INT64_LIMIT)
	  return(simple_out_of_range(sc, sc->round_symbol, x, its_too_large_string));
#endif
	return(make_integer(sc, (s7_int)r5rs_round(z)));
      }

#if WITH_GMP
      case T_BIG_INTEGER:
	return(x);

    case T_BIG_RATIO:
      {
	int32_t rnd;
	mpz_fdiv_qr(sc->mpz_1, sc->mpz_2, mpq_numref(big_ratio(x)), mpq_denref(big_ratio(x)));
	mpz_mul_ui(sc->mpz_2, sc->mpz_2, 2);
	rnd = mpz_cmpabs(sc->mpz_2, mpq_denref(big_ratio(x)));
	mpz_fdiv_q(sc->mpz_2, sc->mpz_2, mpq_denref(big_ratio(x)));
	if (rnd > 0)
	  mpz_add(sc->mpz_1, sc->mpz_1, sc->mpz_2);
	else
	  if ((rnd == 0) &&
	      (mpz_odd_p(sc->mpz_1)))
	    mpz_add_ui(sc->mpz_1, sc->mpz_1, 1);
	return(mpz_to_integer(sc, sc->mpz_1));
      }

    case T_BIG_REAL:
      if (mpfr_nan_p(big_real(x)))
	return(simple_out_of_range(sc, sc->round_symbol, x, its_nan_string));
      if (mpfr_inf_p(big_real(x)))
	return(simple_out_of_range(sc, sc->round_symbol, x, its_infinite_string));
      mpfr_set(sc->mpfr_1, big_real(x), MPFR_RNDN);
      mpfr_rint(sc->mpfr_2, sc->mpfr_1, MPFR_RNDN);
      mpfr_get_z(sc->mpz_3, sc->mpfr_2, MPFR_RNDN);
      return(mpz_to_integer(sc, sc->mpz_3));

    case T_BIG_COMPLEX:
#endif
    case T_COMPLEX:
    default:
      return(method_or_bust_one_arg(sc, x, sc->round_symbol, args, T_REAL));
    }
}

static s7_int round_i_i(s7_int i) {return(i);}

#if (!WITH_GMP)
static s7_int round_i_7d(s7_scheme *sc, s7_double z)
{
  if (is_NaN(z))
    simple_out_of_range(sc, sc->round_symbol, wrap_real1(sc, z), its_nan_string);
  if ((is_inf(z)) ||
      (z > DOUBLE_TO_INT64_LIMIT) || (z < -DOUBLE_TO_INT64_LIMIT))
    simple_out_of_range(sc, sc->round_symbol, wrap_real1(sc, z), its_too_large_string);
  return((s7_int)r5rs_round(z));
}
#endif

