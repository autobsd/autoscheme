#define min_out_x(Sc, X, Y) method_or_bust_pp(Sc, X, Sc->min_symbol, X, Y, T_REAL, 1)
#define min_out_y(Sc, X, Y) method_or_bust_pp(Sc, Y, Sc->min_symbol, X, Y, T_REAL, 2)

static s7_pointer min_p_pp(s7_scheme *sc, s7_pointer x, s7_pointer y)
{
  if (type(x) == type(y))
    {
      if (is_t_integer(x))
	return((integer(x) > integer(y)) ? y : x);
      if (is_t_real(x))
	return(((is_NaN(real(x))) || (real(x) <= real(y))) ? x : y);
      if (is_t_ratio(x))
	return((fraction(x) > fraction(y)) ? y : x);
#if WITH_GMP
      if (is_t_big_integer(x))
	return((mpz_cmp(big_integer(x), big_integer(y)) > 0) ? y : x);
      if (is_t_big_ratio(x))
	return((mpq_cmp(big_ratio(x), big_ratio(y)) > 0) ? y : x);
      if (is_t_big_real(x))
	return(((mpfr_nan_p(big_real(x)) != 0) || (mpfr_lessequal_p(big_real(x), big_real(y)))) ? x : y); /* ?? */
#endif
    }
  switch (type(x))
    {
    case T_INTEGER:
      switch (type(y))
	{
	case T_RATIO:       return((integer(x) > fraction(y)) ? y : x);
	case T_REAL:
	  if (is_NaN(real(y))) return(y);
	  return((integer(x) > real(y)) ? y : x);
#if WITH_GMP
	case T_BIG_INTEGER: return((mpz_cmp_si(big_integer(y), integer(x)) > 0) ? x : y);
	case T_BIG_RATIO:   return((mpq_cmp_si(big_ratio(y), integer(x), 1) > 0) ? x : y);
	case T_BIG_REAL:
	  if (mpfr_nan_p(big_real(y))) return(y);
	  return((mpfr_cmp_si(big_real(y), integer(x)) > 0) ? x : y);
#endif
	default:
	  return(min_out_y(sc, x, y));
	}
      break;

    case T_RATIO:
      switch (type(y))
	{
	case T_INTEGER:
	  return((fraction(x) > integer(y)) ? y : x);
	case T_REAL:
	  if (is_NaN(real(y))) return(y);
	  return((fraction(x) > real(y)) ? y : x);
#if WITH_GMP
	case T_BIG_INTEGER:
	  mpq_set_si(sc->mpq_1, numerator(x), denominator(x));
	  return((mpq_cmp_z(sc->mpq_1, big_integer(y)) > 0) ? y : x);
	case T_BIG_RATIO:
	  return((mpq_cmp_si(big_ratio(y), numerator(x), denominator(x)) > 0) ? x : y);
	case T_BIG_REAL:
	  if (mpfr_nan_p(big_real(y))) return(y);
	  mpq_set_si(sc->mpq_1, numerator(x), denominator(x));
	  return((mpfr_cmp_q(big_real(y), sc->mpq_1) > 0) ? x : y);
#endif
	default:
	  return(min_out_y(sc, x, y));
	}

    case T_REAL:
      switch (type(y))
	{
	case T_INTEGER:
	  if (is_NaN(real(x))) return(x);
	  return((real(x) > integer(y)) ? y : x);
	case T_RATIO:
	  return((real(x) > fraction(y)) ? y : x);
#if WITH_GMP
	case T_BIG_INTEGER:
	  if (is_NaN(real(x))) return(x);
	  mpfr_set_d(sc->mpfr_1, real(x), MPFR_RNDN);
	  return((mpfr_cmp_z(sc->mpfr_1, big_integer(y)) > 0) ? y : x);

	case T_BIG_RATIO:
	  if (is_NaN(real(x))) return(x);
	  mpfr_set_d(sc->mpfr_1, real(x), MPFR_RNDN);
	  return((mpfr_cmp_q(sc->mpfr_1, big_ratio(y)) > 0) ? y : x);

	case T_BIG_REAL:
	  if (is_NaN(real(x))) return(x);
	  if (mpfr_nan_p(big_real(y))) return(y);
	  return((mpfr_cmp_d(big_real(y), real(x)) > 0) ? x : y);
#endif
	default:
	  return(min_out_y(sc, x, y));
	}
      break;

#if WITH_GMP
    case T_BIG_INTEGER:
      switch (type(y))
	{
	case T_INTEGER:
	  return((mpz_cmp_si(big_integer(x), integer(y)) > 0) ? y : x);
	case T_RATIO:
	  mpq_set_z(sc->mpq_1, big_integer(x));
	  return((mpq_cmp_si(sc->mpq_1, numerator(y), denominator(y)) > 0) ? y : x);
	case T_REAL:
	  if (is_NaN(real(y))) return(y);
	  mpfr_set_z(sc->mpfr_1, big_integer(x), MPFR_RNDN);
	  return((mpfr_cmp_d(sc->mpfr_1, real(y)) > 0) ? y : x);
	case T_BIG_RATIO:
	  return((mpq_cmp_z(big_ratio(y), big_integer(x)) > 0) ? x : y);
	case T_BIG_REAL:
	  if (mpfr_nan_p(big_real(y))) return(y);
	  return((mpfr_cmp_z(big_real(y), big_integer(x)) > 0) ? x : y);
	default:
	  return(min_out_y(sc, x, y));
	}
    case T_BIG_RATIO:
      switch (type(y))
	{
	case T_INTEGER:
	  return((mpq_cmp_si(big_ratio(x), integer(y), 1) > 0) ? y : x);
	case T_RATIO:
	  return((mpq_cmp_si(big_ratio(x), numerator(y), denominator(y)) > 0) ? y : x);
	case T_REAL:
	  if (is_NaN(real(y))) return(y);
	  mpfr_set_q(sc->mpfr_1, big_ratio(x), MPFR_RNDN);
	  return((mpfr_cmp_d(sc->mpfr_1, real(y)) > 0) ? y : x);
	case T_BIG_INTEGER:
	  return((mpq_cmp_z(big_ratio(x), big_integer(y)) > 0) ? y : x);
	case T_BIG_REAL:
	  if (mpfr_nan_p(big_real(y))) return(y);
	  return((mpfr_cmp_q(big_real(y), big_ratio(x)) > 0) ? x : y);
	default:
	  return(min_out_y(sc, x, y));
	}

    case T_BIG_REAL:
      switch (type(y))
	{
	case T_INTEGER:
	  if (mpfr_nan_p(big_real(x))) return(x);
	  return((mpfr_cmp_si(big_real(x), integer(y)) > 0) ? y : x);
	case T_RATIO:
	  if (mpfr_nan_p(big_real(x))) return(x);
	  mpq_set_si(sc->mpq_1, numerator(y), denominator(y));
	  return((mpfr_cmp_q(big_real(x), sc->mpq_1) > 0) ? y : x);
	case T_REAL:
	  if (mpfr_nan_p(big_real(x))) return(x);
	  if (is_NaN(real(y))) return(y);
	  return((mpfr_cmp_d(big_real(x), real(y)) > 0) ? y : x);
	case T_BIG_INTEGER:
	  if (mpfr_nan_p(big_real(x))) return(x);
	  return((mpfr_cmp_z(big_real(x), big_integer(y)) > 0) ? y : x);
	case T_BIG_RATIO:
	  if (mpfr_nan_p(big_real(x))) return(x);
	  return((mpfr_cmp_q(big_real(x), big_ratio(y)) > 0) ? y : x);
	default:
	  return(min_out_y(sc, x, y));
	}
#endif
    default:
      return(min_out_x(sc, x, y));
    }
  return(x);
}

static s7_pointer g_min(s7_scheme *sc, s7_pointer args)
{
  #define H_min "(min ...) returns the minimum of its arguments"
  #define Q_min sc->pcl_r

  s7_pointer x, p;
  x = car(args);
  if (is_null(cdr(args)))
    {
      if (s7_is_real(x)) return(x);
      return(method_or_bust_p(sc, x, sc->min_symbol, T_REAL));
    }
  for (p = cdr(args); is_pair(p); p = cdr(p))
    x = min_p_pp(sc, x, car(p));
  return(x);
}

static s7_int min_i_ii(s7_int i1, s7_int i2) {return((i1 < i2) ? i1 : i2);}
static s7_int min_i_iii(s7_int i1, s7_int i2, s7_int i3) {return((i1 < i2) ? ((i1 < i3) ? i1 : i3) : ((i2 < i3) ? i2 : i3));}
static s7_double min_d_dd(s7_double x1, s7_double x2) {if (is_NaN(x1)) return(x1); return((x1 < x2) ? x1 : x2);}
static s7_double min_d_ddd(s7_double x1, s7_double x2, s7_double x3) {return(min_d_dd(x1, min_d_dd(x2, x3)));}
static s7_double min_d_dddd(s7_double x1, s7_double x2, s7_double x3, s7_double x4) {return(min_d_dd(x1, min_d_ddd(x2, x3, x4)));}


