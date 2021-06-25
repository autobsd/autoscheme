static bool lt_out_x(s7_scheme *sc, s7_pointer x, s7_pointer y)
{
  if (has_active_methods(sc, x))
    return(find_and_apply_method(sc, x, sc->lt_symbol, set_plist_2(sc, x, y)) != sc->F);
  wrong_type_argument(sc, sc->lt_symbol, 1, x, T_REAL);
  return(false);
}

static bool lt_out_y(s7_scheme *sc, s7_pointer x, s7_pointer y)
{
  if (has_active_methods(sc, y))
    return(find_and_apply_method(sc, y, sc->lt_symbol, set_plist_2(sc, x, y)) != sc->F);
  wrong_type_argument(sc, sc->lt_symbol, 2, y, T_REAL);
  return(false);
}

static bool lt_b_7pp(s7_scheme *sc, s7_pointer x, s7_pointer y)
{
  if (type(x) == type(y))
    {
      if (is_t_integer(x))
	return(integer(x) < integer(y));
      if (is_t_real(x))
	return(real(x) < real(y));
      if (is_t_ratio(x))
	return(fraction(x) < fraction(y));
#if WITH_GMP
      if (is_t_big_integer(x))
	return(mpz_cmp(big_integer(x), big_integer(y)) < 0);
      if (is_t_big_ratio(x))
	return(mpq_cmp(big_ratio(x), big_ratio(y)) < 0);
      if (is_t_big_real(x))
	return(mpfr_less_p(big_real(x), big_real(y)));
#endif
    }
  switch (type(x))
    {
    case T_INTEGER:
      switch (type(y))
	{
	case T_RATIO:	return(integer(x) < fraction(y)); /* ?? */
	case T_REAL:	return(integer(x) < real(y));
#if WITH_GMP
	case T_BIG_INTEGER: return(mpz_cmp_si(big_integer(y), integer(x)) > 0);
	case T_BIG_RATIO:   return(mpq_cmp_si(big_ratio(y), integer(x), 1) > 0);
	case T_BIG_REAL:    return(mpfr_cmp_si(big_real(y), integer(x)) > 0);
#endif
	default: return(lt_out_y(sc, x, y));
	}
      break;

    case T_RATIO:
      switch (type(y))
	{
	case T_INTEGER: return(fraction(x) < integer(y));
	case T_REAL:    return(fraction(x) < real(y));
#if WITH_GMP
	case T_BIG_INTEGER:
	  mpq_set_si(sc->mpq_1, numerator(x), denominator(x));
	  return(mpq_cmp_z(sc->mpq_1, big_integer(y)) < 0);
	case T_BIG_RATIO:
	  return(mpq_cmp_si(big_ratio(y), numerator(x), denominator(x)) > 0);
	case T_BIG_REAL:
	  mpq_set_si(sc->mpq_1, numerator(x), denominator(x));
	  return(mpfr_cmp_q(big_real(y), sc->mpq_1) > 0);
#endif
	default: return(lt_out_y(sc, x, y));
	}

    case T_REAL:
      switch (type(y))
	{
	case T_INTEGER: return(real(x) < integer(y));
	case T_RATIO:	return(real(x) < fraction(y));
#if WITH_GMP
	case T_BIG_INTEGER:
	  if (is_NaN(real(x))) return(false);
	  mpfr_set_d(sc->mpfr_1, real(x), MPFR_RNDN);
	  return(mpfr_cmp_z(sc->mpfr_1, big_integer(y)) < 0);

	case T_BIG_RATIO:
	  mpfr_set_d(sc->mpfr_1, real(x), MPFR_RNDN);
	  return(mpfr_cmp_q(sc->mpfr_1, big_ratio(y)) < 0);

	case T_BIG_REAL:
	  return(mpfr_cmp_d(big_real(y), real(x)) > 0);
#endif
	default: return(lt_out_y(sc, x, y));
	}
      break;

#if WITH_GMP
    case T_BIG_INTEGER:
      switch (type(y))
	{
	case T_INTEGER:
	  return(mpz_cmp_si(big_integer(x), integer(y)) < 0);
	case T_RATIO:
	  mpq_set_z(sc->mpq_1, big_integer(x));
	  return(mpq_cmp_si(sc->mpq_1, numerator(y), denominator(y)) < 0);
	case T_REAL:
	  if (is_NaN(real(y))) return(false);
	  mpfr_set_z(sc->mpfr_1, big_integer(x), MPFR_RNDN);
	  return(mpfr_cmp_d(sc->mpfr_1, real(y)) < 0);
	case T_BIG_RATIO:
	  return(mpq_cmp_z(big_ratio(y), big_integer(x)) > 0);
	case T_BIG_REAL:
	  return(mpfr_cmp_z(big_real(y), big_integer(x)) > 0);
	default: return(lt_out_y(sc, x, y));
	}
    case T_BIG_RATIO:
      switch (type(y))
	{
	case T_INTEGER:
	  return(mpq_cmp_si(big_ratio(x), integer(y), 1) < 0);
	case T_RATIO:
	  return(mpq_cmp_si(big_ratio(x), numerator(y), denominator(y)) < 0);
	case T_REAL:
	  if (is_NaN(real(y))) return(false);
	  mpfr_set_q(sc->mpfr_1, big_ratio(x), MPFR_RNDN);
	  return(mpfr_cmp_d(sc->mpfr_1, real(y)) < 0);
	case T_BIG_INTEGER:
	  return(mpq_cmp_z(big_ratio(x), big_integer(y)) < 0);
	case T_BIG_REAL:
	  return(mpfr_cmp_q(big_real(y), big_ratio(x)) > 0);
	default: return(lt_out_y(sc, x, y));
	}

    case T_BIG_REAL:
      switch (type(y))
	{
	case T_INTEGER:
	  return(mpfr_cmp_si(big_real(x), integer(y)) < 0);
	case T_RATIO:
	  mpq_set_si(sc->mpq_1, numerator(y), denominator(y));
	  return(mpfr_cmp_q(big_real(x), sc->mpq_1) < 0);
	case T_REAL:
	  return(mpfr_cmp_d(big_real(x), real(y)) < 0);
	case T_BIG_INTEGER:
	  return(mpfr_cmp_z(big_real(x), big_integer(y)) < 0);
	case T_BIG_RATIO:
	  return(mpfr_cmp_q(big_real(x), big_ratio(y)) < 0);
	default: return(lt_out_y(sc, x, y));
	}
#endif
    default: return(lt_out_x(sc, x, y));
    }
  return(true);
}

static s7_pointer g_less(s7_scheme *sc, s7_pointer args)
{
  #define H_less "(< x1 ...) returns #t if its arguments are in increasing order"
  #define Q_less s7_make_circular_signature(sc, 1, 2, sc->is_boolean_symbol, sc->is_real_symbol)

  s7_pointer x, p;
  x = car(args);
  p = cdr(args);
  if (is_null(cdr(p)))
    return(make_boolean(sc, lt_b_7pp(sc, x, car(p))));

  for (; is_pair(p); p = cdr(p))
    {
      if (!lt_b_7pp(sc, x, car(p)))
	{
	  for (p = cdr(p); is_pair(p); p = cdr(p))
	    if (!is_real_via_method(sc, car(p)))
	      return(wrong_type_argument(sc, sc->lt_symbol, position_of(p, args), car(p), T_REAL));
	  return(sc->F);
	}
      x = car(p);
    }
  return(sc->T);
}

static bool ratio_lt_pi(s7_pointer x, s7_int y)
{
  if ((y >= 0) && (numerator(x) < 0))
    return(true);
  if ((y <= 0) && (numerator(x) > 0))
    return(false);
  if (denominator(x) < S7_INT32_MAX)
    return(numerator(x) < (y * denominator(x)));
  return(fraction(x) < y);
}

static s7_pointer g_less_x0(s7_scheme *sc, s7_pointer args)
{
  s7_pointer x;
  x = car(args);
  if (is_t_integer(x))
    return(make_boolean(sc, integer(x) < 0));
  if (is_small_real(x))
    return(make_boolean(sc, s7_is_negative(x)));
#if WITH_GMP
  if (is_t_big_integer(x))
    return(make_boolean(sc, mpz_cmp_si(big_integer(x), 0) < 0));
  if (is_t_big_real(x))
    return(make_boolean(sc, mpfr_cmp_si(big_real(x), 0) < 0));
  if (is_t_big_ratio(x))
    return(make_boolean(sc, mpq_cmp_si(big_ratio(x), 0, 1) < 0));
#endif
  return(method_or_bust(sc, x, sc->lt_symbol, args, T_REAL, 1));
}

static s7_pointer g_less_xi(s7_scheme *sc, s7_pointer args)
{
  s7_int y;
  s7_pointer x;

  x = car(args);
  y = integer(cadr(args));

  if (is_t_integer(x))
    return(make_boolean(sc, integer(x) < y));
  if (is_t_real(x))
    return(make_boolean(sc, real(x) < y));
  if (is_t_ratio(x))
    return(make_boolean(sc, ratio_lt_pi(x, y)));
#if WITH_GMP
  if (is_t_big_integer(x))
    return(make_boolean(sc, mpz_cmp_si(big_integer(x), y) < 0));
  if (is_t_big_real(x))
    return(make_boolean(sc, mpfr_cmp_si(big_real(x), y) < 0));
  if (is_t_big_ratio(x))
    return(make_boolean(sc, mpq_cmp_si(big_ratio(x), y, 1) < 0));
#endif
  return(method_or_bust(sc, x, sc->lt_symbol, args, T_REAL, 1));
}

static s7_pointer g_less_xf(s7_scheme *sc, s7_pointer args)
{
  s7_double y;
  s7_pointer x;

  x = car(args);
  y = real(cadr(args)); /* chooser below checks is_t_real(y) */

  if (is_t_real(x))
    return(make_boolean(sc, real(x) < y));
  if (is_t_integer(x))
    return(make_boolean(sc, integer(x) < y));
  if (is_t_ratio(x))
    return(make_boolean(sc, fraction(x) < y));
#if WITH_GMP
  if (is_t_big_real(x))
    return(make_boolean(sc, mpfr_cmp_d(big_real(x), y) < 0));
  if (is_t_big_integer(x))
    {
      mpfr_set_d(sc->mpfr_1, y, MPFR_RNDN);
      return(make_boolean(sc, mpfr_cmp_z(sc->mpfr_1, big_integer(x)) > 0));
    }
  if (is_t_big_ratio(x))
    {
      mpfr_set_d(sc->mpfr_1, y, MPFR_RNDN);
      return(make_boolean(sc, mpfr_cmp_q(sc->mpfr_1, big_ratio(x)) > 0));
    }
#endif
  return(method_or_bust(sc, x, sc->lt_symbol, args, T_REAL, 1));
}

static inline s7_pointer lt_p_pp(s7_scheme *sc, s7_pointer p1, s7_pointer p2) {return(make_boolean(sc, lt_b_7pp(sc, p1, p2)));}
static bool lt_b_ii(s7_int i1, s7_int i2) {return(i1 < i2);}
static bool lt_b_dd(s7_double i1, s7_double i2) {return(i1 < i2);}
static s7_pointer lt_p_dd(s7_scheme *sc, s7_double x1, s7_double x2) {return(make_boolean(sc, x1 < x2));}
static s7_pointer lt_p_ii(s7_scheme *sc, s7_int x1, s7_int x2) {return(make_boolean(sc, x1 < x2));}

static bool lt_b_pi(s7_scheme *sc, s7_pointer p1, s7_int p2)
{
  if (is_t_integer(p1)) return(integer(p1) < p2);
  if (is_t_real(p1))  return(real(p1) < p2);
  if (is_t_ratio(p1)) return(ratio_lt_pi(p1, p2));
#if WITH_GMP
  if (is_t_big_integer(p1))
    return(mpz_cmp_si(big_integer(p1), p2) < 0);
  if (is_t_big_real(p1))
    return(mpfr_cmp_si(big_real(p1), p2) < 0);
  if (is_t_big_ratio(p1))
    return(mpq_cmp_si(big_ratio(p1), p2, 1) < 0);
#endif
  simple_wrong_type_argument(sc, sc->lt_symbol, p1, T_REAL);
  return(false);
}

static s7_pointer g_less_2(s7_scheme *sc, s7_pointer args) {return(lt_p_pp(sc, car(args), cadr(args)));}
static s7_pointer lt_p_pi(s7_scheme *sc, s7_pointer p1, s7_int p2) {return(make_boolean(sc, lt_b_pi(sc, p1, p2)));}

static s7_pointer less_chooser(s7_scheme *sc, s7_pointer f, int32_t args, s7_pointer expr, bool ops)
{
  if (args == 2)
    {
      if (ops)
	{
	  s7_pointer arg2;
	  arg2 = caddr(expr);
	  if (is_t_integer(arg2))
	    {
	      if (integer(arg2) == 0)
		return(sc->less_x0);

	      if ((integer(arg2) < S7_INT32_MAX) &&
		  (integer(arg2) > S7_INT32_MIN))
		return(sc->less_xi);
	    }
	  if (is_t_real(arg2))
	    return(sc->less_xf);
	}
      return(sc->less_2);
    }
  return(f);
}

