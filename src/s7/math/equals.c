static bool eq_out_x(s7_scheme *sc, s7_pointer x, s7_pointer y)
{
  if (has_active_methods(sc, x))
    return(find_and_apply_method(sc, x, sc->num_eq_symbol, set_plist_2(sc, x, y)) != sc->F);
  wrong_type_argument_with_type(sc, sc->num_eq_symbol, 1, x, a_number_string);
  return(false);
}

static bool eq_out_y(s7_scheme *sc, s7_pointer x, s7_pointer y)
{
  if (has_active_methods(sc, y))
    return(find_and_apply_method(sc, y, sc->num_eq_symbol, set_plist_2(sc, x, y)) != sc->F);
  wrong_type_argument_with_type(sc, sc->num_eq_symbol, 2, y, a_number_string);
  return(false);
}

static bool num_eq_b_7pp(s7_scheme *sc, s7_pointer x, s7_pointer y)
{
  if (type(x) == type(y))
    {
      if (is_t_integer(x))
	return(integer(x) == integer(y));
      if (is_t_real(x))
	return(real(x) == real(y));
      if (is_t_complex(x))
	return((real_part(x) == real_part(y)) && (imag_part(x) == imag_part(y)));
      if (is_t_ratio(x))
	return((numerator(x) == numerator(y)) && (denominator(x) == denominator(y)));
#if WITH_GMP
      if (is_t_big_integer(x))
	return(mpz_cmp(big_integer(x), big_integer(y)) == 0);
      if (is_t_big_ratio(x))
	return(mpq_equal(big_ratio(x), big_ratio(y)));
      if (is_t_big_real(x))
	return(mpfr_equal_p(big_real(x), big_real(y)));
      if (is_t_big_complex(x)) /* mpc_cmp can't handle NaN */
	{
	  if ((mpfr_nan_p(mpc_realref(big_complex(x)))) || (mpfr_nan_p(mpc_imagref(big_complex(x)))) ||
	      (mpfr_nan_p(mpc_realref(big_complex(y)))) || (mpfr_nan_p(mpc_imagref(big_complex(y)))))
	    return(false);
	  return(mpc_cmp(big_complex(x), big_complex(y)) == 0);
	}
#endif
    }

  switch (type(x))
    {
    case T_INTEGER:
      switch (type(y))
	{
	case T_RATIO:
	  return(false);
	case T_REAL:
#if WITH_GMP
	  if (s7_int_abs(integer(x)) >= INT64_TO_DOUBLE_LIMIT)
	    {
	      if (is_NaN(real(y))) return(false);
	      mpfr_set_d(sc->mpfr_1, real(y), MPFR_RNDN);
	      return(mpfr_cmp_si(sc->mpfr_1, integer(x)) == 0);
	    }
#endif
	  return(integer(x) == real(y));
	case T_COMPLEX:
	  return(false);
#if WITH_GMP
	case T_BIG_INTEGER:
	  return((mpz_fits_slong_p(big_integer(y))) && (integer(x) == mpz_get_si(big_integer(y))));
	case T_BIG_RATIO:
	  return(false);
	case T_BIG_REAL:
	  return((!mpfr_nan_p(big_real(y))) && (mpfr_cmp_si(big_real(y), integer(x)) == 0));
	case T_BIG_COMPLEX:
	  return(false);
#endif
	default: return(eq_out_y(sc, x, y));
	}
      break;

    case T_RATIO:
      switch (type(y))
	{
	case T_INTEGER: return(false);
	case T_REAL:    return(fraction(x) == real(y));
	case T_COMPLEX: return(false);
#if WITH_GMP
	case T_BIG_INTEGER:
	  return(false);
	case T_BIG_RATIO:
	  mpq_set_si(sc->mpq_1, numerator(x), denominator(x));
	  return(mpq_equal(sc->mpq_1, big_ratio(y)));
	case T_BIG_REAL:
	  if (mpfr_nan_p(big_real(y))) return(false);
	  mpq_set_si(sc->mpq_1, numerator(x), denominator(x));
	  return(mpfr_cmp_q(big_real(y), sc->mpq_1) == 0);
	case T_BIG_COMPLEX:
	  return(false);
#endif
	default: return(eq_out_y(sc, x, y));
	}
      break;

    case T_REAL:
      switch (type(y))
	{
	case T_INTEGER:
	  return(real(x) == integer(y));
	case T_RATIO:
	  return(real(x) == fraction(y));
	case T_COMPLEX:
	  return(false);
#if WITH_GMP
	case T_BIG_INTEGER:
	  if (is_NaN(real(x))) return(false);
	  mpfr_set_d(sc->mpfr_1, real(x), MPFR_RNDN);
	  return(mpfr_cmp_z(sc->mpfr_1, big_integer(y)) == 0);
	case T_BIG_RATIO:
	  if (is_NaN(real(x))) return(false);
	  mpfr_set_d(sc->mpfr_1, real(x), MPFR_RNDN);
	  return(mpfr_cmp_q(sc->mpfr_1, big_ratio(y)) == 0);
	case T_BIG_REAL:
	  if (is_NaN(real(x))) return(false);
	  return((!mpfr_nan_p(big_real(y))) && (mpfr_cmp_d(big_real(y), real(x)) == 0));
	case T_BIG_COMPLEX:
	  return(false);
#endif
	default: return(eq_out_y(sc, x, y));
	}
      break;

    case T_COMPLEX:
      if (is_real(y)) return(false);
#if WITH_GMP
      if (is_t_big_complex(y))
	{
	  if ((is_NaN(real_part(x))) || (is_NaN(imag_part(x))) ||
	      (mpfr_nan_p(mpc_realref(big_complex(y)))) || (mpfr_nan_p(mpc_imagref(big_complex(y)))))
	    return(false);
	  mpc_set_d_d(sc->mpc_1, real_part(x), imag_part(x), MPC_RNDNN);
	  return(mpc_cmp(big_complex(y), sc->mpc_1) == 0);
	}
#endif
      return(eq_out_y(sc, x, y));

#if WITH_GMP
    case T_BIG_INTEGER:
      switch (type(y))
	{
	case T_INTEGER:
	  return((mpz_fits_slong_p(big_integer(x))) && (integer(y) == mpz_get_si(big_integer(x))));
	case T_REAL:
	  if (is_NaN(real(y))) return(false);
	  mpfr_set_d(sc->mpfr_1, real(y), MPFR_RNDN);
	  return(mpfr_cmp_z(sc->mpfr_1, big_integer(x)) == 0);
	case T_RATIO: case T_COMPLEX: case T_BIG_RATIO: case T_BIG_COMPLEX:
	  return(false);
	case T_BIG_REAL:
	  return((!mpfr_nan_p(big_real(y))) && (mpfr_cmp_z(big_real(y), big_integer(x)) == 0));
	default: return(eq_out_y(sc, x, y));
	}
    case T_BIG_RATIO:
      switch (type(y))
	{
	case T_RATIO:
	  mpq_set_si(sc->mpq_1, numerator(y), denominator(y));
	  return(mpq_equal(sc->mpq_1, big_ratio(x)));
	case T_REAL:
	  if (is_NaN(real(y))) return(false);
	  mpfr_set_d(sc->mpfr_1, real(y), MPFR_RNDN);
	  return(mpfr_cmp_q(sc->mpfr_1, big_ratio(x)) == 0);
	case T_INTEGER: case T_BIG_INTEGER: case T_COMPLEX: case T_BIG_COMPLEX:
	  return(false);
	case T_BIG_REAL:
	  return((!mpfr_nan_p(big_real(y))) && (mpfr_cmp_q(big_real(y), big_ratio(x)) == 0));
	default: return(eq_out_y(sc, x, y));
	}

    case T_BIG_REAL:
      if ((is_number(y)) && (mpfr_nan_p(big_real(x)))) return(false);
      switch (type(y))
	{
	case T_INTEGER:
	  return(mpfr_cmp_si(big_real(x), integer(y)) == 0);
	case T_RATIO:
	  mpq_set_si(sc->mpq_1, numerator(y), denominator(y));
	  return(mpfr_cmp_q(big_real(x), sc->mpq_1) == 0);
	case T_REAL:
	  return((!is_NaN(real(y))) && (mpfr_cmp_d(big_real(x), real(y)) == 0));
	case T_BIG_INTEGER:
	  return(mpfr_cmp_z(big_real(x), big_integer(y)) == 0);
	case T_BIG_RATIO:
	  return(mpfr_cmp_q(big_real(x), big_ratio(y)) == 0);
	case T_COMPLEX: case T_BIG_COMPLEX:
	  return(false);
	default: return(eq_out_y(sc, x, y));
	}

    case T_BIG_COMPLEX:
      switch (type(y))
	{
	case T_RATIO: case T_REAL: case T_INTEGER: case T_BIG_INTEGER: case T_BIG_RATIO: case T_BIG_REAL:
	  return(false);
	case T_COMPLEX:
	  if ((is_NaN(real_part(y))) || (is_NaN(imag_part(y))) ||
	      (mpfr_nan_p(mpc_realref(big_complex(x)))) || (mpfr_nan_p(mpc_imagref(big_complex(x)))))
	    return(false);
	  mpc_set_d_d(sc->mpc_1, real_part(y), imag_part(y), MPC_RNDNN);
	  return(mpc_cmp(big_complex(x), sc->mpc_1) == 0); /* NaN's not allowed! */
	default: return(eq_out_y(sc, x, y));
	}
#endif
    default: return(eq_out_x(sc, x, y));
    }
  return(false);
}

static bool is_number_via_method(s7_scheme *sc, s7_pointer p)
{
  if (s7_is_number(p))
    return(true);
  if (has_active_methods(sc, p))
    {
      s7_pointer f;
      f = find_method_with_let(sc, p, sc->is_number_symbol);
      if (f != sc->undefined)
	return(is_true(sc, call_method(sc, p, f, set_plist_1(sc, p))));
    }
  return(false);
}

static s7_pointer g_num_eq(s7_scheme *sc, s7_pointer args)
{
  #define H_num_eq "(= z1 ...) returns #t if all its arguments are equal"
  #define Q_num_eq s7_make_circular_signature(sc, 1, 2, sc->is_boolean_symbol, sc->is_number_symbol)

  s7_pointer x, p;
  x = car(args);
  p = cdr(args);
  if (is_null(cdr(p)))
    return(make_boolean(sc, num_eq_b_7pp(sc, x, car(p))));

  for (; is_pair(p); p = cdr(p))
    if (!num_eq_b_7pp(sc, x, car(p)))
      {
	for (p = cdr(p); is_pair(p); p = cdr(p))
	  if (!is_number_via_method(sc, car(p)))
	    return(wrong_type_argument_with_type(sc, sc->num_eq_symbol, position_of(p, args), car(p), a_number_string));
	return(sc->F);
      }
  return(sc->T);
}

static bool num_eq_b_ii(s7_int i1, s7_int i2) {return(i1 == i2);}
static bool num_eq_b_dd(s7_double i1, s7_double i2) {return(i1 == i2);}

static s7_pointer num_eq_p_dd(s7_scheme *sc, s7_double x1, s7_double x2) {return(make_boolean(sc, x1 == x2));}
static s7_pointer num_eq_p_ii(s7_scheme *sc, s7_int x1, s7_int x2)       {return(make_boolean(sc, x1 == x2));}
static s7_pointer num_eq_p_pp(s7_scheme *sc, s7_pointer x, s7_pointer y) {return(make_boolean(sc, num_eq_b_7pp(sc, x, y)));}

static s7_pointer num_eq_p_pi(s7_scheme *sc, s7_pointer p1, s7_int p2)
{
  if (is_t_integer(p1))
    return((integer(p1) == p2) ? sc->T : sc->F);
  if (is_t_real(p1))
    return((real(p1) == p2) ? sc->T : sc->F);
#if WITH_GMP
  if (is_t_big_integer(p1))
    return(((mpz_fits_slong_p(big_integer(p1))) && (p2 == mpz_get_si(big_integer(p1)))) ? sc->T : sc->F);
  if (is_t_big_real(p1))
    return((mpfr_cmp_si(big_real(p1), p2) == 0) ? sc->T : sc->F);
#endif
  return((is_number(p1)) ? sc->F : make_boolean(sc, eq_out_x(sc, p1, make_integer(sc, p2))));
}

static bool num_eq_b_pi(s7_scheme *sc, s7_pointer x, s7_int y)
{
  if (is_t_integer(x))
    return(integer(x) == y);
  if (is_t_real(x))
    return(real(x) == y);
#if WITH_GMP
  if (is_t_big_integer(x))
    return((mpz_fits_slong_p(big_integer(x))) && (y == mpz_get_si(big_integer(x))));
  if (is_t_big_real(x))
    return(mpfr_cmp_si(big_real(x), y) == 0);
#endif
  if (!is_number(x)) /* complex/ratio */
    simple_wrong_type_argument_with_type(sc, sc->num_eq_symbol, x, a_number_string);
  return(false);
}

static s7_pointer g_num_eq_2(s7_scheme *sc, s7_pointer args)
{
  s7_pointer x, y;
  x = car(args);
  y = cadr(args);
  if ((is_t_integer(x)) && (is_t_integer(y))) /* this is by far the most common case (ratios aren't used much, and = with floats is frowned upon) */
    return(make_boolean(sc, integer(x) == integer(y)));
  return(make_boolean(sc, num_eq_b_7pp(sc, x, y)));
}

static inline s7_pointer num_eq_xx(s7_scheme *sc, s7_pointer x, s7_pointer y)
{
  if (is_t_integer(x))
    return(make_boolean(sc, integer(x) == integer(y)));
  if (is_t_real(x))
    return((is_NaN(real(x))) ? sc->F : make_boolean(sc, real(x) == integer(y)));
  if (!is_number(x))
    return(make_boolean(sc, eq_out_x(sc, x, y)));
#if WITH_GMP
  if (is_t_big_integer(x))
    return(make_boolean(sc, mpz_cmp_si(big_integer(x), integer(y)) == 0));
  if (is_t_big_real(x))
    {
      if (mpfr_nan_p(big_real(x))) return(sc->F);
      return(make_boolean(sc, mpfr_cmp_si(big_real(x), integer(y)) == 0));
    }
  if (is_t_big_ratio(x))
    return(make_boolean(sc, mpq_cmp_si(big_ratio(x), integer(y), 1) == 0));
#endif
  return(sc->F);
}

static s7_pointer g_num_eq_xi(s7_scheme *sc, s7_pointer args) {return(num_eq_xx(sc, car(args), cadr(args)));}
static s7_pointer g_num_eq_ix(s7_scheme *sc, s7_pointer args) {return(num_eq_xx(sc, cadr(args), car(args)));}

static s7_pointer num_eq_chooser(s7_scheme *sc, s7_pointer ur_f, int32_t args, s7_pointer expr, bool ops)
{
  if (args == 2)
    {
      if ((ops) && (is_t_integer(caddr(expr))))
	return(sc->num_eq_xi);
      return(((ops) && (is_t_integer(cadr(expr)))) ? sc->num_eq_ix : sc->num_eq_2);
    }
  return(ur_f);
}

