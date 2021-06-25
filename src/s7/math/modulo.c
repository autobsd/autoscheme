static s7_int modulo_i_ii(s7_int x, s7_int y)
{
  s7_int z;
  if (y > 1)
    {
      z = x % y;
      return((z >= 0) ? z : z + y);
    }
  if (y < -1)
    {
      z = x % y;
      return((z > 0) ? z + y : z);
    }
  if (y == 0) return(x);     /* else arithmetic exception */
  return(0);
}

static s7_int modulo_i_ii_unchecked(s7_int i1, s7_int i2) /* here we know i2 > 1 */
{
  /* i2 > 1 */
  s7_int z;
  z = i1 % i2;
  return((z < 0) ? (z + i2) : z);
}

static s7_double modulo_d_7dd(s7_scheme *sc, s7_double x1, s7_double x2)
{
  s7_double c;
  if ((is_NaN(x1)) || (is_NaN(x2)) || (is_inf(x1)) || (is_inf(x2))) return(NAN);
  if (x2 == 0.0) return(x1);
  if (fabs(x1) > 1e17)
    simple_out_of_range(sc, sc->modulo_symbol, wrap_real1(sc, x1), its_too_large_string);
  c = x1 / x2;
  if ((c > 1e19) || (c < -1e19))
    simple_out_of_range(sc, sc->modulo_symbol,
			list_3(sc, sc->divide_symbol, wrap_real1(sc, x1), wrap_real2(sc, x2)),
			wrap_string(sc, "intermediate (a/b) is too large", 31));
  return(x1 - x2 * (s7_int)floor(c));
}

static s7_pointer modulo_p_pp(s7_scheme *sc, s7_pointer x, s7_pointer y)
{
#if WITH_GMP
  /* as tricky as expt, so just use bignums; mpz_mod|_ui = mpz_fdiv_r_ui, but sign ignored -- probably not worth the code
   *   originally   subtract_p_pp(sc, x, multiply_p_pp(sc, y, floor_p_p(sc, divide_p_pp(sc, x, y))))
   *   quotient is                                            truncate_p_p(sc, divide_p_pp(sc, x, y))
   *   remainder is subtract_p_pp(sc, x, multiply_p_pp(sc, y, quotient_p_pp(sc, x, y)))
   */
  if (s7_is_zero(y))
    {
      if (s7_is_real(x))
	return(x);
      return(method_or_bust_pp(sc, x, sc->modulo_symbol, x, y, T_REAL, 1));
    }
  return(big_mod_or_rem(sc, x, y, true));
#else
  s7_double a, b;
  s7_int n1, n2, d1, d2;

  switch (type(x))
    {
    case T_INTEGER:
      switch (type(y))
	{
	case T_INTEGER:
	  return(make_integer(sc, modulo_i_ii(integer(x), integer(y))));

	case T_RATIO:
	  n1 = integer(x);
	  d1 = 1;
	  n2 = numerator(y);
	  d2 = denominator(y);
	  if ((n1 == n2) && (d1 > d2)) return(x); /* signs match so this should be ok */
	  goto RATIO_MOD_RATIO;

	case T_REAL:
	  if ((integer(x) == S7_INT64_MIN) || (s7_int_abs(integer(x)) > QUOTIENT_INT_LIMIT))
	    return(simple_out_of_range(sc, sc->modulo_symbol, x, its_too_large_string));
	  b = real(y);
	  if (b == 0.0) return(x);
	  if (is_NaN(b)) return(y);
	  if (is_inf(b)) return(real_NaN);
	  a = (s7_double)integer(x);
	  goto REAL_MOD;

	default:
	  return(method_or_bust_pp(sc, y, sc->modulo_symbol, x, y, T_REAL, 2));
	}

    case T_RATIO:
      switch (type(y))
	{
	case T_INTEGER:
	  if (integer(y) == 0) return(x);
	  n1 = numerator(x);
	  d1 = denominator(x);
	  n2 = integer(y);

	  if ((n2 > 0) && (n1 > 0) && (n2 > n1)) return(x);
	  if ((n2 < 0) && (n1 < 0) && (n2 < n1)) return(x);
	  if (n2 == S7_INT64_MIN)
	    return(simple_out_of_range(sc, sc->modulo_symbol,
				       list_3(sc, sc->divide_symbol, x, y),
				       wrap_string(sc, "intermediate (a/b) is too large", 31)));
	  /* the problem here is that (modulo 3/2 most-negative-fixnum)
	   * will segfault with signal SIGFPE, Arithmetic exception, so try to trap it.
	   */
	  if ((n1 == n2) && (d1 > 1)) return(x);
	  d2 = 1;
	  goto RATIO_MOD_RATIO;

	case T_RATIO:
	  parcel_out_fractions(x, y);
	  if (d1 == d2)
	    return(s7_make_ratio(sc, modulo_i_ii(n1, n2), d1));
	  if ((n1 == n2) && (d1 > d2)) return(x);

	RATIO_MOD_RATIO:
#if HAVE_OVERFLOW_CHECKS
	  {
	    s7_int n2d1, n1d2, d1d2, fl;
	    if (!multiply_overflow(n2, d1, &n2d1))
	      {
		if ((n2d1 == 1) || (n2d1 == -1)) /* (modulo 100 -1/2) */
		  return(int_zero);

		if (!multiply_overflow(n1, d2, &n1d2))
		  {
		    fl = (s7_int)(n1d2 / n2d1);
		    if (((n1 < 0) && (n2 > 0)) ||
			((n1 > 0) && (n2 < 0)))
		      fl -= 1;
		    if (fl == 0)
		      return(x);

		    if ((!multiply_overflow(d1, d2, &d1d2)) &&
			(!multiply_overflow(fl, n2d1, &fl)) &&
			(!subtract_overflow(n1d2, fl, &fl)))
		      return(s7_make_ratio(sc, fl, d1d2));
		  }}}
#else
	  {
	    s7_int n1d2, n2d1, fl;
	    n1d2 = n1 * d2;
	    n2d1 = n2 * d1;

	    if (n2d1 == 1)
	      return(int_zero);

	    /* can't use "floor" here (float->int ruins everything) */
	    fl = (s7_int)(n1d2 / n2d1);
	    if (((n1 < 0) && (n2 > 0)) ||
		((n1 > 0) && (n2 < 0)))
	      fl -= 1;

	    if (fl == 0)
	      return(x);

	    return(s7_make_ratio(sc, n1d2 - (n2d1 * fl), d1 * d2));
	  }
#endif
	  return(simple_out_of_range(sc, sc->modulo_symbol,
				     list_3(sc, sc->divide_symbol, x, y),
				     wrap_string(sc, "intermediate (a/b) is too large", 31)));

	case T_REAL:
	  b = real(y);
	  if (is_inf(b)) return(real_NaN);
	  if (fabs(b) > 1e17)
	    return(simple_out_of_range(sc, sc->modulo_symbol, y, its_too_large_string));
	  if (b == 0.0) return(x);
	  if (is_NaN(b)) return(y);
	  a = fraction(x);
	  return(make_real(sc, a - b * (s7_int)floor(a / b)));

	default:
	  return(method_or_bust_pp(sc, y, sc->modulo_symbol, x, y, T_REAL, 2));
	}

    case T_REAL:
      {
	s7_double c;
	a = real(x);
	if (!is_real(y))
	  return(method_or_bust_pp(sc, y, sc->modulo_symbol, x, y, T_REAL, 2));
	if (is_NaN(a)) return(x);
	if (is_inf(a)) return(real_NaN); /* not b */
	if (fabs(a) > 1e17)
	  return(simple_out_of_range(sc, sc->modulo_symbol, x, its_too_large_string));

	switch (type(y))
	  {
	  case T_INTEGER:
	    if (integer(y) == 0) return(x);
	    if ((integer(y) == S7_INT64_MIN) || (s7_int_abs(integer(y)) > QUOTIENT_INT_LIMIT))
	      return(simple_out_of_range(sc, sc->modulo_symbol, y, its_too_large_string));
	    b = (s7_double)integer(y);
	    goto REAL_MOD;

	  case T_RATIO:
	    b = fraction(y);
	    goto REAL_MOD;

	  case T_REAL:
	    b = real(y);
	    if (b == 0.0) return(x);
	    if (is_NaN(b)) return(y);
	    if (is_inf(b)) return(real_NaN);
	  REAL_MOD:
	    c = a / b;
	    if (fabs(c) > 1e19)
	      return(simple_out_of_range(sc, sc->modulo_symbol,
					 list_3(sc, sc->divide_symbol, x, y),
					 wrap_string(sc, "intermediate (a/b) is too large", 31)));
	    return(make_real(sc, a - b * (s7_int)floor(c)));

	  default:
	    return(method_or_bust_pp(sc, y, sc->modulo_symbol, x, y, T_REAL, 2));
	  }}

    default:
      return(method_or_bust_pp(sc, x, sc->modulo_symbol, x, y, T_REAL, 1));
    }
#endif
}

static s7_pointer g_modulo(s7_scheme *sc, s7_pointer args)
{
  #define H_modulo "(modulo x1 x2) returns x1 mod x2; (modulo 4 3) = 1.  The arguments can be real numbers."
  #define Q_modulo sc->pcl_r
  /* (define (mod x1 x2) (- x1 (* x2 (floor (/ x1 x2))))) from slib
   * (mod x 0) = x according to "Concrete Mathematics"
   */
  return(modulo_p_pp(sc, car(args), cadr(args)));
}
