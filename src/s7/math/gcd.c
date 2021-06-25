#if WITH_GMP
static s7_pointer big_gcd(s7_scheme *sc, s7_int num, s7_int den, s7_pointer args)
{
  s7_pointer x;

  mpz_set_si(sc->mpz_3, num);
  mpz_set_si(sc->mpz_4, den);

  for (x = args; is_pair(x); x = cdr(x))
    {
      s7_pointer rat;
      rat = car(x);
      switch (type(rat))
	{
	case T_INTEGER:
	  mpz_set_si(sc->mpz_1, integer(rat));
	  mpz_gcd(sc->mpz_3, sc->mpz_3, sc->mpz_1);
	  break;

	case T_RATIO:
	  mpz_set_si(sc->mpz_1, numerator(rat));
	  mpz_set_si(sc->mpz_2, denominator(rat));
	  mpz_gcd(sc->mpz_3, sc->mpz_3, sc->mpz_1);
	  mpz_lcm(sc->mpz_4, sc->mpz_4, sc->mpz_2);
	  break;

	case T_BIG_INTEGER:
	  mpz_gcd(sc->mpz_3, sc->mpz_3, big_integer(rat));
	  break;

	case T_BIG_RATIO:
	  mpz_gcd(sc->mpz_3, sc->mpz_3, mpq_numref(big_ratio(rat)));
	  mpz_lcm(sc->mpz_4, sc->mpz_4, mpq_denref(big_ratio(rat)));
	  break;

	case T_REAL: case T_BIG_REAL: case T_COMPLEX: case T_BIG_COMPLEX:
	  return(wrong_type_argument_with_type(sc, sc->gcd_symbol, position_of(x, args), rat, a_rational_string));

	default:
	  return(method_or_bust_with_type(sc, rat, sc->gcd_symbol,
					  set_ulist_1(sc, mpz_to_rational(sc, sc->mpz_3, sc->mpz_4), x),
					  a_rational_string, position_of(x, args)));
	}}
  return(mpz_to_rational(sc, sc->mpz_3, sc->mpz_4));
}
#endif

static s7_pointer g_gcd(s7_scheme *sc, s7_pointer args)
{
  #define H_gcd "(gcd ...) returns the greatest common divisor of its rational arguments"
  #define Q_gcd sc->pcl_f

  s7_int n = 0, d = 1;
  s7_pointer p;

  if (!is_pair(args))       /* (gcd) */
    return(int_zero);

  if (!is_pair(cdr(args)))  /* (gcd 3/4) */
    {
      if (!is_rational(car(args)))
	return(method_or_bust_with_type(sc, car(args), sc->gcd_symbol, args, a_rational_string, 1));
      return(abs_p_p(sc, car(args)));
    }

  for (p = args; is_pair(p); p = cdr(p))
    {
      s7_pointer x;
      x = car(p);
      switch (type(x))
	{
	case T_INTEGER:
	  if (integer(x) == S7_INT64_MIN)
#if WITH_GMP
	    return(big_gcd(sc, n, d, p));
#else
	    return(simple_out_of_range(sc, sc->lcm_symbol, args, its_too_large_string));
#endif
	  n = c_gcd(n, integer(x));
	  break;

	case T_RATIO:
	  {
#if HAVE_OVERFLOW_CHECKS
	    s7_int dn;
#endif
	    n = c_gcd(n, numerator(x));
	    if (d == 1)
	      d = denominator(x);
	    else
	      {
		s7_int b;
		b = denominator(x);
#if HAVE_OVERFLOW_CHECKS
		if (multiply_overflow(d / c_gcd(d, b), b, &dn)) /* (gcd 1/92233720368547758 1/3005) */
#if WITH_GMP
		  return(big_gcd(sc, n, d, x));
#else
		  return(simple_out_of_range(sc, sc->gcd_symbol, args, wrap_string(sc, "intermediate result is too large", 32)));
#endif
		d = dn;
#else
		d = (d / c_gcd(d, b)) * b;
#endif
	      }}
	  break;

#if WITH_GMP
	case T_BIG_INTEGER:
	case T_BIG_RATIO:
	  return(big_gcd(sc, n, d, p));
#endif

	case T_REAL: case T_BIG_REAL: case T_COMPLEX: case T_BIG_COMPLEX:
	  return(wrong_type_argument_with_type(sc, sc->gcd_symbol, position_of(p, args), x, a_rational_string));

	default:
	  return(method_or_bust_with_type(sc, x, sc->gcd_symbol,
					  set_ulist_1(sc, (d <= 1) ? make_integer(sc, n) : s7_make_ratio(sc, n, d), p),
					  a_rational_string, position_of(p, args)));
	}}
  return((d <= 1) ? make_integer(sc, n) : make_simple_ratio(sc, n, d));
}

