#if WITH_GMP
static s7_pointer big_lcm(s7_scheme *sc, s7_int num, s7_int den, s7_pointer args)
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
	  mpz_lcm(sc->mpz_3, sc->mpz_3, sc->mpz_1);
	  mpz_set_si(sc->mpz_4, 1);
	  break;

	case T_RATIO:
	  mpz_set_si(sc->mpz_1, numerator(rat));
	  mpz_set_si(sc->mpz_2, denominator(rat));
	  mpz_lcm(sc->mpz_3, sc->mpz_3, sc->mpz_1);
	  mpz_gcd(sc->mpz_4, sc->mpz_4, sc->mpz_2);
	  break;

	case T_BIG_INTEGER:
	  mpz_lcm(sc->mpz_3, sc->mpz_3, big_integer(rat));
	  mpz_set_si(sc->mpz_4, 1);
	  break;

	case T_BIG_RATIO:
	  mpz_lcm(sc->mpz_3, sc->mpz_3, mpq_numref(big_ratio(rat)));
	  mpz_gcd(sc->mpz_4, sc->mpz_4, mpq_denref(big_ratio(rat)));
	  break;

	case T_REAL: case T_BIG_REAL: case T_COMPLEX: case T_BIG_COMPLEX:
	  return(wrong_type_argument_with_type(sc, sc->lcm_symbol, position_of(x, args), rat, a_rational_string));

	default:
	  return(method_or_bust_with_type(sc, rat, sc->lcm_symbol,
					  set_ulist_1(sc, mpz_to_rational(sc, sc->mpz_3, sc->mpz_4), x),
					  a_rational_string, position_of(x, args)));
	}}
  return(mpz_to_rational(sc, sc->mpz_3, sc->mpz_4));
}
#endif

static s7_pointer g_lcm(s7_scheme *sc, s7_pointer args)
{
  /* (/ (* m n) (gcd m n)), (lcm a b c) -> (lcm a (lcm b c)) */
  #define H_lcm "(lcm ...) returns the least common multiple of its rational arguments"
  #define Q_lcm sc->pcl_f

  s7_int n = 1, d = 0;
  s7_pointer p;

  if (!is_pair(args))
    return(int_one);

  if (!is_pair(cdr(args)))
    {
      if (!is_rational(car(args)))
	return(method_or_bust_with_type(sc, car(args), sc->lcm_symbol, args, a_rational_string, 1));
      return(g_abs(sc, args));
    }

  for (p = args; is_pair(p); p = cdr(p))
    {
      s7_pointer x;
      s7_int b;
#if HAVE_OVERFLOW_CHECKS
      s7_int n1;
#endif
      x = car(p);
      switch (type(x))
	{
	case T_INTEGER:
	  d = 1;
	  if (integer(x) == 0) /* return 0 unless there's a wrong-type-arg (geez what a mess) */
	    {
	      for (p = cdr(p); is_pair(p); p = cdr(p))
		{
		  s7_pointer x1;
		  x1 = car(p);
		  if (is_number(x1))
		    {
		      if (!is_rational(x1))
			return(wrong_type_argument_with_type(sc, sc->lcm_symbol, position_of(p, args), x1, a_rational_string));
		    }
		  else
		    if (has_active_methods(sc, x1))
		      {
			s7_pointer f;
			f = find_method_with_let(sc, x1, sc->is_rational_symbol);
			if ((f == sc->undefined) ||
			    (is_false(sc, call_method(sc, x1, f, set_plist_1(sc, x1)))))
			  return(wrong_type_argument_with_type(sc, sc->lcm_symbol, position_of(p, args), x1, a_rational_string));
		      }
		    else return(wrong_type_argument_with_type(sc, sc->lcm_symbol, position_of(p, args), x1, a_rational_string));
		}
	      return(int_zero);
	    }
	  b = integer(x);
	  if (b < 0)
	    {
	      if (b == S7_INT64_MIN)
#if WITH_GMP
		return(big_lcm(sc, n, d, p));
#else
		return(simple_out_of_range(sc, sc->lcm_symbol, args, its_too_large_string));
#endif
	      b = -b;
	    }
#if HAVE_OVERFLOW_CHECKS
	  if (multiply_overflow(n / c_gcd(n, b), b, &n1))
#if WITH_GMP
	    return(big_lcm(sc, n, d, p));
#else
	    return(simple_out_of_range(sc, sc->lcm_symbol, args, result_is_too_large_string));
#endif
	  n = n1;
#else
	  n = (n / c_gcd(n, b)) * b;
#endif
	  break;

	case T_RATIO:
	  b = numerator(x);
	  if (b < 0)
	    {
	      if (b == S7_INT64_MIN)
#if WITH_GMP
		return(big_lcm(sc, n, d, p));
#else
		return(simple_out_of_range(sc, sc->lcm_symbol, args, its_too_large_string));
#endif
	      b = -b;
	    }
#if HAVE_OVERFLOW_CHECKS
	  if (multiply_overflow(n / c_gcd(n, b), b, &n1))  /* (lcm 92233720368547758/3 3005/2) */
#if WITH_GMP
	    return(big_lcm(sc, n, d, p));
#else
	    return(simple_out_of_range(sc, sc->lcm_symbol, args, wrap_string(sc, "intermediate result is too large", 32)));
#endif
          n = n1;
#else
	  n = (n / c_gcd(n, b)) * b;
#endif
	  if (d == 0)
	    d = (p == args) ? denominator(x) : 1;
	  else d = c_gcd(d, denominator(x));
	  break;

#if WITH_GMP
	case T_BIG_INTEGER:
	  d = 1;
	case T_BIG_RATIO:
	  return(big_lcm(sc, n, d, p));
#endif

	case T_REAL: case T_BIG_REAL: case T_COMPLEX: case T_BIG_COMPLEX:
	  return(wrong_type_argument_with_type(sc, sc->lcm_symbol, position_of(p, args), x, a_rational_string));

	default:
	  return(method_or_bust_with_type(sc, x, sc->lcm_symbol,
					  set_ulist_1(sc, (d <= 1) ? make_integer(sc, n) : s7_make_ratio(sc, n, d), p),
					  a_rational_string, position_of(p, args)));
	}}
  return((d <= 1) ? make_integer(sc, n) : make_simple_ratio(sc, n, d));
}

