static s7_int int_to_int(s7_int x, s7_int n)
{
  /* from GSL */
  s7_int value = 1;
  do {
    if (n & 1) value *= x;
    n >>= 1;
#if HAVE_OVERFLOW_CHECKS
    if (multiply_overflow(x, x, &x))
      break;
#else
    x *= x;
#endif
  } while (n);
  return(value);
}

static const int64_t nth_roots[63] = {
  S7_INT64_MAX, S7_INT64_MAX, 3037000499LL, 2097151, 55108, 6208, 1448, 511, 234, 127, 78, 52, 38, 28, 22,
  18, 15, 13, 11, 9, 8, 7, 7, 6, 6, 5, 5, 5, 4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2,
  2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2};

static bool int_pow_ok(s7_int x, s7_int y)
{
  return((y < S7_INT_BITS) && (nth_roots[y] >= s7_int_abs(x)));
}

#if WITH_GMP
static s7_pointer real_part_p_p(s7_scheme *sc, s7_pointer p);
static bool lt_b_pi(s7_scheme *sc, s7_pointer p1, s7_int p2);

static s7_pointer big_expt(s7_scheme *sc, s7_pointer args)
{
  s7_pointer x, y, res;
  x = car(args);
  if (!s7_is_number(x))
    return(method_or_bust_with_type(sc, x, sc->expt_symbol, args, a_number_string, 1));

  y = cadr(args);
  if (!s7_is_number(y))
    return(method_or_bust_with_type(sc, y, sc->expt_symbol, args, a_number_string, 2));

  if (s7_is_zero(x))
    {
      if ((s7_is_integer(x)) &&
	  (s7_is_integer(y)) &&
	  (s7_is_zero(y)))
	return(int_one);

      if (s7_is_real(y))
	{
	  if (s7_is_negative(y))
	    return(division_by_zero_error(sc, sc->expt_symbol, args));
	}
      else
	if (s7_is_negative(real_part_p_p(sc, y))) /* handle big_complex as well as complex */
	  return(division_by_zero_error(sc, sc->expt_symbol, args));

      if ((s7_is_rational(x)) &&
	  (s7_is_rational(y)))
	return(int_zero);
      return(real_zero);
    }

  if (s7_is_integer(y))
    {
      s7_int yval;
      yval = s7_integer_checked(sc, y);
      if (yval == 0)
	return((s7_is_rational(x)) ? int_one : real_one);

      if (yval == 1)
	return(x);

      if ((!is_big_number(x)) &&
	  ((s7_is_one(x)) || (s7_is_zero(x))))
	return(x);

      if ((yval < S7_INT32_MAX) &&
	  (yval > S7_INT32_MIN))
	{
	  /* (protect against gmp exception if for example (expt 1/9223372036854775807 -9223372036854775807) */
	  if (s7_is_integer(x))
	    {
	      if (is_t_big_integer(x))
		mpz_set(sc->mpz_2, big_integer(x));
	      else mpz_set_si(sc->mpz_2, integer(x));
	      if (yval >= 0)
		{
		  mpz_pow_ui(sc->mpz_2, sc->mpz_2, (uint32_t)yval);
		  return(mpz_to_integer(sc, sc->mpz_2));
		}
	      mpz_pow_ui(sc->mpz_2, sc->mpz_2, (uint32_t)(-yval));
	      mpq_set_z(sc->mpq_1, sc->mpz_2);
	      mpq_inv(sc->mpq_1, sc->mpq_1);
	      if (mpz_cmp_ui(mpq_denref(sc->mpq_1), 1) == 0)
		return(mpz_to_integer(sc, mpq_numref(sc->mpq_1)));
	      return(mpq_to_big_ratio(sc, sc->mpq_1));
	    }

	  if (s7_is_ratio(x)) /* here y is an integer */
	    {
	      if (is_t_big_ratio(x))
		{
		  mpz_set(sc->mpz_1, mpq_numref(big_ratio(x)));
		  mpz_set(sc->mpz_2, mpq_denref(big_ratio(x)));
		}
	      else
		{
		  mpz_set_si(sc->mpz_1, numerator(x));
		  mpz_set_si(sc->mpz_2, denominator(x));
		}
	      if (yval >= 0)
		{
		  mpz_pow_ui(sc->mpz_1, sc->mpz_1, (uint32_t)yval);
		  mpz_pow_ui(sc->mpz_2, sc->mpz_2, (uint32_t)yval);
		  mpq_set_num(sc->mpq_1, sc->mpz_1);
		  mpq_set_den(sc->mpq_1, sc->mpz_2);
		}
	      else
		{
		  yval = -yval;
		  mpz_pow_ui(sc->mpz_1, sc->mpz_1, (uint32_t)yval);
		  mpz_pow_ui(sc->mpz_2, sc->mpz_2, (uint32_t)yval);
		  mpq_set_num(sc->mpq_1, sc->mpz_2);
		  mpq_set_den(sc->mpq_1, sc->mpz_1);
		  mpq_canonicalize(sc->mpq_1);
		}
	      if (mpz_cmp_ui(mpq_denref(sc->mpq_1), 1) == 0)
		return(mpz_to_integer(sc, mpq_numref(sc->mpq_1)));
	      return(mpq_to_big_ratio(sc, sc->mpq_1));
	    }

	  if (s7_is_real(x))
	    {
	      if (is_t_big_real(x))
		mpfr_set(sc->mpfr_1, big_real(x), MPFR_RNDN);
	      else mpfr_set_d(sc->mpfr_1, real(x), MPFR_RNDN);
	      mpfr_pow_si(sc->mpfr_1, sc->mpfr_1, yval, MPFR_RNDN);
	      return(mpfr_to_big_real(sc, sc->mpfr_1));
	    }}}

  if ((is_t_ratio(y)) &&              /* not s7_is_ratio which accepts bignums */
      (numerator(y) == 1))
    {
      if (denominator(y) == 2)
	return(sqrt_p_p(sc, x));

      if ((s7_is_real(x)) &&
	  (denominator(y) == 3))
	{
	  any_real_to_mpfr(sc, x, sc->mpfr_1);
	  mpfr_cbrt(sc->mpfr_1, sc->mpfr_1, MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	}}

  res = any_number_to_mpc(sc, y, sc->mpc_2);
  if (res == real_infinity)
    {
      if (s7_is_one(x)) return(int_one);
      if (s7_is_real(x))
	{
	  if (s7_is_zero(x))
	    {
	      if (s7_is_negative(y)) return(division_by_zero_error(sc, sc->expt_symbol, args));
	      return(real_zero);
	    }
	  if (lt_b_pi(sc, x, 0))
	    {
	      if (lt_b_pi(sc, x, -1))
		return((s7_is_positive(y)) ? real_infinity : real_zero);
	      return((s7_is_positive(y)) ? real_zero : real_infinity);
	    }
	  if (lt_b_pi(sc, x, 1))
	    return((s7_is_positive(y)) ? real_zero : real_infinity);
	  return((s7_is_positive(y)) ? real_infinity : real_zero);
	}
      return((s7_is_negative(y)) ? real_zero : complex_NaN);
    }
  if (res) return(complex_NaN);

  if ((s7_is_real(x)) &&
      (s7_is_real(y)) &&
      (s7_is_positive(x)))
    {
      res = any_real_to_mpfr(sc, x, sc->mpfr_1);
      if (res)
	{
	  if (res == real_infinity)
	    {
	      if (s7_is_negative(y)) return(real_zero);
	      return((s7_is_zero(y)) ? real_one : real_infinity);
	    }
	  return(complex_NaN);
	}
      mpfr_pow(sc->mpfr_1, sc->mpfr_1, mpc_realref(sc->mpc_2), MPFR_RNDN);
      return(mpfr_to_big_real(sc, sc->mpfr_1));
    }

  res = any_number_to_mpc(sc, x, sc->mpc_1);
  if (res)
    {
      if ((res == real_infinity) && (s7_is_real(y)))
	{
	  if (s7_is_negative(y)) return(real_zero);
	  return((s7_is_zero(y)) ? real_one : real_infinity);
	}
      return(complex_NaN);
    }
  if (mpc_cmp_si_si(sc->mpc_1, 0, 0) == 0)
    return(int_zero);
  if (mpc_cmp_si_si(sc->mpc_1, 1, 0) == 0)
    return(int_one);

  mpc_pow(sc->mpc_1, sc->mpc_1, sc->mpc_2, MPC_RNDNN);

  if ((!mpfr_nan_p(mpc_imagref(sc->mpc_1))) && (mpfr_cmp_ui(mpc_imagref(sc->mpc_1), 0) == 0)) /* (expt -inf.0 1/3) -> +inf.0+nan.0i in mpc */
    {
      if ((s7_is_rational(car(args))) &&
	  (s7_is_rational(cadr(args))) &&
	  (mpfr_integer_p(mpc_realref(sc->mpc_1)) != 0))
	{
	  /* mpfr_integer_p can be confused: (expt 2718/1000 (bignum 617/5)) returns an int32_t if precision=128, float if 512 */
	  /*   so first make sure we're within (say) 31 bits */
	  mpfr_set_ui(sc->mpfr_1, S7_INT32_MAX, MPFR_RNDN);
	  if (mpfr_cmpabs(mpc_realref(sc->mpc_1), sc->mpfr_1) < 0)
	    {
	      mpfr_get_z(sc->mpz_1, mpc_realref(sc->mpc_1), MPFR_RNDN);
	      return(mpz_to_integer(sc, sc->mpz_1));
	    }}
      mpfr_set(sc->mpfr_1, mpc_realref(sc->mpc_1), MPFR_RNDN);
      return(mpfr_to_big_real(sc, sc->mpfr_1));
    }
  return(mpc_to_number(sc, sc->mpc_1));
}
#endif

static s7_pointer g_expt(s7_scheme *sc, s7_pointer args)
{
  #define H_expt "(expt z1 z2) returns z1^z2"
  #define Q_expt sc->pcl_n

  s7_pointer n, pw;

#if WITH_GMP
  return(big_expt(sc, args));
  /* big_expt sometimes chooses a different value: g_expt (expt -1 1/3) is -1, but big_expt (expt -1 (bignum 1/3)) is (complex 1/2 (/ (sqrt 3) 2)) */
#endif

  n = car(args);
  if (!s7_is_number(n))
    return(method_or_bust_with_type(sc, n, sc->expt_symbol, args, a_number_string, 1));

  pw = cadr(args);
  if (!s7_is_number(pw))
    return(method_or_bust_with_type(sc, pw, sc->expt_symbol, args, a_number_string, 2));

  /* this provides more than 2 args to expt:
   *  if (is_not_null(cddr(args))) return(g_expt(sc, list_2(sc, car(args), g_expt(sc, cdr(args)))));
   * but it's unusual in scheme to process args in reverse order, and the syntax by itself is ambiguous (does (expt 2 2 3) = 256 or 64?)
   */

  if (s7_is_zero(n))
    {
      if (s7_is_zero(pw))
	{
	  if ((s7_is_integer(n)) && (s7_is_integer(pw)))       /* (expt 0 0) -> 1 */
	    return(int_one);
	  return(real_zero);                                   /* (expt 0.0 0) -> 0.0 */
	}
      if (s7_is_real(pw))
	{
	  if (s7_is_negative(pw))                              /* (expt 0 -1) */
	    return(division_by_zero_error(sc, sc->expt_symbol, args));
	  /* (Clisp gives divide-by-zero error here, Guile returns inf.0) */

	  if (is_NaN(s7_real(pw)))                             /* (expt 0 +nan.0) */
	    return(pw);
	}
      else
	{                                                      /* (expt 0 a+bi) */
	  if (real_part(pw) < 0.0)                             /* (expt 0 -1+i) */
	    return(division_by_zero_error(sc, sc->expt_symbol, args));
	  if ((is_NaN(real_part(pw))) ||                       /* (expt 0 0+1/0i) */
	      (is_NaN(imag_part(pw))))
	    return(real_NaN);
	}
      if ((s7_is_integer(n)) && (s7_is_integer(pw)))           /* pw != 0, (expt 0 2312) */
	return(int_zero);
      return(real_zero);                                       /* (expt 0.0 123123) */
    }

  if (s7_is_one(pw))
    {
      if (s7_is_integer(pw))                                   /* (expt x 1) */
	return(n);
      if (is_rational(n))                                      /* (expt ratio 1.0) */
	return(make_real(sc, rational_to_double(sc, n)));
      return(n);
    }
  if (is_t_integer(pw))
    {
      s7_int y;
      y = integer(pw);
      if (y == 0)
	{
	  if (is_rational(n))                                 /* (expt 3 0) */
	    return(int_one);
	  if ((is_NaN(s7_real_part(n))) ||                    /* (expt 1/0 0) -> NaN */
	      (is_NaN(s7_imag_part(n))))                      /* (expt (complex 0 1/0) 0) -> NaN */
	    return(n);
	  return(real_one);                                   /* (expt 3.0 0) */
	}
      switch (type(n))
	{
	case T_INTEGER:
	  {
	    s7_int x;
	    x = s7_integer_checked(sc, n);
	    if (x == 1)                                       /* (expt 1 y) */
	      return(n);

	    if (x == -1)
	      {
		if (y == S7_INT64_MIN)                          /* (expt -1 most-negative-fixnum) */
		  return(int_one);
		if (s7_int_abs(y) & 1)                          /* (expt -1 odd-int) */
		  return(n);
		return(int_one);                              /* (expt -1 even-int) */
	      }

	    if (y == S7_INT64_MIN)                              /* (expt x most-negative-fixnum) */
	      return(int_zero);
	    if (x == S7_INT64_MIN)                              /* (expt most-negative-fixnum y) */
	      return(make_real(sc, pow((double)x, (double)y)));

	    if (int_pow_ok(x, s7_int_abs(y)))
	      {
		if (y > 0)
		  return(make_integer(sc, int_to_int(x, y)));
		return(s7_make_ratio(sc, 1, int_to_int(x, -y)));
	      }}
	  break;

	case T_RATIO:
	  {
	    s7_int nm, dn;

	    nm = numerator(n);
	    dn = denominator(n);

	    if (y == S7_INT64_MIN)
	      {
		if (s7_int_abs(nm) > dn)
		  return(int_zero);                /* (expt 4/3 most-negative-fixnum) -> 0? */
		return(real_infinity);               /* (expt 3/4 most-negative-fixnum) -> inf? */
	      }

	    if ((int_pow_ok(nm, s7_int_abs(y))) &&
		(int_pow_ok(dn, s7_int_abs(y))))
	      {
		if (y > 0)
		  return(s7_make_ratio(sc, int_to_int(nm, y), int_to_int(dn, y)));
		return(s7_make_ratio(sc, int_to_int(dn, -y), int_to_int(nm, -y)));
	      }}
	  break;
	  /* occasionally int^rat can be int32_t but it happens so infrequently it's probably not worth checking
	   *  one possibly easy case: (expt 1 1/2) -> 1 (-1?) etc
	   */

	case T_REAL:
	  /* (expt -1.0 most-positive-fixnum) should be -1.0
	   * (expt -1.0 (+ (expt 2 53) 1)) -> -1.0
	   * (expt -1.0 (- 1 (expt 2 54))) -> -1.0
	   */
	  if (real(n) == -1.0)
	    {
	      if (y == S7_INT64_MIN)
		return(real_one);
	      return((s7_int_abs(y) & 1) ? n : real_one);
	    }
	  break;

	case T_COMPLEX:
#if HAVE_COMPLEX_NUMBERS
	  if ((s7_real_part(n) == 0.0) &&
	      ((s7_imag_part(n) == 1.0) ||
	       (s7_imag_part(n) == -1.0)))
	    {
	      bool yp, np;
	      yp = (y > 0);
	      np = (s7_imag_part(n) > 0.0);
	      switch (s7_int_abs(y) % 4)
		{
		case 0: return(real_one);
		case 1: return(s7_make_complex(sc, 0.0, (yp == np) ? 1.0 : -1.0));
		case 2: return(make_real(sc, -1.0));
		case 3: return(s7_make_complex(sc, 0.0, (yp == np) ? -1.0 : 1.0));
		}}
#else
	  return(out_of_range(sc, sc->expt_symbol, int_two, n, no_complex_numbers_string));
#endif
	  break;
	}}

  if ((s7_is_real(n)) &&
      (s7_is_real(pw)))
    {
      s7_double x, y;

      if ((is_t_ratio(pw)) &&
	  (numerator(pw) == 1))
	{
	  if (denominator(pw) == 2)
	    return(sqrt_p_p(sc, n));
	  if (denominator(pw) == 3)
	    return(make_real(sc, cbrt(s7_real(n)))); /* (expt 27 1/3) should be 3, not 3.0... */
	  /* but: (expt 512/729 1/3) -> 0.88888888888889, and 4 -> sqrt(sqrt...) etc? */
	}

      x = s7_real(n);
      y = s7_real(pw);

      if (is_NaN(x)) return(n);
      if (is_NaN(y)) return(pw);
      if (y == 0.0) return(real_one);

      /* I think pow(rl, inf) is ok */
      if (x > 0.0)
	return(make_real(sc, pow(x, y)));      /* tricky cases abound here: (expt -1 1/9223372036854775807) */
    }

  /* (expt 0+i 1e+16) = 0.98156860153485-0.19111012657867i ?
   * (expt 0+i 1+1/0i) = 0.0 ??
   */
  return(c_complex_to_s7(sc, cpow(s7_to_c_complex(n), s7_to_c_complex(pw))));
}

