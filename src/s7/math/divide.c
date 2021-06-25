static s7_pointer complex_invert(s7_scheme *sc, s7_pointer p)
{
  s7_double r2, i2, den;
  r2 = real_part(p);
  i2 = imag_part(p);
  den = (r2 * r2 + i2 * i2);
  /* here if p is, for example, -inf.0+i, den is +inf.0 so -i2/den is -0.0 (in gcc anyway), so the imag part is 0.0 */
  return(s7_make_complex(sc, r2 / den, -i2 / den));
}

static s7_pointer invert_p_p(s7_scheme *sc, s7_pointer p)
{
#if WITH_GMP
  s7_pointer x;
#endif
  switch (type(p))
    {
    case T_INTEGER:
#if WITH_GMP && (!POINTER_32)
      if (integer(p) == S7_INT64_MIN) /* (/ 1 (*s7* 'most-negative-fixnum)) -> -1/9223372036854775808 */
	{
	  new_cell(sc, x, T_BIG_RATIO);
	  big_ratio_bgr(x) = alloc_bigrat(sc);
	  add_big_ratio(sc, x);
	  mpz_set_si(sc->mpz_1, S7_INT64_MAX);
	  mpz_set_si(sc->mpz_2, 1);
	  mpz_add(sc->mpz_1, sc->mpz_1, sc->mpz_2);
	  mpq_set_si(big_ratio(x), -1, 1);
	  mpq_set_den(big_ratio(x), sc->mpz_1); /* geez... */
	  return(x);
	}
#endif
      if (integer(p) == 0)
	return(division_by_zero_error(sc, sc->divide_symbol, p));
      return(make_simple_ratio(sc, 1, integer(p)));  /* this checks for int */
    case T_RATIO:
      return(make_simple_ratio(sc, denominator(p), numerator(p)));
    case T_REAL:
      if (real(p) == 0.0)
	return(division_by_zero_error(sc, sc->divide_symbol, p));
      return(make_real(sc, 1.0 / real(p)));
    case T_COMPLEX:
      return(complex_invert(sc, p));

#if WITH_GMP
    case T_BIG_INTEGER:
      if (mpz_cmp_ui(big_integer(p), 0) == 0)
	return(division_by_zero_error(sc, sc->divide_symbol, p));
      if ((mpz_cmp_ui(big_integer(p), 1) == 0) || (mpz_cmp_si(big_integer(p), -1) == 0))
	return(p);
      new_cell(sc, x, T_BIG_RATIO);
      big_ratio_bgr(x) = alloc_bigrat(sc);
      add_big_ratio(sc, x);
      mpq_set_si(big_ratio(x), 1, 1);
      mpq_set_den(big_ratio(x), big_integer(p));
      mpq_canonicalize(big_ratio(x));
      return(x);

    case T_BIG_RATIO:
      if (mpz_cmp_ui(mpq_numref(big_ratio(p)), 1) == 0)
	return(mpz_to_integer(sc, mpq_denref(big_ratio(p))));
      if (mpz_cmp_si(mpq_numref(big_ratio(p)), -1) == 0)
	{
	  mpz_neg(sc->mpz_1, mpq_denref(big_ratio(p)));
	  return(mpz_to_integer(sc, sc->mpz_1));
	}
      new_cell(sc, x, T_BIG_RATIO);
      big_ratio_bgr(x) = alloc_bigrat(sc);
      add_big_ratio(sc, x);
      mpq_inv(big_ratio(x), big_ratio(p));
      mpq_canonicalize(big_ratio(x));
      return(x);

    case T_BIG_REAL:
      if (mpfr_zero_p(big_real(p)))
	return(division_by_zero_error(sc, sc->divide_symbol, p));
      x = mpfr_to_big_real(sc, big_real(p));
      mpfr_ui_div(big_real(x), 1, big_real(x), MPFR_RNDN);
      return(x);

    case T_BIG_COMPLEX:
      if ((!mpfr_number_p(mpc_realref(big_complex(p)))) || (!mpfr_number_p(mpc_imagref(big_complex(p)))))
	return(complex_NaN);
      mpc_ui_div(sc->mpc_1, 1, big_complex(p), MPC_RNDNN);
      return(mpc_to_number(sc, sc->mpc_1)); /* x might be 0+0i if real-part is inf? */
#endif
    default:
      check_method(sc, p, sc->divide_symbol, set_plist_1(sc, p));
      return(wrong_type_argument_with_type(sc, sc->divide_symbol, 1, p, a_number_string));
    }
}

static s7_pointer divide_p_pp(s7_scheme *sc, s7_pointer x, s7_pointer y)
{
  /* splitting out real/real here saves very little */
  switch (type(x))
    {
    case T_INTEGER:
      switch (type(y))
	{
	  /* -------- integer x -------- */
	case T_INTEGER:
	  if (integer(y) == 0)
	    return(division_by_zero_error(sc, sc->divide_symbol, set_elist_2(sc, x, y)));
	  if (integer(x) == 1)  /* mainly to handle (/ 1 -9223372036854775808) correctly! */
	    return(invert_p_p(sc, y));
	  return(s7_make_ratio(sc, integer(x), integer(y)));

	case T_RATIO:
#if HAVE_OVERFLOW_CHECKS
	  {
	    s7_int dn;
	    if (multiply_overflow(integer(x), denominator(y), &dn))
#if WITH_GMP
	      {
		mpq_set_si(sc->mpq_1, integer(x), 1);
		mpq_set_si(sc->mpq_2, numerator(y), denominator(y));
		mpq_div(sc->mpq_1, sc->mpq_1, sc->mpq_2);
		return(mpq_to_canonicalized_rational(sc, sc->mpq_1));
	      }
#else
	      return(make_real(sc, integer(x) * inverted_fraction(y)));
#endif
	    return(s7_make_ratio(sc, dn, numerator(y)));
	  }
#else
	  return(s7_make_ratio(sc, integer(x) * denominator(y), numerator(y)));
#endif

	case T_REAL:
	  if (is_NaN(real(y))) return(real_NaN);
	  if (is_inf(real(y))) return(real_zero);
	  if (real(y) == 0.0)
	    return(division_by_zero_error(sc, sc->divide_symbol, set_elist_2(sc, x, y)));
#if WITH_GMP
	  if ((s7_int_abs(integer(x))) > QUOTIENT_INT_LIMIT)
	    {
	      mpfr_set_si(sc->mpfr_1, integer(x), MPFR_RNDN);
	      mpfr_set_d(sc->mpfr_2, real(y), MPFR_RNDN);
	      mpfr_div(sc->mpfr_1, sc->mpfr_1, sc->mpfr_2, MPFR_RNDN);
	      return(mpfr_to_big_real(sc, sc->mpfr_1));
	    }
#endif
	  return(make_real(sc, (s7_double)(integer(x)) / real(y)));

	case T_COMPLEX:
	  {
	    s7_double r1, i2, r2, den;
	    r1 = (s7_double)integer(x);
	    r2 = real_part(y);
	    i2 = imag_part(y);
	    den = 1.0 / (r2 * r2 + i2 * i2);
	    /* we could avoid the squaring (see Knuth II p613 16), not a big deal: (/ 1.0e308+1.0e308i 2.0e308+2.0e308i) => nan, (gmp case is ok here) */
	    return(s7_make_complex(sc, r1 * r2 * den, -(r1 * i2 * den)));
	  }

#if WITH_GMP
	case T_BIG_INTEGER:
	  if (mpz_cmp_ui(big_integer(y), 0) == 0)
	    return(division_by_zero_error(sc, sc->divide_symbol, set_elist_2(sc, x, y)));
	  mpq_set_si(sc->mpq_1, integer(x), 1);
	  mpq_set_den(sc->mpq_1, big_integer(y));
	  return(mpq_to_canonicalized_rational(sc, sc->mpq_1));
	case T_BIG_RATIO:
	  mpq_set_si(sc->mpq_1, integer(x), 1);
	  mpq_div(sc->mpq_1, sc->mpq_1, big_ratio(y));
	  return(mpq_to_canonicalized_rational(sc, sc->mpq_1));
	case T_BIG_REAL:
	  if (mpfr_zero_p(big_real(y)))
	    return(division_by_zero_error(sc, sc->divide_symbol, set_elist_2(sc, x, y)));
	  mpfr_si_div(sc->mpfr_1, integer(x), big_real(y), MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	case T_BIG_COMPLEX:
	  if ((!mpfr_number_p(mpc_realref(big_complex(y)))) || (!mpfr_number_p(mpc_imagref(big_complex(y)))))
	    return(complex_NaN);
	  mpc_set_si(sc->mpc_1, integer(x), MPC_RNDNN);
	  mpc_div(sc->mpc_1, sc->mpc_1, big_complex(y), MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1)); /* x might be 0? */
#endif
	default:
	  return(method_or_bust_with_type_pp(sc, y, sc->divide_symbol, x, y, a_number_string, 2));
	}
      break;

      /* -------- ratio x -------- */
    case T_RATIO:
      switch (type(y))
	{
	case T_INTEGER:
	  if (integer(y) == 0)
	    return(division_by_zero_error(sc, sc->divide_symbol, set_elist_2(sc, x, y)));
#if HAVE_OVERFLOW_CHECKS
	  {
	    s7_int dn;
	    if (multiply_overflow(denominator(x), integer(y), &dn))
#if WITH_GMP
	      {
		mpq_set_si(sc->mpq_1, numerator(x), denominator(x));
		mpq_set_si(sc->mpq_2, integer(y), 1);
		mpq_div(sc->mpq_1, sc->mpq_1, sc->mpq_2);
		return(mpq_to_rational(sc, sc->mpq_1));
	      }
#else
	      return(make_real(sc, (long_double)numerator(x) / ((long_double)denominator(x) * (long_double)integer(y))));
#endif
	    return(s7_make_ratio(sc, numerator(x), dn));
	  }
#else
	  return(s7_make_ratio(sc, numerator(x), denominator(x) * integer(y)));
#endif

	case T_RATIO:
	  {
	    s7_int d1, d2, n1, n2;
	    parcel_out_fractions(x, y);
	    if (d1 == d2)
	      return(s7_make_ratio(sc, n1, n2));
#if HAVE_OVERFLOW_CHECKS
	    if ((multiply_overflow(n1, d2, &n1)) ||
		(multiply_overflow(n2, d1, &d1)))
	      {
#if WITH_GMP
		mpq_set_si(sc->mpq_1, numerator(x), denominator(x)); /* not n1 and d1! they are garbage here */
		mpq_set_si(sc->mpq_2, n2, d2);
		mpq_div(sc->mpq_1, sc->mpq_1, sc->mpq_2);
		return(mpq_to_rational(sc, sc->mpq_1));
#else
		s7_double r1, r2;
		r1 = fraction(x);
		r2 = inverted_fraction(y);
		return(make_real(sc, r1 * r2));
#endif
	      }
	    return(s7_make_ratio(sc, n1, d1));
#else
	    return(s7_make_ratio(sc, n1 * d2, n2 * d1));
#endif
	  }

	case T_REAL:
	  if (real(y) == 0.0)
	    return(division_by_zero_error(sc, sc->divide_symbol, set_elist_2(sc, x, y)));
	  return(make_real(sc, fraction(x) / real(y)));

	case T_COMPLEX:
	  {
	    s7_double rx, r2, i2, den;
	    rx = fraction(x);
	    r2 = real_part(y);
	    i2 = imag_part(y);
	    den = 1.0 / (r2 * r2 + i2 * i2);
	    return(s7_make_complex(sc, rx * r2 * den, -rx * i2 * den)); /* not unchecked: (/ 3/4 -inf.0+i) */
	  }

#if WITH_GMP
	case T_BIG_INTEGER:
	  if (mpz_cmp_ui(big_integer(y), 0) == 0)
	    return(division_by_zero_error(sc, sc->divide_symbol, set_elist_2(sc, x, y)));
	  mpq_set_z(sc->mpq_1, big_integer(y));
	  mpq_set_si(sc->mpq_2, numerator(x), denominator(x));
	  mpq_div(sc->mpq_1, sc->mpq_2, sc->mpq_1);
	  return(mpq_to_rational(sc, sc->mpq_1));
	case T_BIG_RATIO:
	  mpq_set_si(sc->mpq_1, numerator(x), denominator(x));
	  mpq_div(sc->mpq_1, sc->mpq_1, big_ratio(y));
	  return(mpq_to_rational(sc, sc->mpq_1));
	case T_BIG_REAL:
	  if (mpfr_zero_p(big_real(y)))
	    return(division_by_zero_error(sc, sc->divide_symbol, set_elist_2(sc, x, y)));
	  mpq_set_si(sc->mpq_1, numerator(x), denominator(x));
	  mpfr_set_q(sc->mpfr_1, sc->mpq_1, MPFR_RNDN);
	  mpfr_div(sc->mpfr_1, sc->mpfr_1, big_real(y), MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	case T_BIG_COMPLEX:
	  if ((!mpfr_number_p(mpc_realref(big_complex(y)))) || (!mpfr_number_p(mpc_imagref(big_complex(y)))))
	    return(complex_NaN);
	  mpq_set_si(sc->mpq_1, numerator(x), denominator(x));
	  mpc_set_q(sc->mpc_1, sc->mpq_1, MPC_RNDNN);
	  mpc_div(sc->mpc_1, sc->mpc_1, big_complex(y), MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
#endif
	default:
	  return(method_or_bust_with_type_pp(sc, y, sc->divide_symbol, x, y, a_number_string, 2));
	}

      /* -------- real x -------- */
    case T_REAL:
      switch (type(y))
	{
	case T_INTEGER:
	  if (integer(y) == 0)
	    return(division_by_zero_error(sc, sc->divide_symbol, set_elist_2(sc, x, y)));
	  if (is_NaN(real(x))) return(real_NaN); /* what is (/ +nan.0 0)? */
	  if (is_inf(real(x)))
	    return((real(x) > 0.0) ? ((integer(y) > 0) ? real_infinity : real_minus_infinity) : ((integer(y) > 0) ? real_minus_infinity : real_infinity));
	  return(make_real(sc, (long_double)real(x) / (long_double)integer(y)));

	case T_RATIO:
	  if (is_NaN(real(x))) return(real_NaN);
	  if (is_inf(real(x)))
	    return((real(x) > 0) ? ((numerator(y) > 0) ? real_infinity : real_minus_infinity) : ((numerator(y) > 0) ? real_minus_infinity : real_infinity));
	  return(make_real(sc, real(x) * inverted_fraction(y)));

	case T_REAL:
	  if (is_NaN(real(y))) return(real_NaN);
	  if (real(y) == 0.0)
	    return(division_by_zero_error(sc, sc->divide_symbol, set_elist_2(sc, x, y)));
	  if (is_NaN(real(x))) return(real_NaN);
	  if (is_inf(real(y)))
	    return((is_inf(real(x))) ? real_NaN : real_zero);
	  return(make_real(sc, real(x) / real(y)));

	case T_COMPLEX:
	  {
	    s7_double den, r2, i2;
	    if (is_NaN(real(x))) return(complex_NaN);
	    r2 = real_part(y);
	    i2 = imag_part(y);
	    if ((is_NaN(r2)) || (is_inf(r2))) return(complex_NaN);
	    if ((is_NaN(i2)) || (is_inf(i2))) return(complex_NaN);
	    den = 1.0 / (r2 * r2 + i2 * i2);
	    return(s7_make_complex(sc, real(x) * r2 * den, -real(x) * i2 * den));
	  }

#if WITH_GMP
	case T_BIG_INTEGER:
	  if (mpz_cmp_ui(big_integer(y), 0) == 0)
	    return(division_by_zero_error(sc, sc->divide_symbol, set_elist_2(sc, x, y)));
	  mpfr_set_z(sc->mpfr_1, big_integer(y), MPFR_RNDN);
	  mpfr_d_div(sc->mpfr_1, real(x), sc->mpfr_1, MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	case T_BIG_RATIO:
	  mpfr_set_d(sc->mpfr_1, real(x), MPFR_RNDN);
	  mpfr_div_q(sc->mpfr_1, sc->mpfr_1, big_ratio(y), MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	case T_BIG_REAL:
	  if (mpfr_zero_p(big_real(y)))
	    return(division_by_zero_error(sc, sc->divide_symbol, set_elist_2(sc, x, y)));
	  mpfr_d_div(sc->mpfr_1, real(x), big_real(y), MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	case T_BIG_COMPLEX:
	  if ((is_NaN(real(x))) || (!mpfr_number_p(mpc_realref(big_complex(y)))) || (!mpfr_number_p(mpc_imagref(big_complex(y)))))
	    return(complex_NaN);
	  mpc_set_d_d(sc->mpc_1, real(x), 0.0, MPC_RNDNN);
	  mpc_div(sc->mpc_1, sc->mpc_1, big_complex(y), MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
#endif
	default:
	  return(method_or_bust_with_type_pp(sc, y, sc->divide_symbol, x, y, a_number_string, 2));
	}

      /* -------- complex x -------- */
    case T_COMPLEX:
      switch (type(y))
	{
	case T_INTEGER:
	  {
	    s7_double r1;
	    if (integer(y) == 0)
	      return(division_by_zero_error(sc, sc->divide_symbol, set_elist_2(sc, x, y)));
	    r1 = (long_double)1.0 / (long_double)integer(y);
	    return(s7_make_complex(sc, real_part(x) * r1, imag_part(x) * r1));
	  }

	case T_RATIO:
	  {
	    s7_double frac;
	    frac = inverted_fraction(y);
	    return(make_complex(sc, real_part(x) * frac, imag_part(x) * frac));
	  }

	case T_REAL:
	  {
	    s7_double r1;
	    if (real(y) == 0.0)
	      return(division_by_zero_error(sc, sc->divide_symbol, set_elist_2(sc, x, y)));
	    r1 = 1.0 / real(y);
	    return(make_complex(sc, real_part(x) * r1, imag_part(x) * r1)); /* (/ 0.0+1.0i +inf.0) */
	  }

	case T_COMPLEX:
	  {
	    s7_double r1, r2, i1, i2, den;
	    r1 = real_part(x);
	    if (is_NaN(r1)) return(real_NaN);
	    i1 = imag_part(x);
	    if (is_NaN(i1)) return(real_NaN);
	    r2 = real_part(y);
	    if (is_NaN(r2)) return(real_NaN);
	    if (is_inf(r2)) return(complex_NaN);
	    i2 = imag_part(y);
	    if (is_NaN(i2)) return(real_NaN);
	    den = 1.0 / (r2 * r2 + i2 * i2);
	    return(s7_make_complex(sc, (r1 * r2 + i1 * i2) * den, (r2 * i1 - r1 * i2) * den));
	  }

#if WITH_GMP
	case T_BIG_INTEGER:
	  if (mpz_cmp_ui(big_integer(y), 0) == 0)
	    return(division_by_zero_error(sc, sc->divide_symbol, set_elist_2(sc, x, y)));
	  mpc_set_d_d(sc->mpc_1, real_part(x), imag_part(x), MPC_RNDNN);
	  mpc_set_z(sc->mpc_2, big_integer(y), MPC_RNDNN);
	  mpc_div(sc->mpc_1, sc->mpc_1, sc->mpc_2, MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	case T_BIG_RATIO:
	  mpc_set_d_d(sc->mpc_1, real_part(x), imag_part(x), MPC_RNDNN);
	  mpc_set_q(sc->mpc_2, big_ratio(y), MPC_RNDNN);
	  mpc_div(sc->mpc_1, sc->mpc_1, sc->mpc_2, MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	case T_BIG_REAL:
	  if (mpfr_zero_p(big_real(y)))
	    return(division_by_zero_error(sc, sc->divide_symbol, set_elist_2(sc, x, y)));
	  mpc_set_d_d(sc->mpc_1, real_part(x), imag_part(x), MPC_RNDNN);
	  mpc_div_fr(sc->mpc_1, sc->mpc_1, big_real(y), MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	case T_BIG_COMPLEX:
	  if ((!mpfr_number_p(mpc_realref(big_complex(y)))) || (!mpfr_number_p(mpc_imagref(big_complex(y)))))
	    return(complex_NaN);
	  mpc_set_d_d(sc->mpc_1, real_part(x), imag_part(x), MPC_RNDNN);
	  mpc_div(sc->mpc_1, sc->mpc_1, big_complex(y), MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
#endif
	default:
	  return(method_or_bust_with_type_pp(sc, y, sc->divide_symbol, x, y, a_number_string, 2));
	}

#if WITH_GMP
    case T_BIG_INTEGER:
      switch (type(y))
	{
	case T_INTEGER:
	  if (integer(y) == 0)
	    return(division_by_zero_error(sc, sc->divide_symbol, set_elist_2(sc, x, y)));
	  mpz_set_si(sc->mpz_1, integer(y));
	  mpq_set_num(sc->mpq_1, big_integer(x));
	  mpq_set_den(sc->mpq_1, sc->mpz_1);
	  return(mpq_to_canonicalized_rational(sc, sc->mpq_1));
	case T_RATIO:
	  mpq_set_z(sc->mpq_2, big_integer(x));
	  mpq_set_si(sc->mpq_1, numerator(y), denominator(y)); /* can't invert here, mpq den=unsigned */
	  mpq_div(sc->mpq_1, sc->mpq_2, sc->mpq_1);
	  return(mpq_to_canonicalized_rational(sc, sc->mpq_1));
	case T_REAL:
	  if (is_NaN(real(y))) return(real_NaN);
	  if (real(y) == 0.0)
	    return(division_by_zero_error(sc, sc->divide_symbol, set_elist_2(sc, x, y)));
	  mpfr_set_z(sc->mpfr_1, big_integer(x), MPFR_RNDN);
	  mpfr_div_d(sc->mpfr_1, sc->mpfr_1, real(y), MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	case T_COMPLEX:
	  if ((is_NaN(real_part(y))) || (is_NaN(imag_part(y))) ||
	      (is_inf(real_part(y))) || (is_inf(imag_part(y))))
	    return(complex_NaN);
	  mpc_set_d_d(sc->mpc_1, real_part(y), imag_part(y), MPC_RNDNN);
	  mpc_set_z(sc->mpc_2, big_integer(x), MPC_RNDNN);
	  mpc_div(sc->mpc_1, sc->mpc_2, sc->mpc_1, MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	case T_BIG_INTEGER:
	  if (mpz_cmp_ui(big_integer(y), 0) == 0)
	    return(division_by_zero_error(sc, sc->divide_symbol, set_elist_2(sc, x, y)));
	  mpq_set_num(sc->mpq_1, big_integer(x));
	  mpq_set_den(sc->mpq_1, big_integer(y));
	  return(mpq_to_canonicalized_rational(sc, sc->mpq_1));
	case T_BIG_RATIO:
	  mpq_set_si(sc->mpq_1, 0, 1);
	  mpq_set_num(sc->mpq_1, big_integer(x));
	  mpq_div(sc->mpq_1, sc->mpq_1, big_ratio(y));
	  return(mpq_to_rational(sc, sc->mpq_1));
	case T_BIG_REAL:
	  if (mpfr_zero_p(big_real(y)))
	    return(division_by_zero_error(sc, sc->divide_symbol, set_elist_2(sc, x, y)));
	  mpfr_set_z(sc->mpfr_1, big_integer(x), MPFR_RNDN);
	  mpfr_div(sc->mpfr_1, sc->mpfr_1, big_real(y), MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	case T_BIG_COMPLEX:
	  if ((!mpfr_number_p(mpc_realref(big_complex(y)))) || (!mpfr_number_p(mpc_imagref(big_complex(y)))))
	    return(complex_NaN);
	  mpc_set_z(sc->mpc_1, big_integer(x), MPC_RNDNN);
	  mpc_div(sc->mpc_1, sc->mpc_1, big_complex(y), MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	default:
	  return(method_or_bust_with_type_pp(sc, y, sc->divide_symbol, x, y, a_number_string, 2));
	}
    case T_BIG_RATIO:
      switch (type(y))
	{
	case T_INTEGER:
	  if (integer(y) == 0)
	    return(division_by_zero_error(sc, sc->divide_symbol, set_elist_2(sc, x, y)));
	  mpq_set_si(sc->mpq_1, integer(y), 1);
	  mpq_div(sc->mpq_1, big_ratio(x), sc->mpq_1);
	  return(mpq_to_rational(sc, sc->mpq_1));
	case T_RATIO:
	  mpq_set_si(sc->mpq_1, numerator(y), denominator(y));
	  mpq_div(sc->mpq_1, big_ratio(x), sc->mpq_1);
	  return(mpq_to_rational(sc, sc->mpq_1));
	case T_REAL:
	  if (is_NaN(real(y))) return(real_NaN);
	  if (real(y) == 0.0)
	    return(division_by_zero_error(sc, sc->divide_symbol, set_elist_2(sc, x, y)));
	  mpfr_set_q(sc->mpfr_1, big_ratio(x), MPFR_RNDN);
	  mpfr_div_d(sc->mpfr_1, sc->mpfr_1, real(y), MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	case T_COMPLEX:
	  if ((is_NaN(real_part(y))) || (is_NaN(imag_part(y))) ||
	      (is_inf(real_part(y))) || (is_inf(imag_part(y))))
	    return(complex_NaN);
	  mpc_set_q(sc->mpc_1, big_ratio(x), MPC_RNDNN);
	  mpc_set_d_d(sc->mpc_2, real_part(y), imag_part(y), MPC_RNDNN);
	  mpc_div(sc->mpc_1, sc->mpc_1, sc->mpc_2, MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	case T_BIG_INTEGER:
	  if (mpz_cmp_ui(big_integer(y), 0) == 0)
	    return(division_by_zero_error(sc, sc->divide_symbol, set_elist_2(sc, x, y)));
	  mpq_set_z(sc->mpq_1, big_integer(y));
	  mpq_div(sc->mpq_1, big_ratio(x), sc->mpq_1);
	  return(mpq_to_canonicalized_rational(sc, sc->mpq_1));
	case T_BIG_RATIO:
	  mpq_div(sc->mpq_1, big_ratio(x), big_ratio(y));
	  return(mpq_to_canonicalized_rational(sc, sc->mpq_1));
	case T_BIG_REAL:
	  if (mpfr_zero_p(big_real(y)))
	    return(division_by_zero_error(sc, sc->divide_symbol, set_elist_2(sc, x, y)));
	  mpfr_set_q(sc->mpfr_1, big_ratio(x), MPFR_RNDN);
	  mpfr_div(sc->mpfr_1, sc->mpfr_1, big_real(y), MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	case T_BIG_COMPLEX:
	  if ((!mpfr_number_p(mpc_realref(big_complex(y)))) || (!mpfr_number_p(mpc_imagref(big_complex(y)))))
	    return(complex_NaN);
	  mpc_set_q(sc->mpc_1, big_ratio(x), MPC_RNDNN);
	  mpc_div(sc->mpc_1, sc->mpc_1, big_complex(y), MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	default:
	  return(method_or_bust_with_type_pp(sc, y, sc->divide_symbol, x, y, a_number_string, 2));
	}
    case T_BIG_REAL:
      switch (type(y))
	{
	case T_INTEGER:
	  if (integer(y) == 0)
	    return(division_by_zero_error(sc, sc->divide_symbol, set_elist_2(sc, x, y)));
	  mpfr_div_si(sc->mpfr_1, big_real(x), integer(y), MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	case T_RATIO:
	  mpq_set_si(sc->mpq_1, numerator(y), denominator(y));
	  mpfr_div_q(sc->mpfr_1, big_real(x), sc->mpq_1, MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	case T_REAL:
	  if (is_NaN(real(y))) return(real_NaN);
	  if (real(y) == 0.0)
	    return(division_by_zero_error(sc, sc->divide_symbol, set_elist_2(sc, x, y)));
	  mpfr_div_d(sc->mpfr_1, big_real(x), real(y), MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	case T_COMPLEX:
	  if ((is_NaN(real_part(y))) || (is_NaN(imag_part(y))) ||
	      (is_inf(real_part(y))) || (is_inf(imag_part(y))))
	    return(complex_NaN);
	  mpc_set_d_d(sc->mpc_1, real_part(y), imag_part(y), MPC_RNDNN);
	  mpc_fr_div(sc->mpc_1, big_real(x), sc->mpc_1, MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	case T_BIG_INTEGER:
	  if (mpz_cmp_ui(big_integer(y), 0) == 0)
	    return(division_by_zero_error(sc, sc->divide_symbol, set_elist_2(sc, x, y)));
	  mpfr_div_z(sc->mpfr_1, big_real(x), big_integer(y), MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	case T_BIG_RATIO:
	  mpfr_div_q(sc->mpfr_1, big_real(x), big_ratio(y), MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	case T_BIG_REAL:
	  if (mpfr_zero_p(big_real(y)))
	    return(division_by_zero_error(sc, sc->divide_symbol, set_elist_2(sc, x, y)));
	  mpfr_div(sc->mpfr_1, big_real(x), big_real(y), MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	case T_BIG_COMPLEX:
	  if ((!mpfr_number_p(mpc_realref(big_complex(y)))) || (!mpfr_number_p(mpc_imagref(big_complex(y)))))
	    return(complex_NaN);
	  mpc_fr_div(sc->mpc_1, big_real(x), big_complex(y), MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	default:
	  return(method_or_bust_with_type_pp(sc, y, sc->divide_symbol, x, y, a_number_string, 2));
	}
    case T_BIG_COMPLEX:
      switch (type(y))
	{
	case T_INTEGER:
	  if (integer(y) == 0)
	    return(division_by_zero_error(sc, sc->divide_symbol, set_elist_2(sc, x, y)));
	  mpc_set_si(sc->mpc_1, integer(y), MPC_RNDNN);
	  mpc_div(sc->mpc_1, big_complex(x), sc->mpc_1, MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	case T_RATIO:
	  mpq_set_si(sc->mpq_1, numerator(y), denominator(y));
	  mpc_set_q(sc->mpc_1, sc->mpq_1, MPC_RNDNN);
	  mpc_div(sc->mpc_1, big_complex(x), sc->mpc_1, MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	case T_REAL:
	  /* if (is_NaN(real(y))) return(real_NaN); */
	  if (real(y) == 0.0)
	    return(division_by_zero_error(sc, sc->divide_symbol, set_elist_2(sc, x, y)));
	  mpc_set_d_d(sc->mpc_1, real(y), 0.0, MPC_RNDNN);
	  mpc_div(sc->mpc_1, big_complex(x), sc->mpc_1, MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	case T_COMPLEX:
	  if ((is_NaN(real_part(y))) || (is_NaN(imag_part(y))) ||
	      (is_inf(real_part(y))) || (is_inf(imag_part(y))))
	    return(complex_NaN);
	  mpc_set_d_d(sc->mpc_1, real_part(y), imag_part(y), MPC_RNDNN);
	  mpc_div(sc->mpc_1, big_complex(x), sc->mpc_1, MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	case T_BIG_INTEGER:
	  if (mpz_cmp_ui(big_integer(y), 0) == 0)
	    return(division_by_zero_error(sc, sc->divide_symbol, set_elist_2(sc, x, y)));
	  mpc_set_z(sc->mpc_1, big_integer(y), MPC_RNDNN);
	  mpc_div(sc->mpc_1, big_complex(x), sc->mpc_1, MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	case T_BIG_RATIO:
	  mpc_set_q(sc->mpc_1, big_ratio(y), MPC_RNDNN);
	  mpc_div(sc->mpc_1, big_complex(x), sc->mpc_1, MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	case T_BIG_REAL:
	  if (mpfr_zero_p(big_real(y)))
	    return(division_by_zero_error(sc, sc->divide_symbol, set_elist_2(sc, x, y)));
	  mpc_div_fr(sc->mpc_1, big_complex(x), big_real(y), MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	case T_BIG_COMPLEX:
	  if ((!mpfr_number_p(mpc_realref(big_complex(y)))) || (!mpfr_number_p(mpc_imagref(big_complex(y)))))
	    return(complex_NaN);
	  mpc_div(sc->mpc_1, big_complex(x), big_complex(y), MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	default:
	  return(method_or_bust_with_type_pp(sc, y, sc->divide_symbol, x, y, a_number_string, 2));
	}
#endif

    default: /* x is not a built-in number */
      return(method_or_bust_with_type_pp(sc, x, sc->divide_symbol, x, y, a_number_string, 1)); /* not args here! y = apply * to cdr(args) */
    }
  return(NULL); /* make the compiler happy */
}

static s7_pointer g_divide(s7_scheme *sc, s7_pointer args)
{
  #define H_divide "(/ x1 ...) divides its first argument by the rest, or inverts the first if there is only one argument"
  #define Q_divide sc->pcl_n

  s7_pointer x, y, p;
  x = car(args);
  p = cdr(args);
  if (is_null(p))            /* (/ x) */
    {
      if (!is_number(x))
	return(method_or_bust_with_type_one_arg(sc, x, sc->divide_symbol, args, a_number_string));
      return(invert_p_p(sc, x));
    }
  if (is_null(cdr(p)))
    return(divide_p_pp(sc, x, cadr(args)));
  y = g_multiply(sc, p); /* in some schemes (/ 1 0 +nan.0) is not equal to (/ 1 (* 0 +nan.0)), in s7 they're both +nan.0 */
  return(divide_p_pp(sc, x, y));
}

static s7_pointer g_invert_1(s7_scheme *sc, s7_pointer args) {return(invert_p_p(sc, car(args)));}
static s7_pointer g_divide_2(s7_scheme *sc, s7_pointer args) {return(divide_p_pp(sc, car(args), cadr(args)));}

static s7_pointer g_divide_by_2(s7_scheme *sc, s7_pointer args)
{
  /* (/ x 2) */
  s7_pointer num;
  num = car(args);
  if (is_t_integer(num))
    {
      s7_int i;
      i = integer(num);
      if (i & 1)
	{
	  s7_pointer x;
	  new_cell(sc, x, T_RATIO);
	  numerator(x) = i;
	  denominator(x) = 2;
	  return(x);
	}
      return(make_integer(sc, i >> 1));
    }
  switch (type(num))
    {
    case T_RATIO:
#if HAVE_OVERFLOW_CHECKS
      {
	s7_int dn;
	if (multiply_overflow(denominator(num), 2, &dn))
	  {
	    if ((numerator(num) & 1) == 1)
#if WITH_GMP
	      {
		mpq_set_si(sc->mpq_1, numerator(num), denominator(num));
		mpq_set_si(sc->mpq_2, 1, 2);
		mpq_mul(sc->mpq_1, sc->mpq_1, sc->mpq_2);
		return(mpq_to_rational(sc, sc->mpq_1));
	      }
#else
	      return(make_real(sc, ((long_double)numerator(num) * 0.5) / (long_double)denominator(num)));
#endif
	    return(s7_make_ratio(sc, numerator(num) / 2, denominator(num)));
	  }
	return(s7_make_ratio(sc, numerator(num), dn));
      }
#else
      return(s7_make_ratio(sc, numerator(num), denominator(num) * 2));
#endif

    case T_REAL:    return(make_real(sc, real(num) * 0.5));
    case T_COMPLEX: return(make_complex_unchecked(sc, real_part(num) * 0.5, imag_part(num) * 0.5));

#if WITH_GMP
    case T_BIG_INTEGER:
      mpq_set_z(sc->mpq_1, big_integer(num));
      mpz_mul_ui(mpq_denref(sc->mpq_1), mpq_denref(sc->mpq_1), 2);
      return(mpq_to_canonicalized_rational(sc, sc->mpq_1));
    case T_BIG_RATIO:
      mpq_set_si(sc->mpq_1, 2, 1);
      mpq_div(sc->mpq_1, big_ratio(num), sc->mpq_1);
      return(mpq_to_canonicalized_rational(sc, sc->mpq_1));
    case T_BIG_REAL:
      mpfr_div_si(sc->mpfr_1, big_real(num), 2, MPFR_RNDN);
      return(mpfr_to_big_real(sc, sc->mpfr_1));
    case T_BIG_COMPLEX:
      mpc_set_si(sc->mpc_1, 2, MPC_RNDNN);
      mpc_div(sc->mpc_1, big_complex(num), sc->mpc_1, MPC_RNDNN);
      return(mpc_to_number(sc, sc->mpc_1));
#endif
    default:
      return(method_or_bust_with_type_pp(sc, num, sc->divide_symbol, num, int_two, a_number_string, 1));
    }
}

static s7_pointer g_invert_x(s7_scheme *sc, s7_pointer args)
{
  /* (/ 1.0 x) */
  if (is_t_real(cadr(args)))
    {
      s7_double rl;
      rl = s7_real(cadr(args));
      if (rl == 0.0)
	return(division_by_zero_error(sc, sc->divide_symbol, args));
      return((is_NaN(rl)) ? real_NaN : make_real(sc, 1.0 / rl));
    }
  return(g_divide(sc, args));
}

static s7_double divide_d_7d(s7_scheme *sc, s7_double x)
{
  if (x == 0.0) division_by_zero_error(sc, sc->divide_symbol, set_elist_1(sc, real_zero));
  return(1.0 / x);
}

static s7_double divide_d_7dd(s7_scheme *sc, s7_double x1, s7_double x2)
{
  if (x2 == 0.0) division_by_zero_error(sc, sc->divide_symbol, set_elist_1(sc, real_zero));
  return(x1 / x2);
}

static s7_pointer divide_p_ii(s7_scheme *sc, s7_int x, s7_int y) {return(s7_make_ratio(sc, x, y));} /* make-ratio checks for y==0 */
static s7_pointer divide_p_i(s7_scheme *sc, s7_int x) {return(s7_make_ratio(sc, 1, x));}

static s7_pointer divide_chooser(s7_scheme *sc, s7_pointer f, int32_t args, s7_pointer expr, bool ops)
{
  if (args == 1)
    return(sc->invert_1);
  if ((ops) && (args == 2))
    {
      s7_pointer arg1;
      arg1 = cadr(expr);
      if ((is_t_real(arg1)) && (real(arg1) == 1.0))
	return(sc->invert_x);
      return(((is_t_integer(caddr(expr))) && (integer(caddr(expr)) == 2)) ? sc->divide_by_2 : sc->divide_2);
    }
  return(f);
}

