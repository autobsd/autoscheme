static s7_pointer floor_p_p(s7_scheme *sc, s7_pointer x)
{
  switch (type(x))
    {
    case T_INTEGER:
      return(x);

    case T_RATIO:
      {
	s7_int val;
	val = numerator(x) / denominator(x);
	/* C "/" truncates? -- C spec says "truncation toward 0" */
	/* we're avoiding "floor" here because the int->double conversion introduces inaccuracies for big numbers
	 *   but it's used by opt_i_d_c (via s7_number_to_real) so floor_i_7d below can return different results:
	 *   (let () (define (func) (do ((i 0 (+ i 1))) ((= i 1)) (display (floor 3441313796169221281/1720656898084610641)) (newline))) (func)): 1
	 *   (let () (define (func) (do ((i 0 (+ i 1))) ((= i 1)) (display (/ (floor 3441313796169221281/1720656898084610641))) (newline))) (func)): 1/2
	 */
	return((numerator(x) < 0) ? make_integer(sc, val - 1) : make_integer(sc, val)); /* not "val" because it might be truncated to 0 */
      }

    case T_REAL:
      {
	s7_double z;
	z = real(x);
	if (is_NaN(z))
	  return(simple_out_of_range(sc, sc->floor_symbol, x, its_nan_string));
	if (is_inf(z))
	  return(simple_out_of_range(sc, sc->floor_symbol, x, its_infinite_string));
#if WITH_GMP
	if (fabs(z) > DOUBLE_TO_INT64_LIMIT)
	  {
	    mpfr_set_d(sc->mpfr_1, z, MPFR_RNDN);
	    mpfr_get_z(sc->mpz_1, sc->mpfr_1, MPFR_RNDD);
	    return(mpz_to_integer(sc, sc->mpz_1));
	  }
#else
	if (fabs(z) > DOUBLE_TO_INT64_LIMIT)
	  return(simple_out_of_range(sc, sc->floor_symbol, x, its_too_large_string));
#endif
	return(make_integer(sc, (s7_int)floor(z)));
	/* floor here rounds down, whereas a straight int<=real coercion apparently rounds towards 0 */
      }

#if WITH_GMP
    case T_BIG_INTEGER:
      return(x);

    case T_BIG_RATIO:
      mpz_fdiv_q(sc->mpz_1, mpq_numref(big_ratio(x)), mpq_denref(big_ratio(x)));
      return(mpz_to_integer(sc, sc->mpz_1));

    case T_BIG_REAL:
      if (mpfr_nan_p(big_real(x)))
	return(simple_out_of_range(sc, sc->floor_symbol, x, its_nan_string));
      if (mpfr_inf_p(big_real(x)))
	return(simple_out_of_range(sc, sc->floor_symbol, x, its_infinite_string));
      mpfr_get_z(sc->mpz_1, big_real(x), MPFR_RNDD);
      return(mpz_to_integer(sc, sc->mpz_1));

    case T_BIG_COMPLEX:
#endif
    case T_COMPLEX:
      return(s7_wrong_type_arg_error(sc, "floor", 0, x, "a real number"));

    default:
      return(method_or_bust_one_arg_p(sc, x, sc->floor_symbol, T_REAL));
    }
}

static s7_pointer g_floor(s7_scheme *sc, s7_pointer args)
{
  #define H_floor "(floor x) returns the integer closest to x toward -inf"
  #define Q_floor s7_make_signature(sc, 2, sc->is_integer_symbol, sc->is_real_symbol)
  return(floor_p_p(sc, car(args)));
}

static s7_int floor_i_i(s7_int i) {return(i);}

#if (!WITH_GMP)
static s7_int floor_i_7d(s7_scheme *sc, s7_double x)
{
  if (is_NaN(x))
    simple_out_of_range(sc, sc->floor_symbol, wrap_real1(sc, x), its_nan_string);
  if (fabs(x) > DOUBLE_TO_INT64_LIMIT)
    simple_out_of_range(sc, sc->floor_symbol, wrap_real1(sc, x), its_too_large_string);
  return((s7_int)floor(x));
}

static s7_int floor_i_7p(s7_scheme *sc, s7_pointer p)
{
  if (is_t_integer(p)) return(integer(p));
  if (is_t_real(p)) return(floor_i_7d(sc, real(p)));
  if (is_t_ratio(p)) /* for consistency with floor_p_p, don't use floor(fraction(p)) */
    {
      s7_int val;
      val = numerator(p) / denominator(p);
      return((numerator(p) < 0) ? val - 1 : val);
    }
  return(s7_integer_checked(sc, method_or_bust_p(sc, p, sc->floor_symbol, T_REAL)));
}
#endif

