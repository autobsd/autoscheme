static s7_pointer g_atan(s7_scheme *sc, s7_pointer args)
{
  #define H_atan "(atan z) returns atan(z), (atan y x) returns atan(y/x)"
  #define Q_atan s7_make_signature(sc, 3, sc->is_number_symbol, sc->is_number_symbol, sc->is_real_symbol)
  /* actually if there are two args, both should be real, but how to express that in the signature? */

  s7_pointer x, y;
  /* currently (atan inf.0 inf.0) -> 0.78539816339745, and (atan inf.0 -inf.0) -> 2.3561944901923 (etc) */

  x = car(args);
  if (!is_pair(cdr(args)))
    {
      switch (type(x))
	{
	case T_INTEGER:
	  return((integer(x) == 0) ? int_zero : make_real(sc, atan((double)integer(x))));

	case T_RATIO:
	  return(make_real(sc, atan(fraction(x))));

	case T_REAL:
	  return(make_real(sc, atan(real(x))));

	case T_COMPLEX:
#if HAVE_COMPLEX_NUMBERS
	  return(c_complex_to_s7(sc, catan(to_c_complex(x))));
#else
	  return(out_of_range(sc, sc->atan_symbol, int_one, x, no_complex_numbers_string));
#endif

#if WITH_GMP
	case T_BIG_INTEGER:
	  mpfr_set_z(sc->mpfr_1, big_integer(x), MPFR_RNDN);
	  mpfr_atan(sc->mpfr_1, sc->mpfr_1, MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));

	case T_BIG_RATIO:
	  mpfr_set_q(sc->mpfr_1, big_ratio(x), MPFR_RNDN);
	  mpfr_atan(sc->mpfr_1, sc->mpfr_1, MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));

	case T_BIG_REAL:
	  mpfr_atan(sc->mpfr_1, big_real(x), MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));

	case T_BIG_COMPLEX:
	  mpc_atan(sc->mpc_1, big_complex(x), MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
#endif
	default:
	  return(method_or_bust_with_type_one_arg(sc, x, sc->atan_symbol, args, a_number_string));
	}}

  y = cadr(args);
  switch (type(x))
    {
    case T_INTEGER: case T_RATIO: case T_REAL:
      if (is_small_real(y))
	return(make_real(sc, atan2(s7_real(x), s7_real(y))));
#if WITH_GMP
      if (!s7_is_real(y))
	return(method_or_bust(sc, y, sc->atan_symbol, args, T_REAL, 2));
      mpfr_set_d(sc->mpfr_1, s7_real(x), MPFR_RNDN);
      goto ATAN2_BIG_REAL;
    case T_BIG_INTEGER:
      mpfr_set_z(sc->mpfr_1, big_integer(x), MPFR_RNDN);
      goto ATAN2_BIG_REAL;
    case T_BIG_RATIO:
      mpfr_set_q(sc->mpfr_1, big_ratio(x), MPFR_RNDN);
      goto ATAN2_BIG_REAL;
    case T_BIG_REAL:
      mpfr_set(sc->mpfr_1, big_real(x), MPFR_RNDN);
      goto ATAN2_BIG_REAL;
#endif
    default:
      return(method_or_bust(sc, x, sc->atan_symbol, args, T_REAL, 1));
    }
#if WITH_GMP
 ATAN2_BIG_REAL:
  if (is_small_real(y))
    mpfr_set_d(sc->mpfr_2, s7_real(y), MPFR_RNDN);
  else
    if (is_t_big_real(y))
      mpfr_set(sc->mpfr_2, big_real(y), MPFR_RNDN);
    else
      if (is_t_big_integer(y))
	mpfr_set_z(sc->mpfr_2, big_integer(y), MPFR_RNDN);
      else
	if (is_t_big_ratio(y))
	  mpfr_set_q(sc->mpfr_2, big_ratio(y), MPFR_RNDN);
	else return(method_or_bust(sc, y, sc->atan_symbol, args, T_REAL, 2));
  mpfr_atan2(sc->mpfr_1, sc->mpfr_1, sc->mpfr_2, MPFR_RNDN);
  return(mpfr_to_big_real(sc, sc->mpfr_1));
#endif
}

static s7_double atan_d_dd(s7_double x, s7_double y) {return(atan2(x, y));}
