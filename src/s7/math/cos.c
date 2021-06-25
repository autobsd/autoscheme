static s7_pointer cos_p_p(s7_scheme *sc, s7_pointer x)
{
#if (!WITH_GMP)
  if (is_t_real(x)) return(make_real(sc, cos(real(x)))); /* range check in gmp case */
#endif
  switch (type(x))
    {
    case T_INTEGER:
      if (integer(x) == 0) return(int_one);             /* (cos 0) -> 1 */
#if WITH_GMP
      if (integer(x) > SIN_LIMIT)
	{
	  mpz_set_si(sc->mpz_1, integer(x));
	  mpfr_set_z(sc->mpfr_1, sc->mpz_1, MPFR_RNDN);
	  mpfr_cos(sc->mpfr_1, sc->mpfr_1, MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	}
#endif
      return(make_real(sc, cos((s7_double)(integer(x)))));

    case T_RATIO:
      return(make_real(sc, cos((s7_double)(fraction(x)))));

    case T_REAL: /* if with_gmp */
      {
	s7_double y;
	y = real(x);
#if WITH_GMP
	if (fabs(y) > SIN_LIMIT)
	  {
	    mpfr_set_d(sc->mpfr_1, y, MPFR_RNDN);
	    mpfr_cos(sc->mpfr_1, sc->mpfr_1, MPFR_RNDN);
	    return(mpfr_to_big_real(sc, sc->mpfr_1));
	  }
#endif
	return(make_real(sc, cos(y)));
      }

    case T_COMPLEX:
#if WITH_GMP
      if ((fabs(real_part(x)) > SIN_LIMIT) || (fabs(imag_part(x)) > SINH_LIMIT))
	{
	  mpc_set_d_d(sc->mpc_1, real_part(x), imag_part(x), MPC_RNDNN);
	  mpc_cos(sc->mpc_1, sc->mpc_1, MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	}
#endif
#if HAVE_COMPLEX_NUMBERS
      return(c_complex_to_s7(sc, ccos(to_c_complex(x))));
#else
      return(out_of_range(sc, sc->cos_symbol, int_one, x, no_complex_numbers_string));
#endif

#if WITH_GMP
    case T_BIG_INTEGER:
      mpfr_set_z(sc->mpfr_1, big_integer(x), MPFR_RNDN);
      mpfr_cos(sc->mpfr_1, sc->mpfr_1, MPFR_RNDN);
      return(mpfr_to_big_real(sc, sc->mpfr_1));

    case T_BIG_RATIO:
      mpfr_set_q(sc->mpfr_1, big_ratio(x), MPFR_RNDN);
      mpfr_cos(sc->mpfr_1, sc->mpfr_1, MPFR_RNDN);
      return(mpfr_to_big_real(sc, sc->mpfr_1));

    case T_BIG_REAL:
      mpfr_cos(sc->mpfr_1, big_real(x), MPFR_RNDN);
      return(mpfr_to_big_real(sc, sc->mpfr_1));

    case T_BIG_COMPLEX:
      mpc_cos(sc->mpc_1, big_complex(x), MPC_RNDNN);
      if (mpfr_zero_p(mpc_imagref(sc->mpc_1)))
	return(mpfr_to_big_real(sc, mpc_realref(sc->mpc_1)));
      return(mpc_to_number(sc, sc->mpc_1));
#endif
    default:
      return(method_or_bust_with_type_one_arg_p(sc, x, sc->cos_symbol, a_number_string));
    }
}

static s7_pointer g_cos(s7_scheme *sc, s7_pointer args)
{
  #define H_cos "(cos z) returns cos(z)"
  #define Q_cos sc->pl_nn
  return(cos_p_p(sc, car(args)));
}

#if WITH_GMP
static s7_pointer cos_p_d(s7_scheme *sc, s7_double x)
{
  if (fabs(x) > SIN_LIMIT)
    {
      mpfr_set_d(sc->mpfr_1, x, MPFR_RNDN);
      mpfr_cos(sc->mpfr_1, sc->mpfr_1, MPFR_RNDN);
      return(mpfr_to_big_real(sc, sc->mpfr_1));
    }
  return(make_real(sc, cos(x)));
}
#else
static s7_double cos_d_d(s7_double x) {return(cos(x));}
static s7_pointer cos_p_d(s7_scheme *sc, s7_double x) {return(make_real(sc, cos(x)));}
#endif

