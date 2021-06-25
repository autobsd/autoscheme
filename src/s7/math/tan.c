#define TAN_LIMIT 1.0e18

static s7_pointer tan_p_p(s7_scheme *sc, s7_pointer x)
{
#if (!WITH_GMP)
  if (is_t_real(x)) return(make_real(sc, tan(real(x))));
#endif
  switch (type(x))
    {
    case T_INTEGER:
      if (integer(x) == 0) return(int_zero);                      /* (tan 0) -> 0 */
#if WITH_GMP
      if (integer(x) > TAN_LIMIT)
	{
	  mpz_set_si(sc->mpz_1, integer(x));
	  mpfr_set_z(sc->mpfr_1, sc->mpz_1, MPFR_RNDN);
	  mpfr_tan(sc->mpfr_1, sc->mpfr_1, MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	}
#endif
      return(make_real(sc, tan((s7_double)(integer(x)))));

    case T_RATIO:
      return(make_real(sc, tan((s7_double)(fraction(x)))));

#if WITH_GMP
    case T_REAL:
      if (fabs(real(x)) > TAN_LIMIT)
	{
	  mpfr_set_d(sc->mpfr_1, real(x), MPFR_RNDN);
	  mpfr_tan(sc->mpfr_1, sc->mpfr_1, MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	}
#endif
      return(make_real(sc, tan(real(x))));

    case T_COMPLEX:
#if HAVE_COMPLEX_NUMBERS
      if (imag_part(x) > 350.0)
	return(s7_make_complex(sc, 0.0, 1.0));
      return((imag_part(x) < -350.0) ? s7_make_complex(sc, 0.0, -1.0) : c_complex_to_s7(sc, ctan(to_c_complex(x))));
#else
      return(out_of_range(sc, sc->tan_symbol, int_one, x, no_complex_numbers_string));
#endif

#if WITH_GMP
    case T_BIG_INTEGER:
      mpfr_set_z(sc->mpfr_1, big_integer(x), MPFR_RNDN);
      mpfr_tan(sc->mpfr_1, sc->mpfr_1, MPFR_RNDN);
      return(mpfr_to_big_real(sc, sc->mpfr_1));

    case T_BIG_RATIO:
      mpfr_set_q(sc->mpfr_1, big_ratio(x), MPFR_RNDN);
      mpfr_tan(sc->mpfr_1, sc->mpfr_1, MPFR_RNDN);
      return(mpfr_to_big_real(sc, sc->mpfr_1));

    case T_BIG_REAL:
      mpfr_tan(sc->mpfr_1, big_real(x), MPFR_RNDN);
      return(mpfr_to_big_real(sc, sc->mpfr_1));

    case T_BIG_COMPLEX:
      if ((MPC_INEX_IM(mpc_cmp_si_si(big_complex(x), 1, 350))) > 0)
	return(s7_make_complex(sc, 0.0, 1.0));
      if ((MPC_INEX_IM(mpc_cmp_si_si(big_complex(x), 1, -350))) < 0)
	return(s7_make_complex(sc, 0.0, -1.0));
      mpc_tan(sc->mpc_1, big_complex(x), MPC_RNDNN);
      if (mpfr_zero_p(mpc_imagref(sc->mpc_1)))
	return(mpfr_to_big_real(sc, mpc_realref(sc->mpc_1)));
      return(mpc_to_number(sc, sc->mpc_1));
#endif
    default:
      return(method_or_bust_with_type_one_arg_p(sc, x, sc->tan_symbol, a_number_string));
    }
}

static s7_pointer g_tan(s7_scheme *sc, s7_pointer args)
{
  #define H_tan "(tan z) returns tan(z)"
  #define Q_tan sc->pl_nn
  return(tan_p_p(sc, car(args)));
}

static s7_double tan_d_d(s7_double x) {return(tan(x));}

