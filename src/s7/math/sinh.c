static s7_pointer g_sinh(s7_scheme *sc, s7_pointer args)
{
  #define H_sinh "(sinh z) returns sinh(z)"
  #define Q_sinh sc->pl_nn

  s7_pointer x;
  x = car(args);
  switch (type(x))
    {
    case T_INTEGER:
      if (integer(x) == 0) return(int_zero);                    /* (sinh 0) -> 0 */

    case T_REAL:
    case T_RATIO:
      {
	s7_double y;
	y = s7_real(x);
#if WITH_GMP
	if (fabs(y) > SINH_LIMIT)
	  {
	    mpfr_set_d(sc->mpfr_1, y, MPFR_RNDN);
	    mpfr_sinh(sc->mpfr_1, sc->mpfr_1, MPFR_RNDN);
	    return(mpfr_to_big_real(sc, sc->mpfr_1));
	  }
#endif
	return(make_real(sc, sinh(y)));
      }

    case T_COMPLEX:
#if WITH_GMP
      if ((fabs(real_part(x)) > SINH_LIMIT) || (fabs(imag_part(x)) > SINH_LIMIT))
	{
	  mpc_set_d_d(sc->mpc_1, real_part(x), imag_part(x), MPC_RNDNN);
	  mpc_sinh(sc->mpc_1, sc->mpc_1, MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	}
#endif
#if HAVE_COMPLEX_NUMBERS
      return(c_complex_to_s7(sc, csinh(to_c_complex(x))));
#else
      return(out_of_range(sc, sc->sinh_symbol, int_one, x, no_complex_numbers_string));
#endif

#if WITH_GMP
    case T_BIG_INTEGER:
      mpfr_set_z(sc->mpfr_1, big_integer(x), MPFR_RNDN);
      mpfr_sinh(sc->mpfr_1, sc->mpfr_1, MPFR_RNDN);
      return(mpfr_to_big_real(sc, sc->mpfr_1));

    case T_BIG_RATIO:
      mpfr_set_q(sc->mpfr_1, big_ratio(x), MPFR_RNDN);
      mpfr_sinh(sc->mpfr_1, sc->mpfr_1, MPFR_RNDN);
      return(mpfr_to_big_real(sc, sc->mpfr_1));

    case T_BIG_REAL:
      mpfr_sinh(sc->mpfr_1, big_real(x), MPFR_RNDN);
      return(mpfr_to_big_real(sc, sc->mpfr_1));

    case T_BIG_COMPLEX:
      mpc_sinh(sc->mpc_1, big_complex(x), MPC_RNDNN);
      if (mpfr_zero_p(mpc_imagref(sc->mpc_1)))
	return(mpfr_to_big_real(sc, mpc_realref(sc->mpc_1)));
      return(mpc_to_number(sc, sc->mpc_1));
#endif
    default:
      return(method_or_bust_with_type_one_arg(sc, x, sc->sinh_symbol, args, a_number_string));
    }
}

#if (!WITH_GMP)
static s7_double sinh_d_d(s7_double x) {return(sinh(x));}
#endif

