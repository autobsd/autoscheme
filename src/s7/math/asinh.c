static s7_pointer g_asinh(s7_scheme *sc, s7_pointer args)
{
  #define H_asinh "(asinh z) returns asinh(z)"
  #define Q_asinh sc->pl_nn

  s7_pointer x;
  x = car(args);
  switch (type(x))
    {
    case T_INTEGER:
      return((integer(x) == 0) ? int_zero : make_real(sc, asinh((s7_double)integer(x))));

    case T_RATIO:
      return(make_real(sc, asinh(fraction(x))));

    case T_REAL:
      return(make_real(sc, asinh(real(x))));

    case T_COMPLEX:
#if HAVE_COMPLEX_NUMBERS
  #if (defined(__OpenBSD__)) || (defined(__NetBSD__))
      return(c_complex_to_s7(sc, casinh_1(to_c_complex(x))));
  #else
      return(c_complex_to_s7(sc, casinh(to_c_complex(x))));
  #endif
#else
      return(out_of_range(sc, sc->asinh_symbol, int_one, x, no_complex_numbers_string));
#endif

#if WITH_GMP
    case T_BIG_INTEGER:
      mpfr_set_z(sc->mpfr_1, big_integer(x), MPFR_RNDN);
      mpfr_asinh(sc->mpfr_1, sc->mpfr_1, MPFR_RNDN);
      return(mpfr_to_big_real(sc, sc->mpfr_1));

    case T_BIG_RATIO:
      mpfr_set_q(sc->mpfr_1, big_ratio(x), MPFR_RNDN);
      mpfr_asinh(sc->mpfr_1, sc->mpfr_1, MPFR_RNDN);
      return(mpfr_to_big_real(sc, sc->mpfr_1));

    case T_BIG_REAL:
      mpfr_asinh(sc->mpfr_1, big_real(x), MPFR_RNDN);
      return(mpfr_to_big_real(sc, sc->mpfr_1));

    case T_BIG_COMPLEX:
      mpc_asinh(sc->mpc_1, big_complex(x), MPC_RNDNN);
      return(mpc_to_number(sc, sc->mpc_1));
#endif
    default:
      return(method_or_bust_with_type_one_arg_p(sc, x, sc->asinh_symbol, a_number_string));
    }
}

