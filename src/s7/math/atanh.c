static s7_pointer g_atanh(s7_scheme *sc, s7_pointer args)
{
  #define H_atanh "(atanh z) returns atanh(z)"
  #define Q_atanh sc->pl_nn

  s7_pointer x;
  x = car(args);
  switch (type(x))
    {
    case T_INTEGER:
      if (integer(x) == 0) return(int_zero);                    /* (atanh 0) -> 0 */

    case T_REAL:
    case T_RATIO:
      {
	double x1;
	x1 = s7_real(x);
	if (fabs(x1) < 1.0)
	  return(make_real(sc, atanh(x1)));
      }
      /* if we can't distinguish x from 1.0 even with long_doubles, we'll get inf.0:
       *    (atanh 9223372036854775/9223372036854776) -> 18.714973875119
       *    (atanh 92233720368547758/92233720368547757) -> inf.0
       *    (atanh (bignum 92233720368547758/92233720368547757)) -> 1.987812468492420421418925013176932317086E1+1.570796326794896619231321691639751442098E0i
       *    but the imaginary part is unnecessary
       */
    case T_COMPLEX:
#if HAVE_COMPLEX_NUMBERS
  #if (defined(__OpenBSD__)) || (defined(__NetBSD__))
      return(c_complex_to_s7(sc, catanh_1(s7_to_c_complex(x))));
  #else
      return(c_complex_to_s7(sc, catanh(s7_to_c_complex(x))));
  #endif
#else
      return(out_of_range(sc, sc->atanh_symbol, int_one, x, no_complex_numbers_string));
#endif

#if WITH_GMP
    case T_BIG_INTEGER:
      mpfr_set_z(sc->mpfr_2, big_integer(x), MPFR_RNDN);
      goto ATANH_BIG_REAL;

    case T_BIG_RATIO:
      mpfr_set_q(sc->mpfr_2, big_ratio(x), MPFR_RNDN);
      goto ATANH_BIG_REAL;

    case T_BIG_REAL:
      mpfr_set(sc->mpfr_2, big_real(x), MPFR_RNDN);
    ATANH_BIG_REAL:
      mpfr_set_ui(sc->mpfr_1, 1, MPFR_RNDN);
      if (mpfr_cmpabs(sc->mpfr_2, sc->mpfr_1) < 0)
	{
	  mpfr_atanh(sc->mpfr_2, sc->mpfr_2, MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_2));
	}
      mpc_set_fr(sc->mpc_1, sc->mpfr_2, MPC_RNDNN);
      mpc_atanh(sc->mpc_1, sc->mpc_1, MPC_RNDNN);
      return(mpc_to_number(sc, sc->mpc_1));

    case T_BIG_COMPLEX:
      mpc_atanh(sc->mpc_1, big_complex(x), MPC_RNDNN);
      return(mpc_to_number(sc, sc->mpc_1));
#endif
    default:
      return(method_or_bust_with_type_one_arg_p(sc, x, sc->atanh_symbol, a_number_string));
    }
}

