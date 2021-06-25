#define TANH_LIMIT 350.0
static s7_pointer g_tanh(s7_scheme *sc, s7_pointer args)
{
  #define H_tanh "(tanh z) returns tanh(z)"
  #define Q_tanh sc->pl_nn

  s7_pointer x;
  x = car(args);
  switch (type(x))
    {
    case T_INTEGER:
      if (integer(x) == 0) return(int_zero);  /* (tanh 0) -> 0 */

    case T_REAL:
    case T_RATIO:
      return(make_real(sc, tanh(s7_real(x))));

    case T_COMPLEX:
#if HAVE_COMPLEX_NUMBERS
      if (real_part(x) > TANH_LIMIT)
	return(real_one);                         /* closer than 0.0 which is what ctanh is about to return! */
      if (real_part(x) < -TANH_LIMIT)
	return(make_real(sc, -1.0));              /* closer than ctanh's -0.0 */
      return(c_complex_to_s7(sc, ctanh(to_c_complex(x))));
#else
      return(out_of_range(sc, sc->tanh_symbol, int_one, x, no_complex_numbers_string));
#endif

#if WITH_GMP
    case T_BIG_INTEGER:
      mpfr_set_z(sc->mpfr_1, big_integer(x), MPFR_RNDN);
      goto BIG_REAL_TANH;

    case T_BIG_RATIO:
      mpfr_set_q(sc->mpfr_1, big_ratio(x), MPFR_RNDN);
      goto BIG_REAL_TANH;

    case T_BIG_REAL:
      if (mpfr_nan_p(big_real(x))) return(real_NaN);
      mpfr_set(sc->mpfr_1, big_real(x), MPFR_RNDN);

    BIG_REAL_TANH:
      if (mpfr_cmp_d(sc->mpfr_1, TANH_LIMIT) > 0) return(real_one);
      if (mpfr_cmp_d(sc->mpfr_1, -TANH_LIMIT) < 0) return(make_real(sc, -1.0));
      mpfr_tanh(sc->mpfr_1, sc->mpfr_1, MPFR_RNDN);
      return(mpfr_to_big_real(sc, sc->mpfr_1));

    case T_BIG_COMPLEX:
      if ((MPC_INEX_RE(mpc_cmp_si_si(big_complex(x), TANH_LIMIT, 1))) > 0)
	return(real_one);
      if ((MPC_INEX_RE(mpc_cmp_si_si(big_complex(x), -TANH_LIMIT, 1))) < 0)
	return(make_real(sc, -1.0));

      if ((mpfr_nan_p(mpc_imagref(big_complex(x)))) ||
	  (mpfr_inf_p(mpc_imagref(big_complex(x)))))
	{
	  if (mpfr_cmp_ui(mpc_realref(big_complex(x)), 0) == 0)
	    return(make_complex_unchecked(sc, 0.0, NAN)); /* match non-bignum choice */
	  return(complex_NaN);
	}

      mpc_tanh(sc->mpc_1, big_complex(x), MPC_RNDNN);
      if (mpfr_zero_p(mpc_imagref(sc->mpc_1)))
	return(mpfr_to_big_real(sc, mpc_realref(sc->mpc_1)));
      return(mpc_to_number(sc, sc->mpc_1));
#endif
    default:
      return(method_or_bust_with_type_one_arg(sc, x, sc->tanh_symbol, args, a_number_string));
    }
}

static s7_double tanh_d_d(s7_double x) {return(tanh(x));}

