static s7_pointer g_acosh(s7_scheme *sc, s7_pointer args)
{
  #define H_acosh "(acosh z) returns acosh(z)"
  #define Q_acosh sc->pl_nn

  s7_pointer x;
  x = car(args);
  switch (type(x))
    {
    case T_INTEGER:
      if (integer(x) == 1) return(int_zero);

    case T_REAL:
    case T_RATIO:
      {
	double x1;
	x1 = s7_real(x);
	if (x1 >= 1.0)
	  return(make_real(sc, acosh(x1)));
      }

    case T_COMPLEX:
#if HAVE_COMPLEX_NUMBERS
  #ifdef __OpenBSD__
      return(c_complex_to_s7(sc, cacosh_1(s7_to_c_complex(x))));
  #else
      return(c_complex_to_s7(sc, cacosh(s7_to_c_complex(x)))); /* not to_c_complex because x might not be complex */
  #endif
#else
      /* since we can fall through to this branch, we need a better error message than "must be a number, not 0.0" */
      return(out_of_range(sc, sc->acosh_symbol, int_one, x, no_complex_numbers_string));
#endif

#if WITH_GMP
    case T_BIG_INTEGER:
      mpc_set_z(sc->mpc_1, big_integer(x), MPC_RNDNN);
      mpc_acosh(sc->mpc_1, sc->mpc_1, MPC_RNDNN);
      return(mpc_to_number(sc, sc->mpc_1));

    case T_BIG_RATIO:
      mpc_set_q(sc->mpc_1, big_ratio(x), MPC_RNDNN);
      mpc_acosh(sc->mpc_1, sc->mpc_1, MPC_RNDNN);
      return(mpc_to_number(sc, sc->mpc_1));

    case T_BIG_REAL:
      mpc_set_fr(sc->mpc_1, big_real(x), MPC_RNDNN);
      mpc_acosh(sc->mpc_1, sc->mpc_1, MPC_RNDNN);
      return(mpc_to_number(sc, sc->mpc_1));

    case T_BIG_COMPLEX:
      mpc_acosh(sc->mpc_1, big_complex(x), MPC_RNDNN);
      return(mpc_to_number(sc, sc->mpc_1));
#endif

    default:
      return(method_or_bust_with_type_one_arg_p(sc, x, sc->acosh_symbol, a_number_string));
    }
}

