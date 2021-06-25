#define SIN_LIMIT 1.0e16
#define SINH_LIMIT 20.0
/* (- (sinh (bignum 30.0)) (sinh 30.0)): -3.718172657214174140191915872003397016115E-4
 * (- (sinh (bignum 20.0)) (sinh 20.0)): -7.865629467297586346406367346575835463792E-10, slightly worse (e-8) if imag-part
 */

static s7_pointer sin_p_p(s7_scheme *sc, s7_pointer x)
{
#if (!WITH_GMP)
  if (is_t_real(x)) return(make_real(sc, sin(real(x)))); /* range check in gmp case */
#endif
  switch (type(x))
    {
    case T_INTEGER:
      if (integer(x) == 0) return(int_zero);           /* (sin 0) -> 0 */
#if WITH_GMP
      if (integer(x) > SIN_LIMIT)
	{
	  mpz_set_si(sc->mpz_1, integer(x));
	  mpfr_set_z(sc->mpfr_1, sc->mpz_1, MPFR_RNDN);
	  mpfr_sin(sc->mpfr_1, sc->mpfr_1, MPFR_RNDN);
	  return(mpfr_to_big_real(sc, sc->mpfr_1));
	}
#endif
      return(make_real(sc, sin((s7_double)(integer(x)))));

    case T_RATIO:
      return(make_real(sc, sin((s7_double)(fraction(x)))));

    case T_REAL:
      {
	s7_double y;
	y = real(x);
#if WITH_GMP
	if (fabs(y) > SIN_LIMIT)
	  {
	    mpfr_set_d(sc->mpfr_1, y, MPFR_RNDN);
	    mpfr_sin(sc->mpfr_1, sc->mpfr_1, MPFR_RNDN);
	    return(mpfr_to_big_real(sc, sc->mpfr_1));
	  }
#endif
	return(make_real(sc, sin(y)));
      }

    case T_COMPLEX:
#if WITH_GMP
      if ((fabs(real_part(x)) > SIN_LIMIT) || (fabs(imag_part(x)) > SINH_LIMIT))
	{
	  mpc_set_d_d(sc->mpc_1, real_part(x), imag_part(x), MPC_RNDNN);
	  mpc_sin(sc->mpc_1, sc->mpc_1, MPC_RNDNN);
	  return(mpc_to_number(sc, sc->mpc_1));
	}
#endif
#if HAVE_COMPLEX_NUMBERS
      return(c_complex_to_s7(sc, csin(to_c_complex(x))));
#else
      return(out_of_range(sc, sc->sin_symbol, int_one, x, no_complex_numbers_string));
#endif

#if WITH_GMP
    case T_BIG_INTEGER:
      mpfr_set_z(sc->mpfr_1, big_integer(x), MPFR_RNDN);
      mpfr_sin(sc->mpfr_1, sc->mpfr_1, MPFR_RNDN);
      return(mpfr_to_big_real(sc, sc->mpfr_1));

    case T_BIG_RATIO:
      mpfr_set_q(sc->mpfr_1, big_ratio(x), MPFR_RNDN);
      mpfr_sin(sc->mpfr_1, sc->mpfr_1, MPFR_RNDN);
      return(mpfr_to_big_real(sc, sc->mpfr_1));

    case T_BIG_REAL:
      mpfr_sin(sc->mpfr_1, big_real(x), MPFR_RNDN);
      return(mpfr_to_big_real(sc, sc->mpfr_1));

    case T_BIG_COMPLEX:
      mpc_sin(sc->mpc_1, big_complex(x), MPC_RNDNN);
      if (mpfr_zero_p(mpc_imagref(sc->mpc_1)))
	return(mpfr_to_big_real(sc, mpc_realref(sc->mpc_1)));
      return(mpc_to_number(sc, sc->mpc_1));
#endif
    default:
      return(method_or_bust_with_type_one_arg_p(sc, x, sc->sin_symbol, a_number_string));
    }
  /* sin is inaccurate over about 1e30.  There's a way to get true results, but it involves fancy "range reduction" techniques.
   * (sin 1e32): 0.5852334864823946
   *   but it should be 3.901970254333630491697613212893425767786E-1
   * (remainder 1e22 (* 2 pi)) -> 1.0057952155665e+22 !! (it's now a range error)
   *   it should be 5.263007914620499494429139986095833592117E0
   * before comparing imag-part to 0, we need to look for NaN and inf, else:
   *    (sinh 0+0/0i) -> 0.0 and (sinh (log 0.0)) -> inf.0
   */
}

static s7_pointer g_sin(s7_scheme *sc, s7_pointer args)
{
  #define H_sin "(sin z) returns sin(z)"
  #define Q_sin sc->pl_nn
  return(sin_p_p(sc, car(args)));
}

#if WITH_GMP
static s7_pointer sin_p_d(s7_scheme *sc, s7_double x)
{
  if (fabs(x) > SIN_LIMIT)
    {
      mpfr_set_d(sc->mpfr_1, x, MPFR_RNDN);
      mpfr_sin(sc->mpfr_1, sc->mpfr_1, MPFR_RNDN);
      return(mpfr_to_big_real(sc, sc->mpfr_1));
    }
  return(make_real(sc, sin(x)));
}
#else
static s7_double sin_d_d(s7_double x) {return(sin(x));}
static s7_pointer sin_p_d(s7_scheme *sc, s7_double x) {return(make_real(sc, sin(x)));}
#endif

