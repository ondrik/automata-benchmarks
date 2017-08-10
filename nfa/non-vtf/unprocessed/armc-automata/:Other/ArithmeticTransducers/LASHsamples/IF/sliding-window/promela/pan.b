	switch (t->back) {
	default: Uerror("bad return move");
	case  0: goto R999; /* nothing to undo */

		 /* PROC :init: */
	case  1: /* STATE 1 */
		;
		delproc(0, now._nr_pr-1);
		goto R999;
	case  2: /* STATE 2 */
		;
		delproc(0, now._nr_pr-1);
		goto R999;
	case  3: /* STATE 4 */
		p_restor(II);
		goto R999;

		 /* PROC receiver */
	case  4: /* STATE 1 */
		((P1 *)this)->v = trpt->oval;
		goto R999;
	case  6: /* STATE 2 */
		;
		if (1)
#if defined(FULLSTACK) && defined(NOCOMP) && !defined(BITSTATE) && !defined(MA)
#if VECTORSZ<=1024
			sv_restor(!(t->atom&2));
#else
			p_q_restor(II, ((P1 *)this)->In);
#endif
#else
#if VECTORSZ<=1024
			sv_restor(0);
#else
			p_q_restor(II, ((P1 *)this)->In);
#endif
#endif
		;
		goto R999;
	case  8: /* STATE 4 */
		((P1 *)this)->v = trpt->oval;
		goto R999;
	case  13: /* STATE 10 */
		if (m == 2) m = unsend(((P1 *)this)->Out);
		goto R999;
	case  18: /* STATE 18 */
		p_restor(II);
		goto R999;

		 /* PROC sender */
	case  19: /* STATE 1 */
		((P0 *)this)->ns = trpt->oval;
		goto R999;
	case  20: /* STATE 2 */
		((P0 *)this)->na = trpt->oval;
		goto R999;
	case  23: /* STATE 4 */
		((P0 *)this)->ns = trpt->oval;
		goto R999;
	case  24: /* STATE 5 */
		if (m == 2) m = unsend(((P0 *)this)->Out);
		goto R999;
	case  25: /* STATE 6 */
		;
		if (1)
#if defined(FULLSTACK) && defined(NOCOMP) && !defined(BITSTATE) && !defined(MA)
#if VECTORSZ<=1024
			sv_restor(!(t->atom&2));
#else
			p_q_restor(II, ((P0 *)this)->In);
#endif
#else
#if VECTORSZ<=1024
			sv_restor(0);
#else
			p_q_restor(II, ((P0 *)this)->In);
#endif
#endif
		;
		goto R999;
	case  27: /* STATE 8 */
		((P0 *)this)->na = trpt->oval;
		goto R999;
	case  28: /* STATE 9 */
		((P0 *)this)->ns = trpt->oval;
		goto R999;
	case  33: /* STATE 17 */
		p_restor(II);
		goto R999;
	}

