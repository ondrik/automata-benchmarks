	switch (t->forw) {
	default: Uerror("bad forward move");

		 /* PROC :init: */
	case  1: /* STATE 1 - line 58 "sliding.spl" - [(run sender(q2,q1))] */
		IfNotBlocked
		if (!(addproc(0, ((P2 *)this)->q2, ((P2 *)this)->q1)))
			continue;
		m = 3; goto P999;
	case  2: /* STATE 2 - line 58 "sliding.spl" - [(run receiver(q1,q2))] */
		IfNotBlocked
		if (!(addproc(1, ((P2 *)this)->q1, ((P2 *)this)->q2)))
			continue;
		m = 3; goto P999;
	case  3: /* STATE 4 - line 61 "sliding.spl" - [-end-] */
		IfNotBlocked
		if (!delproc(1, II)) continue;
		m = 3; goto P999;

		 /* PROC receiver */
	case  4: /* STATE 1 - line 34 "sliding.spl" - [v = 0] */
		IfNotBlocked
		(trpt+1)->oval = ((P1 *)this)->v;
		((P1 *)this)->v = 0;
#ifdef VAR_RANGES
		logval("receiver:v", ((P1 *)this)->v);
#endif
		;
		m = 3; goto P999;
	case  5: /* STATE 16 - line 52 "sliding.spl" - [.(goto)] */
		IfNotBlocked
		;
		m = 3; goto P999;
	case  6: /* STATE 2 - line 39 "sliding.spl" - [In?y] */
		if (q_len(((P1 *)this)->In) == 0) continue;
	{
		int XX=1;
#if defined(FULLSTACK) && defined(NOCOMP) && !defined(BITSTATE) && !defined(MA)
		if (t->atom&2)
#endif
#if VECTORSZ<=1024
			sv_save((char *)&now);
#else
			p_q_save(II, ((P1 *)this)->In);
#endif
		;
		((P1 *)this)->y = qrecv(((P1 *)this)->In, XX-1, 0, 1);
#ifdef VAR_RANGES
		logval("receiver:y", ((P1 *)this)->y);
#endif
		;
	}
		;
		m = 4; goto P999;
	case  7: /* STATE 3 - line 41 "sliding.spl" - [((y==(v+1)))] */
		IfNotBlocked
		if (!((((P1 *)this)->y==(((P1 *)this)->v+1))))
			continue;
		m = 3; goto P999;
	case  8: /* STATE 4 - line 41 "sliding.spl" - [v = y] */
		IfNotBlocked
		(trpt+1)->oval = ((P1 *)this)->v;
		((P1 *)this)->v = ((P1 *)this)->y;
#ifdef VAR_RANGES
		logval("receiver:v", ((P1 *)this)->v);
#endif
		;
		m = 3; goto P999;
	case  9: /* STATE 8 - line 44 "sliding.spl" - [.(goto)] */
		IfNotBlocked
		;
		m = 3; goto P999;
	case  10: /* STATE 5 - line 42 "sliding.spl" - [((y!=(v+1)))] */
		IfNotBlocked
		if (!((((P1 *)this)->y!=(((P1 *)this)->v+1))))
			continue;
		m = 3; goto P999;
	case  11: /* STATE 6 - line 42 "sliding.spl" - [(1)] */
		IfNotBlocked
		if (!(1))
			continue;
		m = 3; goto P999;
	case  12: /* STATE 9 - line 44 "sliding.spl" - [((v>-(1)))] */
		IfNotBlocked
		if (!((((P1 *)this)->v> -(1))))
			continue;
		m = 3; goto P999;
	case  13: /* STATE 10 - line 46 "sliding.spl" - [Out!v] */
		IfNotBlocked
		if (q_full(((P1 *)this)->Out))
		{ nlost++; m=3; goto P999; }

		qsend(((P1 *)this)->Out, 0, ((P1 *)this)->v);
		m = 2; goto P999;
	case  14: /* STATE 11 - line 46 "sliding.spl" - [(1)] */
		IfNotBlocked
		if (!(1))
			continue;
		m = 3; goto P999;
	case  15: /* STATE 14 - line 50 "sliding.spl" - [.(goto)] */
		IfNotBlocked
		;
		m = 3; goto P999;
	case  16: /* STATE 12 - line 47 "sliding.spl" - [(1)] */
		IfNotBlocked
		if (!(1))
			continue;
		m = 3; goto P999;
	case  17: /* STATE 17 - line 37 "sliding.spl" - [break] */
		IfNotBlocked
		;
		m = 3; goto P999;
	case  18: /* STATE 18 - line 52 "sliding.spl" - [-end-] */
		IfNotBlocked
		if (!delproc(1, II)) continue;
		m = 3; goto P999;

		 /* PROC sender */
	case  19: /* STATE 1 - line 10 "sliding.spl" - [ns = 0] */
		IfNotBlocked
		(trpt+1)->oval = ((P0 *)this)->ns;
		((P0 *)this)->ns = 0;
#ifdef VAR_RANGES
		logval("sender:ns", ((P0 *)this)->ns);
#endif
		;
		m = 3; goto P999;
	case  20: /* STATE 2 - line 11 "sliding.spl" - [na = 0] */
		IfNotBlocked
		(trpt+1)->oval = ((P0 *)this)->na;
		((P0 *)this)->na = 0;
#ifdef VAR_RANGES
		logval("sender:na", ((P0 *)this)->na);
#endif
		;
		m = 3; goto P999;
	case  21: /* STATE 15 - line 27 "sliding.spl" - [.(goto)] */
		IfNotBlocked
		;
		m = 3; goto P999;
	case  22: /* STATE 3 - line 17 "sliding.spl" - [(((ns<(na+2))&&(ns<=100)))] */
		IfNotBlocked
		if (!(((((P0 *)this)->ns<(((P0 *)this)->na+2))&&(((P0 *)this)->ns<=100))))
			continue;
		m = 3; goto P999;
	case  23: /* STATE 4 - line 17 "sliding.spl" - [ns = (ns+1)] */
		IfNotBlocked
		(trpt+1)->oval = ((P0 *)this)->ns;
		((P0 *)this)->ns = (((P0 *)this)->ns+1);
#ifdef VAR_RANGES
		logval("sender:ns", ((P0 *)this)->ns);
#endif
		;
		m = 3; goto P999;
	case  24: /* STATE 5 - line 17 "sliding.spl" - [Out!ns] */
		IfNotBlocked
		if (q_full(((P0 *)this)->Out))
		{ nlost++; m=3; goto P999; }

		qsend(((P0 *)this)->Out, 0, ((P0 *)this)->ns);
		m = 2; goto P999;
	case  25: /* STATE 6 - line 19 "sliding.spl" - [In?x] */
		if (q_len(((P0 *)this)->In) == 0) continue;
	{
		int XX=1;
#if defined(FULLSTACK) && defined(NOCOMP) && !defined(BITSTATE) && !defined(MA)
		if (t->atom&2)
#endif
#if VECTORSZ<=1024
			sv_save((char *)&now);
#else
			p_q_save(II, ((P0 *)this)->In);
#endif
		;
		((P0 *)this)->x = qrecv(((P0 *)this)->In, XX-1, 0, 1);
#ifdef VAR_RANGES
		logval("sender:x", ((P0 *)this)->x);
#endif
		;
	}
		;
		m = 4; goto P999;
	case  26: /* STATE 7 - line 21 "sliding.spl" - [((x>na))] */
		IfNotBlocked
		if (!((((P0 *)this)->x>((P0 *)this)->na)))
			continue;
		m = 3; goto P999;
	case  27: /* STATE 8 - line 21 "sliding.spl" - [na = x] */
		IfNotBlocked
		(trpt+1)->oval = ((P0 *)this)->na;
		((P0 *)this)->na = ((P0 *)this)->x;
#ifdef VAR_RANGES
		logval("sender:na", ((P0 *)this)->na);
#endif
		;
		m = 3; goto P999;
	case  28: /* STATE 9 - line 21 "sliding.spl" - [ns = na] */
		IfNotBlocked
		(trpt+1)->oval = ((P0 *)this)->ns;
		((P0 *)this)->ns = ((P0 *)this)->na;
#ifdef VAR_RANGES
		logval("sender:ns", ((P0 *)this)->ns);
#endif
		;
		m = 3; goto P999;
	case  29: /* STATE 13 - line 25 "sliding.spl" - [.(goto)] */
		IfNotBlocked
		;
		m = 3; goto P999;
	case  30: /* STATE 10 - line 22 "sliding.spl" - [((x<=na))] */
		IfNotBlocked
		if (!((((P0 *)this)->x<=((P0 *)this)->na)))
			continue;
		m = 3; goto P999;
	case  31: /* STATE 11 - line 22 "sliding.spl" - [(1)] */
		IfNotBlocked
		if (!(1))
			continue;
		m = 3; goto P999;
	case  32: /* STATE 16 - line 15 "sliding.spl" - [break] */
		IfNotBlocked
		;
		m = 3; goto P999;
	case  33: /* STATE 17 - line 27 "sliding.spl" - [-end-] */
		IfNotBlocked
		if (!delproc(1, II)) continue;
		m = 3; goto P999;
	case  _T5:	/* np_ */
		if (!((!(trpt->o_pm&4) && !(trpt->tau&128))))
			continue;
		/* else fall through */
	case  _T2:	/* true */
		m = 3; goto P999;
	}

