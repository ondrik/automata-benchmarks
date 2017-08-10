#ifdef PEG
struct T_SRC {
	char *fl; int ln;
} T_SRC[NTRANS];

void
tr_2_src(int m, char *file, int ln)
{	T_SRC[m].fl = file;
	T_SRC[m].ln = ln;
}

void
putpeg(int n, int m)
{	printf("%5d	trans %4d ", m, n);
	printf("file %s line %3d\n",
		T_SRC[n].fl, T_SRC[n].ln);
}
#else
#define tr_2_src(m,f,l)
#endif

void
settable(void)
{	Trans *T;
	Trans *settr(int, int, int, int, int, char *, int, int, int);

	trans = (Trans ***) emalloc(4*sizeof(Trans **));

	/* proctype 2: :init: */

	trans[2] = (Trans **) emalloc(5*sizeof(Trans *));

	T = trans[ 2][3] = settr(37,2,0,0,0,"ATOMIC", 0, 2, 0);
		/* "sliding.spl":58 */
	T->nxt	= settr(37,2,1,0,0,"ATOMIC", 0, 2, 0);
		/* "sliding.spl":58 */
		tr_2_src(1, "sliding.spl", 58);
	trans[2][1]	= settr(35,2,2,1,1,"(run sender(q2,q1))", 0, 2, 0);
		tr_2_src(2, "sliding.spl", 58);
	trans[2][2]	= settr(36,0,4,2,2,"(run receiver(q1,q2))", 0, 2, 0);
		tr_2_src(3, "sliding.spl", 61);
	trans[2][4]	= settr(38,0,0,3,3,"-end-", 0, 3500, 0);

	/* proctype 1: receiver */

	trans[1] = (Trans **) emalloc(19*sizeof(Trans *));

		tr_2_src(4, "sliding.spl", 34);
	trans[1][1]	= settr(17,0,15,4,4,"v = 0", 0, 2, 0);
		tr_2_src(5, "sliding.spl", 52);
	trans[1][16]	= settr(32,0,15,5,0,".(goto)", 0, 2, 0);
	T = trans[1][15] = settr(31,0,0,0,0,"DO", 0, 2, 0);
		/* "sliding.spl":37 */
	T = T->nxt	= settr(31,0,2,0,0,"DO", 0, 2, 0);
		/* "sliding.spl":37 */
	    T->nxt	= settr(31,0,9,0,0,"DO", 0, 2, 0);
		/* "sliding.spl":37 */
		tr_2_src(6, "sliding.spl", 39);
	trans[1][2]	= settr(18,0,7,6,6,"In?y", 1, 505, 0);
	T = trans[1][7] = settr(23,0,0,0,0,"IF", 0, 2, 0);
		/* "sliding.spl":40 */
	T = T->nxt	= settr(23,0,3,0,0,"IF", 0, 2, 0);
		/* "sliding.spl":40 */
	    T->nxt	= settr(23,0,5,0,0,"IF", 0, 2, 0);
		/* "sliding.spl":40 */
		tr_2_src(7, "sliding.spl", 41);
	trans[1][3]	= settr(19,0,4,7,0,"((y==(v+1)))", 0, 2, 0);
		tr_2_src(8, "sliding.spl", 41);
	trans[1][4]	= settr(20,0,15,8,8,"v = y", 0, 2, 0);
		tr_2_src(9, "sliding.spl", 44);
	trans[1][8]	= settr(24,0,15,9,0,".(goto)", 0, 2, 0);
		tr_2_src(10, "sliding.spl", 42);
	trans[1][5]	= settr(21,0,6,10,0,"((y!=(v+1)))", 0, 2, 0);
		tr_2_src(11, "sliding.spl", 42);
	trans[1][6]	= settr(22,0,15,11,0,"(1)", 0, 2, 0);
		tr_2_src(12, "sliding.spl", 44);
	trans[1][9]	= settr(25,0,13,12,0,"((v>-(1)))", 0, 2, 0);
	T = trans[1][13] = settr(29,0,0,0,0,"IF", 0, 2, 0);
		/* "sliding.spl":45 */
	T = T->nxt	= settr(29,0,10,0,0,"IF", 0, 2, 0);
		/* "sliding.spl":45 */
	    T->nxt	= settr(29,0,12,0,0,"IF", 0, 2, 0);
		/* "sliding.spl":45 */
		tr_2_src(13, "sliding.spl", 46);
	trans[1][10]	= settr(26,0,11,13,13,"Out!v", 1, 6, 0);
		tr_2_src(14, "sliding.spl", 46);
	trans[1][11]	= settr(27,0,15,14,0,"(1)", 0, 2, 0);
		tr_2_src(15, "sliding.spl", 50);
	trans[1][14]	= settr(30,0,15,15,0,".(goto)", 0, 2, 0);
		tr_2_src(16, "sliding.spl", 47);
	trans[1][12]	= settr(28,0,15,16,0,"(1)", 0, 2, 0);
		tr_2_src(17, "sliding.spl", 37);
	trans[1][17]	= settr(33,0,18,17,0,"break", 0, 2, 0);
		tr_2_src(18, "sliding.spl", 52);
	trans[1][18]	= settr(34,0,0,18,18,"-end-", 0, 3500, 0);

	/* proctype 0: sender */

	trans[0] = (Trans **) emalloc(18*sizeof(Trans *));

		tr_2_src(19, "sliding.spl", 10);
	trans[0][1]	= settr(0,0,2,19,19,"ns = 0", 0, 2, 0);
		tr_2_src(20, "sliding.spl", 11);
	trans[0][2]	= settr(1,0,14,20,20,"na = 0", 0, 2, 0);
		tr_2_src(21, "sliding.spl", 27);
	trans[0][15]	= settr(14,0,14,21,0,".(goto)", 0, 2, 0);
	T = trans[0][14] = settr(13,0,0,0,0,"DO", 0, 2, 0);
		/* "sliding.spl":15 */
	T = T->nxt	= settr(13,0,3,0,0,"DO", 0, 2, 0);
		/* "sliding.spl":15 */
	    T->nxt	= settr(13,0,6,0,0,"DO", 0, 2, 0);
		/* "sliding.spl":15 */
		tr_2_src(22, "sliding.spl", 17);
	trans[0][3]	= settr(2,0,4,22,0,"(((ns<(na+2))&&(ns<=100)))", 0, 2, 0);
		tr_2_src(23, "sliding.spl", 17);
	trans[0][4]	= settr(3,0,5,23,23,"ns = (ns+1)", 0, 2, 0);
		tr_2_src(24, "sliding.spl", 17);
	trans[0][5]	= settr(4,0,14,24,24,"Out!ns", 1, 4, 0);
		tr_2_src(25, "sliding.spl", 19);
	trans[0][6]	= settr(5,0,12,25,25,"In?x", 1, 503, 0);
	T = trans[0][12] = settr(11,0,0,0,0,"IF", 0, 2, 0);
		/* "sliding.spl":20 */
	T = T->nxt	= settr(11,0,7,0,0,"IF", 0, 2, 0);
		/* "sliding.spl":20 */
	    T->nxt	= settr(11,0,10,0,0,"IF", 0, 2, 0);
		/* "sliding.spl":20 */
		tr_2_src(26, "sliding.spl", 21);
	trans[0][7]	= settr(6,0,8,26,0,"((x>na))", 0, 2, 0);
		tr_2_src(27, "sliding.spl", 21);
	trans[0][8]	= settr(7,0,9,27,27,"na = x", 0, 2, 0);
		tr_2_src(28, "sliding.spl", 21);
	trans[0][9]	= settr(8,0,14,28,28,"ns = na", 0, 2, 0);
		tr_2_src(29, "sliding.spl", 25);
	trans[0][13]	= settr(12,0,14,29,0,".(goto)", 0, 2, 0);
		tr_2_src(30, "sliding.spl", 22);
	trans[0][10]	= settr(9,0,11,30,0,"((x<=na))", 0, 2, 0);
		tr_2_src(31, "sliding.spl", 22);
	trans[0][11]	= settr(10,0,14,31,0,"(1)", 0, 2, 0);
		tr_2_src(32, "sliding.spl", 15);
	trans[0][16]	= settr(15,0,17,32,0,"break", 0, 2, 0);
		tr_2_src(33, "sliding.spl", 27);
	trans[0][17]	= settr(16,0,0,33,33,"-end-", 0, 3500, 0);
	/* np_ demon: */
	trans[_NP_] = (Trans **) emalloc(2*sizeof(Trans *));
	T = trans[_NP_][0] = settr(9997,0,0,_T2,0,"(1)",   0,2,0);
	    T->nxt	  = settr(9998,0,1,_T5,0,"(np_)", 1,2,0);
	T = trans[_NP_][1] = settr(9999,0,1,_T5,0,"(np_)", 1,2,0);
}

Trans *
settr(	int t_id, int a, int b, int c, int d,
	char *t, int g, int tpe0, int tpe1)
{	Trans *tmp = (Trans *) emalloc(sizeof(Trans));

	tmp->atom  = a&(6|32);	/* only (2|8|32) have meaning */
	if (!g) tmp->atom |= 8;	/* no global references */
	tmp->st    = b;
	tmp->tpe[0] = tpe0;
	tmp->tpe[1] = tpe1;
	tmp->tp    = t;
	tmp->t_id  = t_id;
	tmp->forw  = c;
	tmp->back  = d;
	return tmp;
}

Trans *
cpytr(Trans *a)
{	Trans *tmp = (Trans *) emalloc(sizeof(Trans));

	int i;
	tmp->atom  = a->atom;
	tmp->st    = a->st;
#ifdef HAS_UNLESS
	tmp->e_trans = a->e_trans;
	for (i = 0; i < HAS_UNLESS; i++)
		tmp->escp[i] = a->escp[i];
#endif
	tmp->tpe[0] = a->tpe[0];
	tmp->tpe[1] = a->tpe[1];
	for (i = 0; i < 6; i++)
	{	tmp->qu[i] = a->qu[i];
		tmp->ty[i] = a->ty[i];
	}
	tmp->tp    = (char *) emalloc(strlen(a->tp)+1);
	strcpy(tmp->tp, a->tp);
	tmp->t_id  = a->t_id;
	tmp->forw  = a->forw;
	tmp->back  = a->back;
	return tmp;
}

#ifndef NOREDUCE
int
srinc_set(int n)
{	if (n <= 2) return LOCAL;
	if (n <= 2+  DELTA) return Q_FULL_F; /* 's' or nfull  */
	if (n <= 2+2*DELTA) return Q_EMPT_F; /* 'r' or nempty */
	if (n <= 2+3*DELTA) return Q_EMPT_T; /* empty */
	if (n <= 2+4*DELTA) return Q_FULL_T; /* full  */
	if (n ==   5*DELTA) return GLOBAL;
	if (n ==   6*DELTA) return TIMEOUT_F;
	if (n ==   7*DELTA) return ALPHA_F;
	Uerror("cannot happen srinc_class");
	return BAD;
}
int
srunc(int n, int m)
{	switch(m) {
	case Q_FULL_F: return n-2;
	case Q_EMPT_F: return n-2-DELTA;
	case Q_EMPT_T: return n-2-2*DELTA;
	case Q_FULL_T: return n-2-3*DELTA;
	case ALPHA_F:
	case TIMEOUT_F: return 257; /* non-zero, and > MAXQ */
	}
	Uerror("cannot happen srunc");
	return 0;
}
#endif
int cnt;

void
retrans(int n, int m, int is, short srcln[], uchar reach[])
	/* process n, with m states, is=initial state */
{	Trans *T0, *T1, *T2, *T3;
	int i, j, k, p, h, g, aa;
	if (state_tables >= 4)
	{	printf("STEP 1 proctype %s\n", 
			procname[n]);
		for (i = 1; i < m; i++)
		for (T0 = trans[n][i]; T0; T0 = T0->nxt)
			crack(n, i, T0, srcln);
		return;
	}
	do {
		for (i = 1, cnt = 0; i < m; i++)
		{	T2 = trans[n][i];
			T1 = T2?T2->nxt:(Trans *)0;
/* prescan: */		for (T0 = T1; T0; T0 = T0->nxt)
/* choice in choice */	{	if (T0->st
				&&  trans[n][T0->st]->nxt)
					break;
			}
#if 0
		if (T0)
		printf("\tstate %d / %d: choice in choice\n",
		i, T0->st);
#endif
			if (T0)
			for (T0 = T1; T0; T0 = T0->nxt)
			{	T3 = trans[n][T0->st];
				if (!T3->nxt)
				{	T2->nxt = cpytr(T0);
					T2 = T2->nxt;
					imed(T2, T0->st, n);
					continue;
				}
				do {	T3 = T3->nxt;
					T2->nxt = cpytr(T3);
					T2 = T2->nxt;
					imed(T2, T0->st, n);
				} while (T3->nxt);
				cnt++;
			}
		}
	} while (cnt);
	if (state_tables >= 3)
	{	printf("STEP 2 proctype %s\n", 
			procname[n]);
		for (i = 1; i < m; i++)
		for (T0 = trans[n][i]; T0; T0 = T0->nxt)
			crack(n, i, T0, srcln);
		return;
	}
	for (i = 1; i < m; i++)
	{	if (trans[n][i] && trans[n][i]->nxt) /* optimize */
		{	T1 = trans[n][i]->nxt;
#if 0
			printf("\t\tpull %d (%d) to %d\n",
			T1->st, T1->forw, i);
#endif
			T0 = cpytr(trans[n][T1->st]);
			trans[n][i] = T0;
			reach[T1->st] = 1;
			imed(T0, T1->st, n);
			for (T1 = T1->nxt; T1; T1 = T1->nxt)
			{
#if 0
			printf("\t\tpull %d (%d) to %d\n",
				T1->st, T1->forw, i);
#endif
				T0->nxt = cpytr(trans[n][T1->st]);
				T0 = T0->nxt;
				reach[T1->st] = 1;
				imed(T0, T1->st, n);
	}	}	}
	if (state_tables >= 2)
	{	printf("STEP 3 proctype %s\n", 
			procname[n]);
		for (i = 1; i < m; i++)
		for (T0 = trans[n][i]; T0; T0 = T0->nxt)
			crack(n, i, T0, srcln);
		return;
	}
#ifdef HAS_UNLESS
	for (i = 1; i < m; i++)
	{	if (!trans[n][i]) continue;
		/* check for each state i if an
		 * escape to some state p is defined
		 * if so, copy and mark p's transitions
		 * and prepend them to the transition-
		 * list of state i
		 */
		for (T0 = trans[n][i]; T0; T0 = T0->nxt)
		for (k = HAS_UNLESS-1; k >= 0; k--)
		{	if (p = T0->escp[k])
			for (T1 = trans[n][p]; T1; T1 = T1->nxt)
			{	T2 = cpytr(T1);
				T2->e_trans = p;
				T2->nxt = trans[n][i];
				trans[n][i] = T2;
		}	}
	}
#endif
#ifndef NOREDUCE
	for (i = 1; i < m; i++)
	{
		if (a_cycles)
		{ /* moves through these states are visible */
#if PROG_LAB>0 && defined(HAS_NP)
			if (progstate[n][i])
				goto degrade;
			for (T1 = trans[n][i]; T1; T1 = T1->nxt)
				if (progstate[n][T1->st])
					goto degrade;
#endif
			if (accpstate[n][i] || visstate[n][i])
				goto degrade;
			for (T1 = trans[n][i]; T1; T1 = T1->nxt)
				if (accpstate[n][T1->st])
					goto degrade;
		}
		T1 = trans[n][i];
		if (!T1) continue;
		/* check if mixing of guards preserves reduction */
		if (T1->nxt)
		{	k = 0;
			for (T0 = T1; T0; T0 = T0->nxt)
			{	if (!(T0->atom&8))
					goto degrade;
				for (aa = 0; aa < 2; aa++)
				{	if (srinc_set(T0->tpe[aa])
					>=  GLOBAL)
						goto degrade;
					if (T0->tpe[aa]
					&&  T0->tpe[aa]
					!=  T1->tpe[0])
						k = 1;
			}	}
			g = 0;
			if (k)	/* non-uniform selection */
			for (T0 = T1; T0; T0 = T0->nxt)
			for (aa = 0; aa < 2; aa++)
			{	j = srinc_set(T0->tpe[aa]);
				if (j != LOCAL)
				{	k = srunc(T0->tpe[aa], j);
					for (h = 0; h < 6; h++)
						if (T1->qu[h] == k
						&&  T1->ty[h] == j)
							break;
					if (h >= 6)
					{	T1->qu[g%6] = k;
						T1->ty[g%6] = j;
						g++;
			}	}	}
			if (g > 6)
			{	T1->qu[0] = 0;	/* turn it off */
#if 1
				printf("line %d, ", srcln[i]);
			 	printf("too many types (%d)",
					g);
			  	printf(" in selection\n");
#endif
			  goto degrade;
			}
		}
		/* mark all options global if >=1 is global */
		for (T1 = trans[n][i]; T1; T1 = T1->nxt)
			if (!(T1->atom&8)) break;
		if (T1)
degrade:	for (T1 = trans[n][i]; T1; T1 = T1->nxt)
			T1->atom &= ~8;
		/* can only mix 'r's or 's's if on same chan */
		/* and not mixed with other local operations */
		T1 = trans[n][i]; j = T1->tpe[0];
		if (T1->qu[0]) continue;
		if (T1->nxt && T1->atom&8)
		{ if (j == 5*DELTA)
		  {
#if 1
			printf("warning: line %d ", srcln[i]);
			printf("mixed condition ");
			printf("(defeats reduction)\n");
#endif
			goto degrade;
		  }
		  for (T0 = T1; T0; T0 = T0->nxt)
		  for (aa = 0; aa < 2; aa++)
		  if  (T0->tpe[aa] && T0->tpe[aa] != j)
		  {
#if 1
			printf("warning: line %d ", srcln[i]);
			printf("[%d-%d] mixed %stion ",
				T0->tpe[aa], j, 
				(j==5*DELTA)?"condi":"selec");
			printf("(defeats reduction)\n");
			printf("	'%s' <-> '%s'\n",
				T1->tp, T0->tp);
#endif
			goto degrade;
		} }
	}
#endif
	if (state_tables)
	{	printf("proctype ");
		if (!strcmp(procname[n], ":init:"))
			printf("init\n");
		else
			printf("%s\n", procname[n]);
		for (i = 1; i < m; i++)
			reach[i] = 1;
		tagtable(n, m, is, srcln, reach);
	} else
	for (i = 1; i < m; i++)
	{	int nrelse;
		if (strcmp(procname[n], ":never:") != 0)
		for (T0 = trans[n][i]; T0; T0 = T0->nxt)
		{	if (T0->st == i
			&& strcmp(T0->tp, "(1)") == 0)
			{	printf("error: proctype '%s' ",
					procname[n]);
		  		printf("line %d, state %d: has un",
					srcln[i], i);
				printf("conditional self-loop\n");
				exit(1);
		}	}
		nrelse = 0;
		for (T0 = trans[n][i]; T0; T0 = T0->nxt)
		{	if (strcmp(T0->tp, "else") == 0)
				nrelse++;
		}
		if (nrelse > 1)
		{	printf("error: proctype '%s' state",
				procname[n]);
		  	printf(" %d, inherits %d", i, nrelse);
		  	printf(" 'else' stmnts\n");
			exit(1);
	}	}
}

void
imed(Trans *T, int v, int n)	/* set intermediate state */
{	progstate[n][T->st] |= progstate[n][v];
	accpstate[n][T->st] |= accpstate[n][v];
	stopstate[n][T->st] |= stopstate[n][v];
}

void
tagtable(int n, int m, int is, short srcln[], uchar reach[])
{	Trans *z;

	if (is >= m || !trans[n][is]
	||  is <= 0 || reach[is] == 0)
		return;
	reach[is] = 0;
	if (state_tables)
	for (z = trans[n][is]; z; z = z->nxt)
		crack(n, is, z, srcln);
	for (z = trans[n][is]; z; z = z->nxt)
	{	int i, j;
		tagtable(n, m, z->st, srcln, reach);
#ifdef HAS_UNLESS
		for (i = 0; i < HAS_UNLESS; i++)
		{	j = trans[n][is]->escp[i];
			if (!j) break;
			tagtable(n, m, j, srcln, reach);
		}
#endif
	}
}

void
crack(int n, int j, Trans *z, short srcln[])
{	int i;

	if (!z) return;
	printf("	state %3d -(tr %3d)-> state %3d  ",
		j, z->forw, z->st);
	printf("[id %3d tp %3d", z->t_id, z->tpe[0]);
	if (z->tpe[1]) printf(",%d", z->tpe[1]);
#ifdef HAS_UNLESS
	if (z->e_trans)
		printf(" org %3d", z->e_trans);
	else if (state_tables >= 2)
	for (i = 0; i < HAS_UNLESS; i++)
	{	if (!z->escp[i]) break;
		printf(" esc %d", z->escp[i]);
	}
#endif
	printf("]");
	printf(" [%s%s%s%s%s] line %d => ",
		z->atom&6?"A":z->atom&32?"D":"-",
		accpstate[n][j]?"a" :"-",
		stopstate[n][j]?"e" : "-",
		progstate[n][j]?"p" : "-",
		z->atom & 8 ?"L":"G",
		srcln[j]);
	for (i = 0; z->tp[i]; i++)
		if (z->tp[i] == '\n')
			printf("\\n");
		else
			putchar(z->tp[i]);
#if 0
	printf("\n");
#else
	if (z->qu[0])
	{	printf("\t[");
		for (i = 0; i < 6; i++)
			if (z->qu[i])
				printf("(%d,%d)",
				z->qu[i], z->ty[i]);
		printf("]");
	}
	printf("\n");
#endif
	fflush(stdout);
}

#ifdef VAR_RANGES
#define BYTESIZE	32	/* 2^8 : 2^3 = 256:8 = 32 */

typedef struct Vr_Ptr {
	char	*nm;
	unsigned char vals[BYTESIZE];
	struct Vr_Ptr *nxt;
} Vr_Ptr;
Vr_Ptr *ranges = (Vr_Ptr *) 0;

void
logval(char *s, int v)
{	Vr_Ptr *tmp;

	if (v<0 || v > 255) return;
	for (tmp = ranges; tmp; tmp = tmp->nxt)
		if (!strcmp(tmp->nm, s))
			goto found;
	tmp = (Vr_Ptr *) emalloc(sizeof(Vr_Ptr));
	tmp->nxt = ranges;
	ranges = tmp;
	tmp->nm = s;
found:
	tmp->vals[(v)/8] |= 1<<((v)%8);
}

void
dumpval(unsigned char X[], int range)
{	int w, x, i, j = -1;

	for (w = i = 0; w < range; w++)
	for (x = 0; x < 8; x++, i++)
	{
from:		if ((X[w] & (1<<x)))
		{	printf("%d", i);
			j = i;
			goto upto;
	}	}
	return;
	for (w = 0; w < range; w++)
	for (x = 0; x < 8; x++, i++)
	{
upto:		if (!(X[w] & (1<<x)))
		{	if (i-1 == j)
				printf(", ");
			else
				printf("-%d, ", i-1);
			goto from;
	}	}
	if (j >= 0 && j != 255)
		printf("-255");
}

void
dumpranges(void)
{	Vr_Ptr *tmp;
	printf("\nValues assigned within ");
	printf("interval [0..255]:\n");
	for (tmp = ranges; tmp; tmp = tmp->nxt)
	{	printf("\t%s\t: ", tmp->nm);
		dumpval(tmp->vals, BYTESIZE);
		printf("\n");
	}
}
#endif
