#define Version	"Spin Version 3.2.0 -- 8 April 1998"
#define Source	"sliding.spl"

#define uchar	unsigned char
#define DELTA	500
#ifdef MA
#if MA==1
#undef MA
#define MA	100
#endif
#endif
#ifdef W_XPT
#if W_XPT==1
#undef W_XPT
#define W_XPT 1000000
#endif
#endif
#ifndef NFAIR
#define NFAIR	2	/* must be >= 2 */
#endif
#define INLINE	1
#ifdef NP	/* includes np_ demon */
#define HAS_NP	2
#define VERI	3
#define endclaim	3 /* none */
#endif

short nstates2=5;	/* :init: */
#define endstate2	4
short src_ln2 [] = {
	  0,  58,  58,  58,  61,   0, };
uchar reached2 [] = {
	  0,   1,   0,   0,   0,   0, };

short nstates1=19;	/* receiver */
#define endstate1	18
short src_ln1 [] = {
	  0,  34,  39,  41,  41,  42,  42,  40, 
	 44,  44,  46,  46,  47,  45,  50,  37, 
	 52,  37,  52,   0, };
uchar reached1 [] = {
	  0,   0,   1,   1,   0,   1,   0,   0, 
	  1,   1,   1,   0,   1,   0,   1,   0, 
	  1,   1,   0,   0, };

short nstates0=18;	/* sender */
#define endstate0	17
short src_ln0 [] = {
	  0,  10,  11,  17,  17,  17,  19,  21, 
	 21,  21,  22,  22,  20,  25,  15,  27, 
	 15,  27,   0, };
uchar reached0 [] = {
	  0,   0,   0,   1,   0,   0,   1,   1, 
	  0,   0,   1,   0,   0,   1,   0,   1, 
	  1,   0,   0, };
#define _T5	34
#define _T2	35
#define SYNC	0
#define ASYNC	2

char *procname[] = {
   "sender",
   "receiver",
   ":init:",
   ":np_:",
};

typedef struct P2 { /* :init: */
	unsigned _pid : 8;  /* 0..255 */
	unsigned _t   : 3; /* proctype */
	unsigned _p   : 6; /* state    */
	uchar q1;
	uchar q2;
} P2;
#define Air2	(sizeof(P2) - Offsetof(P2, q2) - 1*sizeof(uchar))
typedef struct P1 { /* receiver */
	unsigned _pid : 8;  /* 0..255 */
	unsigned _t   : 3; /* proctype */
	unsigned _p   : 6; /* state    */
	uchar In;
	uchar Out;
	int v;
	int y;
} P1;
#define Air1	(sizeof(P1) - Offsetof(P1, y) - 1*sizeof(int))
typedef struct P0 { /* sender */
	unsigned _pid : 8;  /* 0..255 */
	unsigned _t   : 3; /* proctype */
	unsigned _p   : 6; /* state    */
	uchar In;
	uchar Out;
	int ns;
	int na;
	int x;
} P0;
#define Air0	(sizeof(P0) - Offsetof(P0, x) - 1*sizeof(int))
typedef struct P3 { /* np_ */
	unsigned _pid : 8;  /* 0..255 */
	unsigned _t   : 3; /* proctype */
	unsigned _p   : 6; /* state    */
} P3;
#define Air3	(sizeof(P3) - 3)
#ifdef VERI
#define BASE	1
#else
#define BASE	0
#endif
#ifdef VERBOSE
#define CHECK
#define DEBUG
#endif
#ifdef SAFETY
#ifndef NOFAIR
#define NOFAIR
#endif
#endif
#ifdef NOREDUCE
#define XUSAFE
#if !defined(SAFETY) && !defined(MA)
#define FULLSTACK
#endif
#else
#ifdef BITSTATE
#ifdef SAFETY
#define CNTRSTACK
#else
#define FULLSTACK
#endif
#else
#define FULLSTACK
#endif
#endif
#ifdef BITSTATE
#define NOCOMP
#endif
#ifndef MEMCNT
#ifdef PC
#define MEMCNT	25	/* 32 Mb */
#else
#define MEMCNT	28
#endif
#endif
#if defined(COLLAPSE2) || defined(COLLAPSE3) || defined(COLLAPSE4)
/* accept the above for backward compatibility */
#define COLLAPSE
#endif
#ifdef COLLAPSE
unsigned long ncomps[256+2];
#endif
#define qptr(x)	(((uchar *)&now)+q_offset[x])
#define pptr(x)	(((uchar *)&now)+proc_offset[x])
#define Pptr(x)	((proc_offset[x])?pptr(x):noptr)
#define q_sz(x)	(((Q0 *)qptr(x))->Qlen)

#define MAXQ   	255
#define MAXPROC	255
#define WS		sizeof(long)   /* word size in bytes */
#ifndef VECTORSZ
#define VECTORSZ	1024           /* sv   size in bytes */
#endif

typedef struct Stack  {	 /* for queues and processes */
#if VECTORSZ>32000
	int o_delta;
	int o_offset;
	int o_skip;
	int o_delqs;
#else
	short o_delta;
	short o_offset;
	short o_skip;
	short o_delqs;
#endif
#ifndef XUSAFE
	char *o_name;
#endif
	char *body;
	struct Stack *nxt;
	struct Stack *lst;
} Stack;

typedef struct Svtack { /* for complete state vector */
#if VECTORSZ>32000
	int o_delta;
	int m_delta;
#else
	short o_delta;	 /* current size of frame */
	short m_delta;	 /* maximum size of frame */
#endif
#if SYNC
	short o_boq;
#endif
	char *body;
	struct Svtack *nxt;
	struct Svtack *lst;
} Svtack;

typedef struct Trans {
	short atom;	/* if &2 = atomic trans; if &8 local */
	short st;	/* the nextstate */
#ifdef HAS_UNLESS
	short escp[HAS_UNLESS];	/* lists the escape states */
	short e_trans;	/* if set, this is an escp-trans */
#endif
	short tpe[2];	/* class of operation (for reduction) */
	short qu[6];	/* for conditional selections: qid's  */
	uchar ty[6];	/* ditto: type's */
#ifdef NIBIS
	short om;	/* completion status of preselects */
#endif
	char *tp;	/* src txt of statement */
	int t_id;	/* transition id, unique within proc */
	int forw;	/* index forward transition */
	int back;	/* index return  transition */
	struct Trans *nxt;
} Trans;

Trans ***trans;	/* 1 ptr per state per proctype */

#if defined(FULLSTACK)
struct H_el *Lstate;
#endif
int depthfound = -1;	/* loop detection */
int proc_offset[MAXPROC], proc_skip[MAXPROC];
int q_offset[MAXQ], q_skip[MAXQ];
long vsize;		/* vector size in bytes */
#ifdef SVDUMP
int vprefix=0, svfd;	/* runtime option -pN */
#endif
short boq = -1;	/* blocked_on_queue status */
typedef struct State {
	uchar _nr_pr;
	uchar _nr_qs;
	uchar   _a_t;	/* cycle detection */
#ifndef NOFAIR
	uchar   _cnt[NFAIR];	/* counters, weak fairness */
#endif
#ifndef NOVSZ
#if VECTORSZ<65536
	unsigned short _vsz;
#else
	unsigned long  _vsz;
#endif
#endif
#ifdef HAS_LAST
	uchar  _last;	/* pid executed in last step */
#endif
#ifdef EVENT_TRACE
#if nstates_event<256
	uchar _event;
#else
	unsigned short _event;
#endif
#endif
	uchar sv[VECTORSZ];
} State;

int _; /* a predefined write-only variable */

#define _NP_	3
uchar reached3[3];  /* np_ */
short nstates3 = 3; /* np_ */
#define endstate3	2 /* np_ */

#define start3	0 /* np_ */
#define start2	3
#define start1	1
#define start0	1
#ifdef NP
#define ACCEPT_LAB	1 /* at least 1 in np_ */
#else
#define ACCEPT_LAB	0 /* user-defined accept labels */
#endif
#define PROG_LAB	0 /* progress labels */
uchar *accpstate[4];
uchar *progstate[4];
uchar *reached[4];
uchar *stopstate[4];
uchar *visstate[4];
short q_flds[3];
short q_max[3];
typedef struct Q2 {
	uchar Qlen;	/* q_size */
	uchar _t;	/* q_type */
	struct {
		int fld0;
	} contents[3];
} Q2;
typedef struct Q1 {
	uchar Qlen;	/* q_size */
	uchar _t;	/* q_type */
	struct {
		int fld0;
	} contents[3];
} Q1;
typedef struct Q0 {	/* generic q */
	uchar Qlen, _t;
} Q0;

/** function prototypes **/
char *emalloc(unsigned);
char *Malloc(unsigned);
int Boundcheck(int, int, int, int, Trans *);
/* int abort(void); */
int addqueue(int, int);
/* int atoi(char *); */
int close(int);
int creat(char *, unsigned short);
int write(int, void *, unsigned);
int delproc(int, int);
int endstate(void);
int hstore(char *, int, short);
#ifdef MA
int gstore(char *, int, uchar);
#endif
int q_cond(short, Trans *);
int q_full(int);
int q_len(int);
int q_zero(int);
int qrecv(int, int, int, int);
int unsend(int);
void *sbrk(int);
void Uerror(char *);
void assert(int, char *, int, int, Trans *);
void checkcycles(void);
void crack(int, int, Trans *, short *);
void d_hash(uchar *, int);
void delq(int);
void do_reach(void);
void exit(int);
void hinit(void);
void imed(Trans *, int, int);
void new_state(void);
void p_restor(int);
void p_q_restor(int, int);
void p_q_save(int, int);
void putpeg(int, int);
void putrail(void);
void q_restor(void);
void retrans(int, int, int, short *, uchar *);
void settable(void);
void setq_claim(int, int, char *, int, char *);
void sv_restor(int);
void sv_save(char *);
void tagtable(int, int, int, short *, uchar *);
void uerror(char *);
void unrecv(int, int, int, int, int);
void usage(FILE *);
void wrap_stats(void);
void xrefsrc(int, int, int);
#if defined(FULLSTACK) && defined(BITSTATE)
int  onstack_now(void);
void onstack_init(void);
void onstack_put(void);
void onstack_zap(void);
#endif
#ifndef XUSAFE
int q_S_check(int, int);
int q_R_check(int, int);
uchar q_claim[MAXQ+1];
char *q_name[MAXQ+1];
char *p_name[MAXPROC+1];
#endif
void qsend(int, int, int);
#define Addproc(x)	addproc(x, 0, 0)
#define LOCAL	1
#define Q_FULL_F	2
#define Q_EMPT_F	3
#define Q_EMPT_T	4
#define Q_FULL_T	5
#define TIMEOUT_F	6
#define GLOBAL	7
#define BAD	8
#define ALPHA_F	9
#define NTRANS	36
#ifdef PEG
long peg[NTRANS];
#endif
