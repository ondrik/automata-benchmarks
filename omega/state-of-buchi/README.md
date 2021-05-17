# Random benchmarks from State of Büchi Complementation

These benchmarks contain state-based acceptance Büchi automata from the paper
below and were downloaded from [GOAL's
webpage](http://goal.im.ntu.edu.tw/wiki/doku.php).

* Ming-Hsien Tsai, Seth Fogarty, Moshe Y. Vardi, and Yih-Kuen Tsay. [State of
  Büchi Complementation](https://doi.org/10.1007/978-3-642-18098-9_28).
  CIAA 2010, LNCS 6482, 261–271, August, 2010.

They are randomly generated using the Tabakov-Vardi model (Tabakov, D., Vardi,
M.Y.: [Experimental evaluation of classical automata
constructions](https://doi.org/10.1007/11591191_28). In: Proc. of LPAR’05,
Springer (2005) 396–411) over a two letter alphabet, starting with 15/20
states, with various different parameters.  No post-processing is applied to
those automata, so some pre-filtering/reductions might be necessary (see the
directories with HOA format for some pre-processed inputs).

In the directories:

 * `original/` denotes *original automata*
 * `after-reduce/` denotes *after simplification using
   [REDUCE](http://www.languageinclusion.org/doku.php?id=tools)*
 * `after-reduce-autfilt/` denotes *after simplification using
   [REDUCE](http://www.languageinclusion.org/doku.php?id=tools), followed by
   a simplification by SPOT's [autfilt](https://spot.lrde.epita.fr/)*.
 * `one-hot-encoding/` denotes encoding into HOA format using *one-hot
   encoding* (every alphabet symbol is translated into a new atomic
   proposition)
 * `binary-encoding/` denotes encoding into HOA format using *binary encoding*
   (log2 atomic propositions wrt size of input alphabet are used with each
   alphabet symbol being assigned a Boolean combination of atomic propositions)
