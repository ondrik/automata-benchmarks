# Benchmarks obtained from Pecan

***Pecan*** is an automated theorem prover for automatic sequences using Büchi automata.
For more details, see the [technical report](https://arxiv.org/abs/2102.01727)
and the [repository](http://reedoei.com/pecan):

* Reed Oei, Dun Ma, Christian Schulz, Philipp Hieronymi.  Pecan: An Automated Theorem Prover for Automatic Sequences using Büchi Automata.  [https://arxiv.org/abs/2102.01727](https://arxiv.org/abs/2102.01727). arXiv preprint. 2021.

Note that bigger automata were removed and some runs of Pecan did not
terminate.  You can obtain more and/or bigger automata by running Pecan.

## Directory structure
* `examples/`: benchmarks from examples from the [repository](http://reedoei.com/pecan) of Pecan
* `examples-autfilt/`: the BAs from `examples/` reduced using `autfilt --high` (on some examples, `autfilt` did not finish)
* `inclusion/`: the BAs from the repository for testing inclusion [here](https://github.com/phreppo/buchi-automata-benchmark/tree/master/benchmark/pecan)
* `sturmian_words/`: benchmarks from automating proofs of theorems about
  [Sturmian words](https://en.wikipedia.org/wiki/Sturmian_word) from
  [here](https://github.com/ReedOei/SturmianWords).
* `sturmian_words-autfilt/`: the BAs from `sturmian_words/` reduced using `autfilt --high` (on some examples, `autfilt` did not finish)
