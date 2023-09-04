# Language Inclusion Benchmarks from Hyperproperties 

**[original source](https://github.com/ravenbeutner/automata-benchmarks-from-hyperproperties)**

This repository contains a range of language inclusion benchmarks on Büchi automata.  
The instances come from hyperproperty model-checking problems produced using [*AutoHyper*](https://github.com/AutoHyper/AutoHyper). 
When checking a HyperLTL formula that involves quantifier alternations, we can naturally encode the model-checking problem as a language inclusion problem of Büchi automata. 
For details see 

> AutoHyper: Explicit-State Model Checking for HyperLTL. Raven Beutner and Bernd Finkbeiner. TACAS 2023.

This repository currently contains 3 families of benchmarks: 

- `gni/` contains instances obtained when checking generalized non-interference (GNI) on small boolean programs with varying size (but small bisimulation quotient)
- `nusmv/` contains instances obtained when checking a wide range of properties on NuSMV models created by [2].
- `planning/` contains instances obtained when checking properties arising in the planning domain, created by [2].

Each instance consists of two automata in the [HOA format](http://adl.github.io/hoaf/), called `<name>_A.hoa` and `<name>_B.hoa`.
The problem is to check if (the language of) `<name>_A.hoa` is contained in (the language of) `<name>_B.hoa`.


## References 

[1] AutoHyper: Explicit-State Model Checking for HyperLTL. Raven Beutner and Bernd Finkbeiner. TACAS 2023.

[2] Bounded Model Checking for Hyperproperties. Tzu-Han Hsu, César Sánchez, and Borzoo Bonakdarpour. TACAS 2021
