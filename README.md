[![Travis build status](https://travis-ci.org/mrc-ide/malariaEquilibrium.svg?branch=master)](https://travis-ci.org/mrc-ide/malariaEquilibrium)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/mrc-ide/malariaEquilibrium?branch=master&svg=true)](https://ci.appveyor.com/project/mrc-ide/malariaEquilibrium)

# malariaEquilibrium

Often we are interested in the state of a given malaria transmission model at
equibrium. However, some models (e.g. the Griffin et al. 2014 model) are quite
complex, and can result in equilibrium solutions that are fairly in-depth. In
these situations it is useful to have a "canonical" equilibrium solution that is
tried and tested, and can be used reliably by multiple users. This package aims
to be a place for hosting these canonical solutions, and for storing useful
tests, checks and plotting functions for exploring a given solution.

## How to interact with this repos

If you have a new model with an equilibrium solution that you would like to host
here, then please go ahead and do so *in a new branch*. The master branch will
correspond to the current most active model used by the malaria group, and
should generally be left alone unless you know what you're doing.

Any changes that you propose to branches that you did not create yourself (e.g.
the master branch) should be done through pull requests, rather than directly,
to give others the opportunity to review your changes before they are
implemented in code. This stipulations are in place because there are other R
packages that test against the equilibrium solutions from this package, and so
any changes here may cause other packages to fail if they are not know about
beforehand.
