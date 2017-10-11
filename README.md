# Pacifica Robinhood Migration

[![Build Status](https://travis-ci.org/pacifica/pacifica-robinhood-migration.svg?branch=master)](https://travis-ci.org/pacifica/pacifica-robinhood-migration)
[![Code Climate](https://codeclimate.com/github/pacifica/pacifica-robinhood-migration/badges/gpa.svg)](https://codeclimate.com/github/pacifica/pacifica-robinhood-migration)
[![Issue Count](https://codeclimate.com/github/pacifica/pacifica-robinhood-migration/badges/issue_count.svg)](https://codeclimate.com/github/pacifica/pacifica-robinhood-migration)

This package provides libraries and command-line utilities for working with data from the Robinhood Policy Engine (http://robinhood.sf.net/).

## Compiling

### From source tarball

It is advised to build the source on your target system, to ensure the best compatibility.

Build requirements:

* The Haskell Tool Stack (the `stack` command; https://haskellstack.org)

Unzip and untar the source distribution:

```
tar xcvf haskell-robinhood-library-x.y.z.tar.gz
cd haskell-robinhood-library-x.y.z
```
(Note: `x.y.z` is the Semantic Version for the source distribution.)

Configure and build:

`stack setup && stack build`

Build targets are located in the `.stack-work/` directory.

### From git repository

Install Git SCM (the `git` command; https://git-scm.com/).

For example, on Red Hat Linux, using Yellowdog Updater, Modified (the `yum` command; http://yum.baseurl.org/):

`yum install git`

Retrieve sources:

```
git clone https://stash.pnnl.gov/scm/~bork374/haskell-robinhood-library.git
cd haskell-robinhood-library
git checkout master
```

Then refer to section 2.1 for next compilation steps.

## Install

Copy binaries to location on the current user's `PATH` (environment variable):

`stack install`

## Testing

Run the test suite:

`stack test`

Code coverage is enabled by passing the `--coverage` flag:

`stack test --coverage`

Generate Haskell programming coverage (https://wiki.haskell.org/Haskell_program_coverage) report:

`stack hpc report`

## Documentation

Generate Haskell programming language documentation using Haddock (https://www.haskell.org/haddock/):

`stack haddock`

## Running

There are two methods for running supported, from source and after installed.

### From source

For example, to run the `robinhood-exe` target, provided by the `robinhood` sub-package:

`stack exec robinhood-exe`

### From installation

Refer to section 3 for installation instructions.

For example, to run the `robinhood-exe` target, provided by the `robinhood` sub-package:

`robinhood-exe`
