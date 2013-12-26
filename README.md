# phenex

<img src="https://github.com/downloads/hyPiRion/phenex/phenex.png"
 alt="Phenex" title="The sigil of Phenex." align="right" />

>The Thirty-Seventh Spirit is Phenex (or Pheynix). He is a great Marquis, and
>appeareth like the Bird Phoenix, having the Voice of a Child. He singeth many
>sweet notes before the Exorcist, which he must not regard, but by-and-by he
>must bid him put on Human Shape. Then he will speak marvellously of all
>wonderful Sciences if required. He is a Poet, good and excellent. And he will
>be willing to perform thy requests.  
>
>He hath hopes also to return to the Seventh Throne after 1,200 years more, as
>he said unto Solomon. He governeth 20 Legions of Spirits. And his Seal is this,
>which wear thou, etc.
>
> -- S.L. MacGregor Mathers

Phenex is a boosting program with diverse classifiers, like music to your ears.

Phenex is feature complete, except (unfortunately) for proper documentation.
Command line parsing, helpful error messages and documentation may appear in the
future, but do not expect it to be.

## Installation

To install, one needs to have sbcl [(1)][], GNU make [(2)][] and buildapp
[(3)][] install on your system. On Debian-based systems, this can be installed
through the following command:

```bash
sudo apt-get install sbcl make buildapp
```

As of now, one need to have quicklisp [(4)][] installed as well, but this
dependency will be removed in the future. To install quicklisp on your system
properly, download the `quicklisp.lisp` file and load it with sbcl like so:

```bash
wget http://beta.quicklisp.org/quicklisp.lisp
sbcl --load "quicklisp.lisp"
```
Then issue the following commands:

```lisp
(quicklisp-quickstart:install)
(ql:add-to-init-file)
```

The default values should set up the system correctly, but if not, one should
take a look at the [quicklisp installation page][quicklisp-install] for more
information. The `Makefile` has an ASDF-path which is relative to the quicklisp
installation, so be sure to find out where the quicklisp system is placed.

## Usage

phenex is ran through issuing `./phenex args` in the shell, with the proper
options.

### Options

Different commands (*TODO: fix this properly*)

    -p, --preprocess (none | discretize-uniform bincount | ...) -- default: none
	--split training-part  -- default: 0.8
	-i, --input file -- default: none, must be specified
	-o, --output file -- default: stdout
	-r, --random seed -- default: (based on timestamp)
	-c, --classifier classifier opts -- can be called multiple times
	-v, --verbosity (summary | ...)

## Example usage

TODO

## Resources

There are some example datasets to work on within the `datasets` folder. They
all come from the [Irvine Machine Learning repository][IMLR], and more can be
found there.

## Licence

Copyright © 2012 Jean Niklas L'orange

*Sigil of Phenex*, the Phenex logo, is in the public domain because its
copyright has expired. The rest is distributed under the Eclipse Public License,
the same as Clojure.

[(1)]: http://www.sbcl.org/ "Steel Bank Common Lisp"
[(2)]: http://www.gnu.org/software/make/ "GNU Make"
[(3)]: http://www.xach.com/lisp/buildapp/ "Buildapp - Create executables with SBCL"
[(4)]: http://www.quicklisp.org/ "Quicklisp"
[quicklisp-install]: http://www.quicklisp.org/beta/ "Installing quicklisp"
[IMLR]: http://archive.ics.uci.edu/ml/ "Irvine Machine Learning repository"
