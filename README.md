---
title: "Advent of Code 2020"
output: html_document
css: modest.css
---
Code to solve the [Advent of Code](http://adventofcode.com/2020/) puzzles. This year, I'm using the puzzles to develop my skills in [Haskell](https://wiki.haskell.org/Haskell). I'm writing up a [commentary on these puzzles and my solutions](https://work.njae.me.uk/tag/advent-of-code/) on my blog.

[Learn you a Haskell](http://learnyouahaskell.com/chapters), [Introduction to Haskell 98](https://www.haskell.org/tutorial/index.html), and [Hackage](https://hackage.haskell.org/) are good resources.

The [Stack documentation](https://docs.haskellstack.org/en/stable/README/) and [How I Start: Haskell](http://howistart.org/posts/haskell/1/) are good sources of using the tools. 


# Toolchain

I'm using the basic Haskell Platform installation, together with `stack` to manage the packages and dependencies (install with
```
$ sudo aptitude install haskell-platform haskell-stack
```
), then updgrade with
```
 stack upgrade --binary-only
```
as the version in the Ubuntu repos is too old to work with current Haskell Stack package sets.

## Creating the repository and project
Create the repository as normal: create the project in Gitolite, clone it, and insert the `.gitignore` and `README.md` files.

There's one package per day, with the code for each package in sub-directories of the root directory. 

Create the basic `stack` project. This will create a new directory. Note that this new directory name can't have a hyphen-delimited word that's just digits, so the project will have to be `advent-of-code`

```
stack new advent-of-code --bare simple
```

Modify the `stack.yaml` file as needed, such as adding the `ghc-options` stanza. 

## Creating subsequent days

Each day lives in a separate directory, with its own `package.yaml` file and code in the `src` directory. (I based this configuration from [mstksg's setup](https://github.com/mstksg/advent-of-code-2018).)

Compile with
```
stack build
```
or 
```
stack build advent01
```

Run with
```
stack exec advent01
```

If you want to pass in additional RTS parameters, do it like this:
```
stack exec -- advent01 +RTS -K0 -RTS
```

Run interactively with
```
stack ghci advent01
```
or 
```
stack ghci advent01:exe:advent01
```
if the first form is ambiguous. 

To profile, use 
```
stack build --executable-profiling --library-profiling --ghc-options="-fprof-auto -rtsopts" advent01
```
then run with
```
stack exec --profile -- advent01 +RTS -p -hy
```
Generate the profile graph with
```
stack exec hp2ps advent01.hp
```

# Packages

Stack is using the [14.16-lts resolver](https://www.stackage.org/lts-16.25) for packages, so make sure you read the [correct documentation for the packages included in it](https://www.stackage.org/lts-16.25/docs). 

Packages I used a lot:

* [Containers](https://hackage.haskell.org/package/containers) (and some [better documentation](https://haskell-containers.readthedocs.io/en/latest/intro.html)); [Unordered containers](https://hackage.haskell.org/package/unordered-containers) is a mostly-equivalent alternative.
* [Attoparsec](https://hackage.haskell.org/package/attoparsec) (and [Megaparsec](https://hackage.haskell.org/package/megaparsec), and [ReadP](https://hackage.haskell.org/package/base-4.14.1.0/docs/Text-ParserCombinators-ReadP.html) once).

There are somewhat decent [tutorials on Megaparsec](https://markkarpov.com/tutorial/megaparsec.html) and [Attoparsec](https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/attoparsec).

Packages I didn't use much, but need to remember:

* [Arithmoi](https://hackage.haskell.org/package/arithmoi) for number theory
* [Pointed List](https://hackage.haskell.org/package/pointedlist-0.6.1) for zipper lists (sometimes circular)
* [Vector](https://hackage.haskell.org/package/vector) for array-like things
* [Linear](https://hackage.haskell.org/package/linear) for coordinate-vector like things
* [Grid](https://hackage.haskell.org/package/grid) for 2-d grids
* [Graph-wrapper](https://hackage.haskell.org/package/graph-wrapper) for graphs
* [Lens](https://hackage.haskell.org/package/lens) (and a [summary of operators](https://github.com/ekmett/lens/wiki/Operators)). I didn't use these much this year, but did a lot last year.
* [RWS](https://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-RWS-Lazy.html) (Reader-Writer-State monad stack); again, used a lot last year but not this year
* [Monad loops](https://hackage.haskell.org/package/monad-loops-0.4.3/docs/Control-Monad-Loops.html), and [a description](https://conscientiousprogrammer.com/blog/2015/12/11/24-days-of-hackage-2015-day-11-monad-loops-avoiding-writing-recursive-functions-by-refactoring/)
* [Replace-Megaparsec](https://github.com/jamesdbrock/replace-megaparsec), for using Mpc for all sorts of things traditionally done with regex substitutions.

# Readme

Build this readme file wth
```
pandoc -s README.md > README.html
```

(Using the [Modest style](https://github.com/markdowncss/modest).)
