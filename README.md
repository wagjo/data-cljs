Finger trees and miscellaneous functions for ClojureScript data structures.

## Overview

Finger trees are based on https://github.com/wagjo/ftree which is based on https://github.com/clojure/data.finger-tree

Resources on finger trees:

* https://github.com/Chouser/talk-finger-tree
* http://www.soi.city.ac.uk/~ross/papers/FingerTree.html
* http://blip.tv/clojure/chris-houser-finger-trees-custom-persistent-collections-4632874

## Usage

Add the following dependency to your project.clj file:

    [com.wagjo/data-cljs "0.1.0-SNAPSHOT"]

## Introduction

Most of functions working with data structures use polymorphism. This is slow
(as of early 2013) in ClojureScript. Faster variants are implemented here
in separate namespaces. Faster variants does not have plymorphism
nor multiple arities.

Nearly every data manipulation function works with seqs. In ClojureScript, this
is very slow, e.g. for use in games (see [Chris Granger's talk](http://www.youtube.com/watch?v=V1Eu9vZaDYw)).
Specialized functions (e.g. for arrays and vectors) are placed directly in clojure.core
namespace and are given a strange names (alength, subvec). This library provides separate
namespace for each data structure.

Functions are provided for following data structures:
* Javascript Array
* Javascript String
* PersistentVector (for now, most functions there are very slow)
* Finger Tree
* miscellaneous functions for characters

Notable functionalities include:
* fast eager variants of map and map-indexed
* fast variants of reduce and reduce-kv
* reduce-reverse and reduce-kv-reverse
* finger trees supporting standard clojure fns (conj, seq, count, nth, assoc, reduce, ...)

This library provides a fast finger tree implementation. Until 
https://github.com/michalmarczyk/flexvec gets ported to CLJS, finger trees
are good candidate for data structure which provides fast insert/remove.

Note that most of functions for persistent vector are very slow. This too will be fixed when
flexvec gets ported.

## List of functions

Following functions are provided for each data structure. Moreover, some data
structures provide additional functions, e.g. array provides additional mutating
functions.

### Creation
* _empty []_ - clojure.core/empty (without argument)

### Access
* _empty? [o]_ - clojure.core/empty?
* _count [o]_ - clojure.core/count
* _nth [o index]_ - clojure.core/nth without not-found
* _nth* [o index not-found]_ - clojure.core/nth with not-found
* _nth-unchecked [o index]_ - clojure.core/nth without boundary check
* _peekl [o]_ - like clojure.core/peek but always from left
* _peekr [o]_ - like clojure.core/peek but always from right
* _peek [o]_ - clojure.core/peek
* _peekl-unchecked [o]_ - like peekl but without boundary check
* _peekr-unchecked [o]_ - like peekr but without boundary check
* _peek-unchecked [o]_ - like peek but without boundary check

### Modify
* _slice [o start end]_ - like clojure.core/subvec
* _slice-from [o start]_ - like slice, but until the end of o
* _slice-to [o end]_ - like slice, but from the beginning of o
* _split-at [o index]_ - clojure.core/split-at
* _cat [o o2]_ - eager variant of clojure.core/concat
* _popl [o]_ - like clojure.core/pop, but always from left
* _popr [o]_ - like clojure.core/pop, but always from right
* _pop [o]_ - clojure.core/pop
* _popl-unchecked [o]_ - like popl, but without boundary check
* _popr-unchecked [o]_ - like popr, but without boundary check
* _pop-unchecked [o]_ - like pop, but without boundary check
* _assoc [o index val]_ - clojure.core/assoc
* _update [o index f]_ - like clojure.core/update-in, one level only
* _conjl [o val]_ - like clojure.core/conj, but always from left
* _conjr [o val]_ - like clojure.core/conj, but always from right
* _conj [o val]_ - clojure.core/conj
* _conjl-arr [o val-arr]_ - like conjl, but with array of vals
* _conjr-arr [o val-arr]_ - like conjr, but with array of vals
* _conj-arr [o val-arr]_ - like conj, but with array of vals
* _splice [o index n val]_ - fast remove and insert in one go
* _splice-arr [o index n val-arr]_ - fast remove and insert in one go
* _insert-before [o index val]_ - insert one item inside coll
* _insert-before-arr [o index val]_ - insert array of items inside coll
* _remove-at [o index]_ - remove one item from index pos
* _remove-n [o index n]_ - remove n items starting at index pos
* _rip [o index]_ - rips coll and returns [pre-coll item-at suf-coll]
* _sew [pre-coll item-arr suf-coll]_ - opposite of rip, but with arr
* _triml [o n]_ - trims n items from left
* _trimr [o n]_ - trims n items from right
* _trim [o nl nr]_ - trims nl items from left and nr items from right

### Use
* _mape [f o]_ - eager version of clojure.core/map
* _mape-indexed [f o]_ - eager version of clojure.core/map-indexed
* _reduce [f init o]_ - faster version of clojure.core/reduce
* _reduce-reverse [f init o]_ - like reduce but in reverse order
* _reduce-kv [f init o]_ - faster version of clojure.core/reduce-kv
* _reduce-kv-reverse [f init o]_ - like reduce-kv but in reverse order
* _index-of [o val]_ - search for val inside o
* _index-of-from [o val index-from]_ - index-of, starting at index-from

## License

Copyright (C) 2011, 2012, 2013 Rich Hickey, Chris Houser, Jozef Wagner.

The use and distribution terms for this software are covered by the
Eclipse Public License 1.0 
(http://opensource.org/licenses/eclipse-1.0.php) which can be found
 in the file epl-v10.html at the root of this distribution.

By using this software in any fashion, you are agreeing to be bound
by the terms of this license.

You must not remove this notice, or any other, from this software.
