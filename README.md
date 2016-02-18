# nben #########################################################################
A numerical and scientific toolkit written for the JVM.

## Author ######################################################################
Noah C. Benson &lt;<nben@nyu,edu>&gt

## Introduction ################################################################

The nben library is a collection of numerical and scientific routines that its
author has found usefil enough, in the course of his research, to implemented on
the Java Virtual Machine. It is written primarily in
[Clojure](https://clojure.org/) and [Java](https://www.java.com/), though there
are some [Scala](http://www.scala-lang.org/) modules planned as well. 

## Contents ####################################################################

This library currently contains the following modules:

1. **nben.mesh.registration**: This namespace contains data structures for
   expressing potential functions over triangle mesh geometric structures and
   algorithms for finding the minima of such potential fields by warping the
   mesh. Although it was designed for use with 2D projections of cortical
   surface meshes, it is by no means limited or even specific to them.
2. *nben.math*: This namespace is currently an under-development numerical
   interface for clojure. 

This library is in its early stages; expect it to change rapidly.

## License #####################################################################

Copyright (C) 2016 by Noah C. Benson.
This README is part of the nben library.

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program.  If not, see <http://www.gnu.org/licenses/>.
