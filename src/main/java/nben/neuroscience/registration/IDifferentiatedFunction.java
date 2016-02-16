////////////////////////////////////////////////////////////////////////////////////////////////////
// IDifferentiatedFunction.java
//
// The nben.neuroscience.registration namespace contains functions related to the registration of 
// surface meshes to models defined on the cortical surface; it is designed to work with front-end 
// neuroscience libraries in other languages, such as the Mathematica Neurotica`Registration 
// namespace of the Neurotica library (see https://github.com/noahbenson/Neurotica/).
//
// Copyright (C) 2016 by Noah C. Benson.
// This file is part of the nben JVM library.
//
// This program is free software: you can redistribute it and/or modify it under the terms of the
// GNU General Public License as published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
// without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See
// the GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along with this program.
// If not, see <http://www.gnu.org/licenses/>.

package nben.neuroscience.registration;

/** An IDifferentiatedFunction is an object that accepts a value x and a reference x0, returns a
 *  single value y(x, x0), and can additionally return the derivative dy(x, x0). Classes that
 *  inherit from IDifferentiatedFunction are intended as shape modifiers for various potential
 *  field types. For example, an edge potential object specifies the direction of the gradient
 *  and the quantity that determines the potential value, but it must be given, for example, a
 *  harmonic function in order to know what potential value and gradient magnitude is relevant.
 *  In this case, the initial distance between a pair of vertices joined by an edge might be r0
 *  while the harmonic function would be f. If, at a particular coordinate configuration, the 
 *  edge length is r(X), then the potential value is f(r(X), r0), and the gradient direction is 
 *  d/dX f(r(X), r0) = (d f(r(X), r0))/(d r(X)) * (d r(X))/dX; the edge potential function
 *  calculates r(X) and (d r(X))/dX while the harmonic function is responsible for calculating
 *  f(r(X), r0) and (d f(r(X), r0))/(d r(X)).
 *
 *  @author Noah C. Benson
 */
public interface IDifferentiatedFunction {
   /** For an IDifferentiatedFunction f, f.y(x, x0) yields the value of the function x given 
    *  the reference value x0. It is up to the individual differentiated function how x0 should
    *  be used; as two examples, a simple harmonic function yields (x - x0)^2 and a simple 
    *  Lennard-Jones function yields 1 + (x0/x)^2 - 0.5*(x0/x).
    *
    *  @param x the value at which to calculate the function value
    *  @param x0 the reference value for the value x
    *  @return the function value at the point x given the reference x0
    */
   public double y(double x, double x0);
   /** For an IDifferentiatedFunction f, f.dy(x, x0) yields the derivative of the function x given
    *  the reference value x0, equivalent to (d f(x, x0))/(d x). It is up to the individual 
    *  differentiated function how x0 should be used.
    *
    *  @param x the value at which to calculate the function derivative
    *  @param x0 the reference value for the value x
    *  @return the function derivative at the point x given the reference x0
    */
   public double dy(double x, double x0);
}
