////////////////////////////////////////////////////////////////////////////////////////////////////
// IPotentialField.java
//
// The nben.mesh.registration namespace contains functions related to the registration of 
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

package nben.mesh.registration;

import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.CancellationException;

/** The IPotentialField interface defines the mechanism by which the nben neuroscience registration
 *  library calculates the potential and the potential gradient at a particular point. This class
 *  does not explicitly control the manner in which the gradient is calculated in order to allow
 *  each potential field to be highly optimized in an ideosyncratic way, but it is suggested that
 *  any potential field use the nben.util.Par (and specifically Par.run()) machinery so that the
 *  potential calculation runs very quickly.
 *
 *  @author Noah C. Benson
 */
public interface IPotentialField {
   /** The calculate method yields the value of the potential at the given coordinate matrix X over
    *  the given subset of vertices in subset and places the gradient for these vertices in the
    *  given gradient matrix. On error, an exception is thrown; this only occurs when invoking the
    *  Par.run() function, so it should be referenced for details of exceptions.
    *
    *  @return the potential energy of the potential field at the coordinate configuration X
    *  @param X the (dims x vertices)-sized matrix of vertex coordinates at which to evaluate the
    *           potential
    *  @param G the (dims x vertices)-sized output matrix to which the gradient matrix should be
    *           added; note that this matrix may already contain data from another potential field
    *           that is part of a potential field sum --- thus one should always add to the gradient
    *           and never overwrite it
    *  @see Par.run
    */
   public double calculate(double[][] X, double[][] G)
      throws InterruptedException,
             ExecutionException, 
             CancellationException,
             NullPointerException, 
             RejectedExecutionException;
   /** Potential Fields must be capable of producing duplicates of themselves that are optimized to
    *  operate over a specific subset of the total vertex set. P.sub(ss) produces one such potential
    *  field for the subset ss. If null is given, then this should be interpreted as all-vertices.
    *
    *  @param ss an array of vertex indices over which the potential and gradient should be
    *            calculated; note that this does not change the size of the gradient or
    *            coordinate matrices, which are always expected to be big enough for the entire
    *            calculation, but it does instruct the function to ignore certain vertices. If null,
    *            this should be interpreted as all vertices.
    *  @return the new potential field
    */
   public IPotentialField subfield(int[] ss);
}
