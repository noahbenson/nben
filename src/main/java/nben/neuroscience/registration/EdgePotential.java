////////////////////////////////////////////////////////////////////////////////////////////////////
// EdgePotential.java
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

import nben.neuroscience.registration.Util;
import nben.neuroscience.registration.IPotentialField;
import nben.neuroscience.registration.ASimplexPotential;
import nben.neuroscience.registration.IDifferentiatedFunction;

import nben.util.Par;
import nben.util.Numbers;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;

/** The EdgePotential class defines the code that computates of potential fields based on the
 *  interactions between neighboring vertices in the mesh. To construct an EdgePotential, one must
 *  explicitly or implicitly choose a differentiated function f; the potential calculated by the
 *  edge potential class will then be the value f(l, l0) where l and l0 are the edge-length and the
 *  reference edge-length respectively.
 *
 *  @author Noah C. Benson
 *  @see ASimplexPotential
 */
public class EdgePotential extends ASimplexPotential {
   /** Constructs an EdgePotential object.
    *  
    *  @see ASimplexPotential
    */
   public EdgePotential(IDifferentiatedFunction[] f, int[][] edges, double[][] X0) {
      super(f, edges, X0);
   }
   public EdgePotential(IDifferentiatedFunction f, int[][] edges, double[][] X0) {
      super(f, edges, X0);
   }
   protected EdgePotential(EdgePotential field, int[] ss) {
      super(field, ss);
   }
   /** Yields a duplicate EdgePotential object but with the given subset of vertices.
    */
   public EdgePotential subfield(int[] ss) {return new EdgePotential(this, ss);}
   /** Actually calculate the value of a single edge and place its gradient in the gradient matrix
    */
   public double calculateSimplex(int e, double[][] X, double[][] G) {
      int j;
      double tmp, d = 0;
      for (j = 0; j < X.length; ++j) {
         tmp = X[j][simplices[1][e]] - X[j][simplices[0][e]];
         G[0][j] = tmp;
         G[1][j] = -tmp;
         d += tmp*tmp;
      }
      d = Math.sqrt(d);
      if (!Numbers.zeroish(d)) {
         for (j = 0; j < X.length; ++j) {
            G[0][j] /= d;
            G[1][j] /= d;
         }
      }
      return d;
   }
}