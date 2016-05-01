////////////////////////////////////////////////////////////////////////////////////////////////////
// PotentialSum.java
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

import nben.mesh.registration.Util;
import nben.mesh.registration.IPotentialField;

import nben.util.Par;

import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.CancellationException;

/** The PotentialSum class aggregates a set of potential field objects and acts as a single field
 *  equal to the sum of these fields.
 *
 *  @author Noah C. Benson
 */
class PotentialSum implements IPotentialField {

   /** The list of terms in this potential field. This should not generally be accessed directly,
    *  but is made protected for flexibility.
    */
   protected IPotentialField[] terms;

   /** Constructs a PotentialSum object from the give list of fields */
   public PotentialSum(IPotentialField... fields) {
      if (fields != null) {
         PotentialSum ps;
         // see if we have any potential sums...
         int n = 0;
         for (int i = 0; i < fields.length; ++i) {
            if (fields[i] instanceof PotentialSum) {
               ps = (PotentialSum)fields[i];
               n += (ps.terms == null? 0 : ps.terms.length);
            } else 
               ++n;
         }
         // copy the array just in case...
         this.terms = new IPotentialField[n];
         int k = 0;
         for (int i = 0; i < fields.length; ++i) {
            if (fields[i] instanceof PotentialSum) {
               ps = (PotentialSum)fields[i];
               if (ps.terms != null)
                  for (int j = 0; j < ps.terms.length; ++j)
                     this.terms[k++] = ps.terms[j];
            } else 
               this.terms[k++] = fields[i];
         }
      }
   }
   /** Constructs an empty potential sum object */
   public PotentialSum() {
      this.terms = null;
   }
   /** Constructs a potential sum object that operates over the given subset of the given
    *  potential sum field.
    */
   protected PotentialSum(PotentialSum field, int[] ss) {
      // we need to make subfields of each term...
      if (field.terms != null) {
         this.terms = new IPotentialField[field.terms.length];
         for (int i = 0; i < field.terms.length; ++i)
            this.terms[i] = field.terms[i].subfield(ss);
      }
   }
   /** Yields a duplicate PotentialSum object but with the given subset of vertices.
    */
   public PotentialSum subfield(int[] ss) {return new PotentialSum(this, ss);}

   /** Adds a potential field to this object */
   synchronized public void addField(IPotentialField f) {
      IPotentialField[] fs;
      if (f == this) {
         return;
      } else if (f instanceof PotentialSum) {
         fs = ((PotentialSum)f).fields();
         for (int i = 0; i < fs.length; ++i)
            addField(fs[i]);
      } else if (terms == null) {
         terms = new IPotentialField[1];
         terms[0] = f;
      } else {
         fs = new IPotentialField[terms.length + 1];
         System.arraycopy(terms, 0, fs, 0, terms.length);
         fs[terms.length] = f;
         terms = fs;
      }
   }

   /** Remove a potential field from this object */
   synchronized public void removeField(IPotentialField f) {
      IPotentialField[] fs;
      if (terms != null) {
         int i;
         for (i = 0; i < terms.length && terms[i] != f; ++i)
            ;
         if (i < terms.length) {            
            fs = new IPotentialField[terms.length - 1];
            if (i > 0)
               System.arraycopy(terms, 0, fs, 0, i);
            if (i + 1 < terms.length)
               System.arraycopy(terms, i+1, fs, i, terms.length - i - 1);
            terms = fs;
         }
      }
   }

   /** Yields the list of potential fields tracked by this summation */
   synchronized public IPotentialField[] fields() {return terms;}

   /** Calculates the potential value and gradient at the given position X and places the gradient
    *  in the given matrix G; this calls the calculate function of each of the terms of the current
    *  potential sum in turn.
    *
    *  @see IPotentialField
    */
   synchronized public double calculate(double[][] X, double[][] G)
      throws InterruptedException,
             ExecutionException, 
             CancellationException,
             NullPointerException, 
             RejectedExecutionException {
      double potential = 0;
      double tmp;
      // these should multi-thread by themselves, so we can run them iteratively
      for (int i = 0; i < terms.length; ++i) {
         tmp = terms[i].calculate(X, G);
         // if we get an infinite or nan value, we can break; these are errors essentially
         if (Double.isInfinite(tmp) || Double.isNaN(tmp)) {
            potential = tmp;
            break;
         } else
            potential += tmp;
      }
      return potential;
   }

}