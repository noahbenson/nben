////////////////////////////////////////////////////////////////////////////////////////////////////
// RealNumber.java, part of nben, a mathematics library for clojure.
// This file defines the real-number class that is compatible with the nben math library.
// 
// Copyright (C) 2012 Noah C. Benson
//
// This file is part of the nben clojure library.
//
// The nben clojure library is free software: you can redistribute it and/or modify it under the 
// terms of the GNU General Public License as published by the Free Software Foundation, either 
// version 3 of the License, or (at your option) any later version.
//
// The nben clojure library is distributed in the hope that it will be useful, but WITHOUT ANY 
// WARRANTY; without even the implied warranty of  MERCHANTABILITY or FITNESS FOR A PARTICULAR
// PURPOSE.  See the GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along with the nben clojure
// library.  If not, see <http://www.gnu.org/licenses/>.
//

package nben.jvm;
import clojure.lang.IPersistentCollection;
import clojure.lang.IPersistentMap;
import clojure.lang.ISeq;
import clojure.lang.IFn;
import clojure.lang.Numbers;

/** RealNumber stores real numbers of arbitrary precision.
 *
 *  A real number class may be initialized
 *  in one of three ways:
 *  (1) with a rational number, in which case the error is saved as 0
 *  (2) with a function that, given a maximum error, calculates the number to within that error
 *  (3) with a lazy seq of seqs, each of which contain a numerical estimate followed by the error.
 *  In cases 2 and 3, the RealNumber class will throw errors if the number is not converging or if
 *  the error is not decreasing.
 *  An estimate of the number can be obtained by calling estimate().
 */
public class RealNumber implements IRealNumber
{
   private static void argError(String msg) {throw new IllegalArgumentException(msg);}
   private static void unstableError(Number preVal, Number preErr, Number nextVal, Number nextErr) {
      throw new IllegalStateException("Real number estimate appears unstable: " + 
                                      preVal.toString() + " +/- " preErr.toString() +
                                      " to " + nextVal.toString() + " +/- " nextErr.toString());
   }
   
   // these are the best error and estimate so far
   private Number m_bestEstimate;
   private Number m_errorBound;
   private IFn m_fn;
   private static final Double zero = Double.valueOf(0);

   // This is used for lazy-seq real numbers
   // this is private because it should never leave the scope of this class; it is not synchromized
   // since the outer class is.
   private static class RealListExpander
      extends AFn
   {
      private static void endedError() {
         throw new IllegalStateException("Infinite series real number ended with non-zero error");
      }
      private static void growthError() {
         throw new IllegalStateException("Error growth observed in infinite series real number");
      }
      private static void badFormatError(Object q) {
         throw new IllegalStateException("Infinite series real number yielded badly formatted" +
                                         " element: " + q.toString());
      }

      public ISeq rest;
      public Number bestEstimate;
      public Number errorBound;

      private void expandElement()
      {
         if (rest == null) endedError();
         Object tmp = rest.first();
         Number newEst = null;
         Number newErr = null;
         if (tmp instanceof ISeq) {
            ISeq el = (ISeq)tmp;
            tmp = el.first();
            ISeq next = el.next();
            if (tmp == null || !(tmp instanceof Number)) badFormatError(el);
            newEst = (Number)tmp;
            if (next != null) {
               tmp = next.first();
               if (tmp == null || !(tmp instanceof Number)) badFormatError(el);
               newErr = (Number)tmp;
            }
         } else if (tmp instanceof Number) {
            // required that abs(lastNumber - number) is descending...
            newEst = (Number)tmp;
         } else badFormatError(tmp);
         // we do the estimation for them if they don't provide it
         if (newErr == null)
            newErr = (Numbers.gt(newEst, bestEstimate)? Numbers.minus(newEst, bestEstimate)
                                                      : Numbers.minus(bestEstimate, newEst));
         if (errorBound != null && Numbers.lte(errorBound, newErr)) growthError();
         // checking is done; go ahead and replace variables
         bestEstimate = newEst;
         errorBound = newErr;
         rest = rest.next();
      }

      public RealListExpander(ISeq q)
      {
         rest = q;
         expandElement();         
      }

      Object invoke(Object errObj)
      {
         double maxErr = ((Number)errObj).doubleValue();
         while (maxErr < errorBound.doubleValue()) expandElement();
         return bestEstimate;
      }
   }

   public RealNumber(ISeq sequence)
   {
      m_fn = new RealListExpander(sequence);
      m_bestEstimate = null;
      m_errorBound = null;
   }
   public RealNumber(IFn estimator)
   {
      m_fn = estimator;
      m_bestEstimate = null;
      m_errorBound = null;
   }
   public RealNumber(Number exact)
   {
      m_fn = null;
      m_bestEstimate = exact;
      m_errorBound = zero;
   }

   // the internal part of estimate that is synced so as to avoid extra calls to the estimator
   private synchronized void updateEstimate(Number errorBound)
   {
      if (m_errorBound != null && Numbers.lt(m_errorBound, errorBound)) return;
      Number newEst = m_fn.invoke(errorBound);
      Number diff = (Numbers.gt(m_bestEstimate, newEst)? Numbers.minusP(m_bestEstimate, newEst)
                                                       : Numbers.minusP(newEst, m_bestEstimate));
      if (m_errorBound != null && Numbers.gte(Numbers.add(diff, errorBound), m_errorBound))
         unstableError(m_bestEstimate, m_errorBound, newEst, errorBound);
      m_errorBound = errorBound;
      m_bestEstimate = newEst;
   }

   /** estimate gives an estimate of the real number that is at least as precise as the given error.
    *
    *  If x = y.estimate(e), then abs(y - x) is less than e. If the error given is less than zero,
    *  an exception is thrown. If the error given is equal to zero, then an exception is thrown 
    *  unless we have 0 error (i.e., this is actually a rational number). 
    */
   public Number estimate(Number errorBound)
   {
      if (Numbers.lt(errorBound, zero))
         argError("Estimate with less than zero error requested");
      else if (Numbers.equiv(errorBound, zero)) {
         if (m_errorBound != null && Numbers.equiv(m_errorBound, zero)) 
            return m_bestEstimate;
         else argError("Zero error requested of irrational number");
      }
      if (m_bestEstimate == null || Numbers.gt(m_errorBound, errorBound))
         // we have to call the synchronized part of the calculation...
         updateEstimate(errorBound);
      return m_bestEstimate;
   }
}
