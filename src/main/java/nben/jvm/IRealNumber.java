////////////////////////////////////////////////////////////////////////////////////////////////////
// IRealNumber.java, part of nben, a mathematics library for clojure.
// This file defines the real-number interface that is compatible with the nben math library.
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

/** IRealNumber is an interface that is intended for use with the reified RealNumber class. */
public interface IRealNumber
{
   public Number estimate(Number errorBound);
}
