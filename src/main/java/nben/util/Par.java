////////////////////////////////////////////////////////////////////////////////////////////////////
// Par.java
//
// The nben.util namespace contains functions that are generally useful across the nben JVM library.
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

package nben.util;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.Callable;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.CancellationException;

import java.util.HashSet;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.LinkedList;
import java.util.Arrays;

/** The nben.util.Par class is a static class that creates a single interface to and philosophy for
 *  parallelization in the nben library. Note that, because the nben library is intended as a 
 *  numerical processing library, the Par class is not setup to help perform complex parallelization
 *  such as coordination between threads with semaphors and the like. Instead, this class is 
 *  intended to make it easy to distribute a large complex calculation across many threads using a
 *  single ExecutorService object.
 *
 *  @author Noah C. Benson
 */
public final class Par {

   /** The MIN_SUGGESTED_TASKS value is intended as a guideline for when one ought to parallelize a
    *  small calculation instead of executing it directly in the current thread. Generally speaking,
    *  if you have a small calculation that needs to be done n times, you should do it in a loop in
    *  the current thread if n is less than this value. This is by no means a hard-and-fast absolute
    *  recommendation --- it is merely a suggestion.
    */
   public final static int MIN_SUGGESTED_TASKS = 200;

   // Data about how to optimally multi-thread
   private static ExecutorService m_pool;
   private static int m_nthreads;
   /** Par.pool() yields an ExecutorService with Par.worker() threads
    *
    *  @return an ExecutorService with Par.workers() active threads.
    *  @see    workers
    */
   public synchronized static final ExecutorService pool() {return m_pool;}
   /** Par.workers() yields the optimal number of threads to use with the ExecutorService obtained
    *  by calling Par.pool().
    *
    *  @return the number of active threads in the Par.pool() ExecutorService object.
    *  @see    pool
    */
   public synchronized static final int workers() {return m_nthreads;}
   /** Sets the number of workers to n and modifies the executor service yielded by Util.pool() to
    *  be optimal for the number of workers.
    *
    *  @param n the number of workers that the Par system should assume are available
    */
   public synchronized static final void setWorkers(int n) {
      if (n < 1)
         throw new IllegalArgumentException("number of workers must be > 0");
      m_nthreads = n;
      m_pool.shutdown();
      m_pool = Executors.newFixedThreadPool(n);
   }
   static {
      m_nthreads = Runtime.getRuntime().availableProcessors();
      m_pool = Executors.newFixedThreadPool(m_nthreads);
   }

   /** Par.run(rs) runs the given set of Runnable workers over the threads of the Par class's
    *  executor service. In the case of error, an exception is thrown.
    *
    *  @param rs an array of Runnable objects to multi-thread
    *  @throws InterruptedException if the current thread was interrupted while waiting on a Future
    *  @throws ExecutionException if one of the computations threw an exception
    *  @throws CancellationException if one of the computations was cancelled
    *  @throws NullPointerException if rs or one of the elements of rs is null
    *  @throws RejectedExecutionException if one of the tasks cannot be scheduled for execution
    */
   public final static void run(Runnable[] rs) 
      throws InterruptedException,
             ExecutionException, 
             CancellationException,
             NullPointerException, 
             RejectedExecutionException {
      if (rs == null) {
         throw new NullPointerException("Argument rs to Par.run is null"); 
      } else if (rs.length == 1) {
         rs[0].run();
      } else if (rs.length > 0) {
         int i;
         ExecutorService exc = Par.pool();
         Future[] fut = new Future[rs.length];
         for (i = 0; i < rs.length; ++i) {
            fut[i] = exc.submit(rs[i]);
         }
         for (i = 0; i < rs.length; ++i) {
            fut[i].get();
         }
      }
   }
   /** Par.run(cs) runs the given set of Callable workers over the threads of the Par class's
    *  executor service and returns an array of the values returned by the workers. On error, an
    *  exception is thrown.
    *
    *  @param cs an array of Callable objects to multi-thread
    *  @return an array of return values, one per Callable object in cs, or null if cs is empty
    *  @throws InterruptedException if the current thread was interrupted while waiting on a Future
    *  @throws ExecutionException if one of the computations threw an exception
    *  @throws CancellationException if one of the computations was cancelled
    *  @throws NullPointerException if rs or one of the elements of rs is null
    *  @throws RejectedExecutionException if one of the tasks cannot be scheduled for execution
    */
   public final static <T> Object[] run(Callable<T>[] cs) 
      throws InterruptedException,
             ExecutionException, 
             CancellationException,
             NullPointerException, 
             RejectedExecutionException {
      if (cs == null) {
         throw new NullPointerException("Argument cs to Par.run is null"); 
      } else if (cs.length == 1) {
         Object[] res = new Object[1];
         try {
            res[0] = cs[0].call();
         } catch (Exception e) {
            throw new ExecutionException("Exception while running single-threaded callable", e);
         }
         return res;
      } else if (cs.length > 0) {
         int i;
         ExecutorService exc = Par.pool();
         @SuppressWarnings("unchecked") Future<T>[] fut = (Future<T>[]) new Future[cs.length];
         Object[] res = new Object[cs.length];
         for (i = 0; i < cs.length; ++i) {
            fut[i] = exc.submit(cs[i]);
         }
         for (i = 0; i < cs.length; ++i) {
            res[i] = fut[i].get();
         }
         return res;
      } else {
         return new Object[0];
      }
   }
   /** Par.run(cs, res) runs the given set of Callable workers over the threads of the Par class's
    *  executor service and returns the given array res after placing the return value of each 
    *  callable in cs into the corresponding element of res. On error, an exception is thrown.
    *
    *  @param cs an array of Callable objects to multi-thread
    *  @param res an array, the same size as cs, that will be used to store the results of the 
    *             callables in cs; if res is null, then a generic array is allocated by casting
    *             an array of Object's to T[]
    *  @return an array of return values, one per Callable object in cs, or null if cs is empty
    *  @throws InterruptedException if the current thread was interrupted while waiting on a Future
    *  @throws ExecutionException if one of the computations threw an exception
    *  @throws CancellationException if one of the computations was cancelled
    *  @throws NullPointerException if cs or one of the elements of cs is null
    *  @throws RejectedExecutionException if one of the tasks cannot be scheduled for execution
    *  @throws IllegalArgumentException if the length of res less than the length of cs
    */
   @SuppressWarnings("unchecked") 
   public final static <T> T[] run(Callable<T>[] cs, T[] res) 
      throws InterruptedException,
             ExecutionException, 
             CancellationException,
             NullPointerException, 
             RejectedExecutionException,
             IllegalArgumentException {
      if (cs == null) {
         throw new NullPointerException("Argument cs to Par.run is null");
      } else if (cs.length > 0) {
         int i;
         ExecutorService exc = Par.pool();
         Future<T>[] fut = (Future<T>[]) new Future[cs.length];
         if (res == null) res = (T[]) new Object[cs.length];
         if (res.length < cs.length)
            throw new IllegalArgumentException("Argument res to Par.run is shorter than cs");
         for (i = 0; i < cs.length; ++i) {
            fut[i] = exc.submit(cs[i]);
         }
         for (i = 0; i < cs.length; ++i) {
            res[i] = fut[i].get();
         }
         return res;
      } else {
         return new T[0];
      }
   }

   /** Par.submit(rs) submits the given array of runnable objects for execution and returns an array
    *  of Future objects that correspond to the submitted futures in rs. On error, an exception is
    *  thrown.
    *
    *  @param rs an array of Runnable objects to multi-thread
    *  @return an array of Future objects corresponding to the given list of runnables, rs
    *  @throws NullPointerException if rs or one of the elements of rs is null
    *  @throws RejectedExecutionException if one of the tasks cannot be scheduled for execution
    */
   public final static Future[] submit(Runnable[] rs) 
      throws NullPointerException, 
             RejectedExecutionException {
      if (rs == null) {
         throw new NullPointerException("Argument rs to Par.submit is null"); 
      } else if (rs.length > 0) {
         int i;
         ExecutorService exc = Par.pool();
         Future[] fut = new Future[rs.length];
         for (i = 0; i < rs.length; ++i) {
            fut[i] = exc.submit(rs[i]);
         }
         return fut;
      }
      return null;
   }
   /** Par.submit(cs) submits the given array of callable objects for execution and returns an array
    *  of Future objects that correspond to the submitted futures in cs. On error, an exception is
    *  thrown.
    *
    *  @param cs an array of Callable objects to multi-thread
    *  @return an array of Future objects corresponding to the given list of callables, cs, or null
    *          if cs is empty
    *  @throws NullPointerException if cs or one of the elements of cs is null
    *  @throws RejectedExecutionException if one of the tasks cannot be scheduled for execution
    */
   public final static <T> Future[] submit(Callable<T>[] cs) 
      throws NullPointerException, 
             RejectedExecutionException {
      if (cs == null) {
         throw new NullPointerException("Argument cs to Par.submit is null"); 
      } else if (cs.length > 0) {
         int i;
         ExecutorService exc = Par.pool();
         Future[] fut = new Future[cs.length];
         for (i = 0; i < cs.length; ++i) {
            fut[i] = exc.submit(cs[i]);
         }
         return fut;
      }
      return null;
   }
   /** Par.submit(cs, futs) submits the given array of callable objects for execution and returns
    *  the given array of Future objects futs after filling in the elemnts such that futs[i]
    *  corresponds to the submitted future in cs[i]. On error, an exception is thrown.
    *
    *  @param cs an array of Callable objects to multi-thread
    *  @param futs an array of Future objects into which the Future generated by submitting the
    *              callables in cs are placed; if futs is null, then a Future generic array
    *              is allocated by casting a Future Object array
    *  @return the array futs after populating it with the Future objects corresponding to the
    *          submissions from the Callable objects in cs, or null if cs is empty
    *  @throws NullPointerException if cs or one of the elements of cs is null
    *  @throws RejectedExecutionException if one of the tasks cannot be scheduled for execution
    *  @throws IllegalArgumentException if the length of res less than the length of cs
    */
   @SuppressWarnings("unchecked") 
   public final static <T> Future<T>[] submit(Callable<T>[] cs, Future<T>[] futs)
      throws NullPointerException, 
             RejectedExecutionException,
             IllegalArgumentException {
      if (cs == null) {
         throw new NullPointerException("Argument cs to Par.submit is null"); 
      } else if (cs.length == 0) {
         return null;
      } else if (cs.length > 0) {
         int i;
         ExecutorService exc = Par.pool();
         if (futs == null)
            futs = (Future<T>[]) new Future[cs.length];
         if (futs.length < cs.length)
            throw new IllegalArgumentException("Argument res to Par.submit is shorter than cs");
         for (i = 0; i < cs.length; ++i) {
            futs[i] = exc.submit(cs[i]);
         }
         return futs;
      }
      return null;
   }
}
