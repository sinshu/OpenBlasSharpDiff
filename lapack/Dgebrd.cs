﻿using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DGEBRD reduces a general real M-by-N matrix A to upper or lower
        /// bidiagonal form B by an orthogonal transformation: Q**T * A * P = B.
        /// </para>
        /// <para>
        /// If m &gt;= n, B is upper bidiagonal; if m &lt; n, B is lower bidiagonal.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="m">
        /// [in] M is INTEGER.
        /// The number of rows in the matrix A.  M &gt;= 0.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The number of columns in the matrix A.  N &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in,out] A is DOUBLE PRECISION array, dimension (LDA,N).
        /// On entry, the M-by-N general matrix to be reduced.
        /// On exit,
        /// if m &gt;= n, the diagonal and the first superdiagonal are
        /// overwritten with the upper bidiagonal matrix B; the
        /// elements below the diagonal, with the array TAUQ, represent
        /// the orthogonal matrix Q as a product of elementary
        /// reflectors, and the elements above the first superdiagonal,
        /// with the array TAUP, represent the orthogonal matrix P as
        /// a product of elementary reflectors;
        /// if m &lt; n, the diagonal and the first subdiagonal are
        /// overwritten with the lower bidiagonal matrix B; the
        /// elements below the first subdiagonal, with the array TAUQ,
        /// represent the orthogonal matrix Q as a product of
        /// elementary reflectors, and the elements above the diagonal,
        /// with the array TAUP, represent the orthogonal matrix P as
        /// a product of elementary reflectors.
        /// See Further Details.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,M).
        /// </param>
        /// <param name="d">
        /// [out] D is DOUBLE PRECISION array, dimension (min(M,N)).
        /// The diagonal elements of the bidiagonal matrix B:
        /// D(i) = A(i,i).
        /// </param>
        /// <param name="e">
        /// [out] E is DOUBLE PRECISION array, dimension (min(M,N)-1).
        /// The off-diagonal elements of the bidiagonal matrix B:
        /// if m &gt;= n, E(i) = A(i,i+1) for i = 1,2,...,n-1;
        /// if m &lt; n, E(i) = A(i+1,i) for i = 1,2,...,m-1.
        /// </param>
        /// <param name="tauq">
        /// [out] TAUQ is DOUBLE PRECISION array, dimension (min(M,N)).
        /// The scalar factors of the elementary reflectors which
        /// represent the orthogonal matrix Q. See Further Details.
        /// </param>
        /// <param name="taup">
        /// [out] TAUP is DOUBLE PRECISION array, dimension (min(M,N)).
        /// The scalar factors of the elementary reflectors which
        /// represent the orthogonal matrix P. See Further Details.
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value.
        /// </returns>
        /// <remarks>
        /// <para>
        ///  The matrices Q and P are represented as products of elementary
        ///  reflectors:
        /// </para>
        /// <para>
        ///  If m &gt;= n,
        /// </para>
        /// <para>
        ///     Q = H(1) H(2) . . . H(n)  and  P = G(1) G(2) . . . G(n-1)
        /// </para>
        /// <para>
        ///  Each H(i) and G(i) has the form:
        /// </para>
        /// <para>
        ///     H(i) = I - tauq * v * v**T  and G(i) = I - taup * u * u**T
        /// </para>
        /// <para>
        ///  where tauq and taup are real scalars, and v and u are real vectors;
        ///  v(1:i-1) = 0, v(i) = 1, and v(i+1:m) is stored on exit in A(i+1:m,i);
        ///  u(1:i) = 0, u(i+1) = 1, and u(i+2:n) is stored on exit in A(i,i+2:n);
        ///  tauq is stored in TAUQ(i) and taup in TAUP(i).
        /// </para>
        /// <para>
        ///  If m &lt; n,
        /// </para>
        /// <para>
        ///     Q = H(1) H(2) . . . H(m-1)  and  P = G(1) G(2) . . . G(m)
        /// </para>
        /// <para>
        ///  Each H(i) and G(i) has the form:
        /// </para>
        /// <para>
        ///     H(i) = I - tauq * v * v**T  and G(i) = I - taup * u * u**T
        /// </para>
        /// <para>
        ///  where tauq and taup are real scalars, and v and u are real vectors;
        ///  v(1:i) = 0, v(i+1) = 1, and v(i+2:m) is stored on exit in A(i+2:m,i);
        ///  u(1:i-1) = 0, u(i) = 1, and u(i+1:n) is stored on exit in A(i,i+1:n);
        ///  tauq is stored in TAUQ(i) and taup in TAUP(i).
        /// </para>
        /// <para>
        ///  The contents of A on exit are illustrated by the following examples:
        /// </para>
        /// <para>
        ///  m = 6 and n = 5 (m &gt; n):          m = 5 and n = 6 (m &lt; n):
        /// </para>
        /// <para>
        ///    (  d   e   u1  u1  u1 )           (  d   u1  u1  u1  u1  u1 )
        ///    (  v1  d   e   u2  u2 )           (  e   d   u2  u2  u2  u2 )
        ///    (  v1  v2  d   e   u3 )           (  v1  e   d   u3  u3  u3 )
        ///    (  v1  v2  v3  d   e  )           (  v1  v2  e   d   u4  u4 )
        ///    (  v1  v2  v3  v4  d  )           (  v1  v2  v3  e   d   u5 )
        ///    (  v1  v2  v3  v4  v5 )
        /// </para>
        /// <para>
        ///  where d and e denote diagonal and off-diagonal elements of B, vi
        ///  denotes an element of the vector defining H(i), and ui an element of
        ///  the vector defining G(i).
        /// </para>
        /// </remarks>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dgebrd", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dgebrd(
            MatrixLayout matrixLayout,
            int m,
            int n,
            double* a,
            int lda,
            double* d,
            double* e,
            double* tauq,
            double* taup);
    }
}
