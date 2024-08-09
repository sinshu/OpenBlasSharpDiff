﻿using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// ZPTTRF computes the L*D*L**H factorization of a complex Hermitian
        /// positive definite tridiagonal matrix A.  The factorization may also
        /// be regarded as having the form A = U**H *D*U.
        /// </para>
        /// </summary>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrix A.  N &gt;= 0.
        /// </param>
        /// <param name="d">
        /// [in,out] D is DOUBLE PRECISION array, dimension (N).
        /// On entry, the n diagonal elements of the tridiagonal matrix
        /// A.  On exit, the n diagonal elements of the diagonal matrix
        /// D from the L*D*L**H factorization of A.
        /// </param>
        /// <param name="e">
        /// [in,out] E is COMPLEX*16 array, dimension (N-1).
        /// On entry, the (n-1) subdiagonal elements of the tridiagonal
        /// matrix A.  On exit, the (n-1) subdiagonal elements of the
        /// unit bidiagonal factor L from the L*D*L**H factorization of A.
        /// E can also be regarded as the superdiagonal of the unit
        /// bidiagonal factor U from the U**H *D*U factorization of A.
        /// </param>
        /// <returns>
        /// = 0: successful exit
        /// &lt; 0: if INFO = -k, the k-th argument had an illegal value
        /// &gt; 0: if INFO = k, the leading principal minor of order k
        /// is not positive; if k &lt; N, the factorization could not
        /// be completed, while if k = N, the factorization was
        /// completed, but D(N) &lt;= 0.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_zpttrf", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Zpttrf(
            int n,
            double* d,
            Complex* e);
    }
}
