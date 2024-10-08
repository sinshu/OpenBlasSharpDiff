﻿using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DPTTRS solves a tridiagonal system of the form
        ///    A * X = B
        /// using the L*D*L**T factorization of A computed by DPTTRF.  D is a
        /// diagonal matrix specified in the vector D, L is a unit bidiagonal
        /// matrix whose subdiagonal is specified in the vector E, and X and B
        /// are N by NRHS matrices.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the tridiagonal matrix A.  N &gt;= 0.
        /// </param>
        /// <param name="nrhs">
        /// [in] NRHS is INTEGER.
        /// The number of right hand sides, i.e., the number of columns
        /// of the matrix B.  NRHS &gt;= 0.
        /// </param>
        /// <param name="d">
        /// [in] D is DOUBLE PRECISION array, dimension (N).
        /// The n diagonal elements of the diagonal matrix D from the
        /// L*D*L**T factorization of A.
        /// </param>
        /// <param name="e">
        /// [in] E is DOUBLE PRECISION array, dimension (N-1).
        /// The (n-1) subdiagonal elements of the unit bidiagonal factor
        /// L from the L*D*L**T factorization of A.  E can also be regarded
        /// as the superdiagonal of the unit bidiagonal factor U from the
        /// factorization A = U**T*D*U.
        /// </param>
        /// <param name="b">
        /// [in,out] B is DOUBLE PRECISION array, dimension (LDB,NRHS).
        /// On entry, the right hand side vectors B for the system of
        /// linear equations.
        /// On exit, the solution vectors, X.
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// The leading dimension of the array B.  LDB &gt;= max(1,N).
        /// </param>
        /// <returns>
        /// = 0: successful exit
        /// &lt; 0: if INFO = -k, the k-th argument had an illegal value
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dpttrs", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dpttrs(
            MatrixLayout matrixLayout,
            int n,
            int nrhs,
            double* d,
            double* e,
            double* b,
            int ldb);
    }
}
