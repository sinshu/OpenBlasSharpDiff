﻿using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// ZPPRFS improves the computed solution to a system of linear
        /// equations when the coefficient matrix is Hermitian positive definite
        /// and packed, and provides error bounds and backward error estimates
        /// for the solution.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="uplo">
        /// [in] UPLO is CHARACTER*1.
        /// = &#39;U&#39;:  Upper triangle of A is stored;
        /// = &#39;L&#39;:  Lower triangle of A is stored.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrix A.  N &gt;= 0.
        /// </param>
        /// <param name="nrhs">
        /// [in] NRHS is INTEGER.
        /// The number of right hand sides, i.e., the number of columns
        /// of the matrices B and X.  NRHS &gt;= 0.
        /// </param>
        /// <param name="ap">
        /// [in] AP is COMPLEX*16 array, dimension (N*(N+1)/2).
        /// The upper or lower triangle of the Hermitian matrix A, packed
        /// columnwise in a linear array.  The j-th column of A is stored
        /// in the array AP as follows:
        /// if UPLO = &#39;U&#39;, AP(i + (j-1)*j/2) = A(i,j) for 1&lt;=i&lt;=j;
        /// if UPLO = &#39;L&#39;, AP(i + (j-1)*(2n-j)/2) = A(i,j) for j&lt;=i&lt;=n.
        /// </param>
        /// <param name="afp">
        /// [in] AFP is COMPLEX*16 array, dimension (N*(N+1)/2).
        /// The triangular factor U or L from the Cholesky factorization
        /// A = U**H*U or A = L*L**H, as computed by DPPTRF/ZPPTRF,
        /// packed columnwise in a linear array in the same format as A
        /// (see AP).
        /// </param>
        /// <param name="b">
        /// [in] B is COMPLEX*16 array, dimension (LDB,NRHS).
        /// The right hand side matrix B.
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// The leading dimension of the array B.  LDB &gt;= max(1,N).
        /// </param>
        /// <param name="x">
        /// [in,out] X is COMPLEX*16 array, dimension (LDX,NRHS).
        /// On entry, the solution matrix X, as computed by ZPPTRS.
        /// On exit, the improved solution matrix X.
        /// </param>
        /// <param name="ldx">
        /// [in] LDX is INTEGER.
        /// The leading dimension of the array X.  LDX &gt;= max(1,N).
        /// </param>
        /// <param name="ferr">
        /// [out] FERR is DOUBLE PRECISION array, dimension (NRHS).
        /// The estimated forward error bound for each solution vector
        /// X(j) (the j-th column of the solution matrix X).
        /// If XTRUE is the true solution corresponding to X(j), FERR(j)
        /// is an estimated upper bound for the magnitude of the largest
        /// element in (X(j) - XTRUE) divided by the magnitude of the
        /// largest element in X(j).  The estimate is as reliable as
        /// the estimate for RCOND, and is almost always a slight
        /// overestimate of the true error.
        /// </param>
        /// <param name="berr">
        /// [out] BERR is DOUBLE PRECISION array, dimension (NRHS).
        /// The componentwise relative backward error of each solution
        /// vector X(j) (i.e., the smallest relative change in
        /// any element of A or B that makes X(j) an exact solution).
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_zpprfs", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Zpprfs(
            MatrixLayout matrixLayout,
            char uplo,
            int n,
            int nrhs,
            Complex* ap,
            Complex* afp,
            Complex* b,
            int ldb,
            Complex* x,
            int ldx,
            double* ferr,
            double* berr);
    }
}
