﻿using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DGTSV  solves the equation
        /// </para>
        /// <para>
        ///    A*X = B,
        /// </para>
        /// <para>
        /// where A is an n by n tridiagonal matrix, by Gaussian elimination with
        /// partial pivoting.
        /// </para>
        /// <para>
        /// Note that the equation  A**T*X = B  may be solved by interchanging the
        /// order of the arguments DU and DL.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrix A.  N &gt;= 0.
        /// </param>
        /// <param name="nrhs">
        /// [in] NRHS is INTEGER.
        /// The number of right hand sides, i.e., the number of columns
        /// of the matrix B.  NRHS &gt;= 0.
        /// </param>
        /// <param name="dl">
        /// [in,out] DL is DOUBLE PRECISION array, dimension (N-1).
        /// On entry, DL must contain the (n-1) sub-diagonal elements of
        /// A.
        /// 
        /// On exit, DL is overwritten by the (n-2) elements of the
        /// second super-diagonal of the upper triangular matrix U from
        /// the LU factorization of A, in DL(1), ..., DL(n-2).
        /// </param>
        /// <param name="d">
        /// [in,out] D is DOUBLE PRECISION array, dimension (N).
        /// On entry, D must contain the diagonal elements of A.
        /// 
        /// On exit, D is overwritten by the n diagonal elements of U.
        /// </param>
        /// <param name="du">
        /// [in,out] DU is DOUBLE PRECISION array, dimension (N-1).
        /// On entry, DU must contain the (n-1) super-diagonal elements
        /// of A.
        /// 
        /// On exit, DU is overwritten by the (n-1) elements of the first
        /// super-diagonal of U.
        /// </param>
        /// <param name="b">
        /// [in,out] B is DOUBLE PRECISION array, dimension (LDB,NRHS).
        /// On entry, the N by NRHS matrix of right hand side matrix B.
        /// On exit, if INFO = 0, the N by NRHS solution matrix X.
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// The leading dimension of the array B.  LDB &gt;= max(1,N).
        /// </param>
        /// <returns>
        /// = 0: successful exit
        /// &lt; 0: if INFO = -i, the i-th argument had an illegal value
        /// &gt; 0: if INFO = i, U(i,i) is exactly zero, and the solution
        /// has not been computed.  The factorization has not been
        /// completed unless i = N.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dgtsv", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dgtsv(
            MatrixLayout matrixLayout,
            int n,
            int nrhs,
            double* dl,
            double* d,
            double* du,
            double* b,
            int ldb);
    }
}
