﻿using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// CTRTRS solves a triangular system of the form
        /// </para>
        /// <para>
        ///    A * X = B,  A**T * X = B,  or  A**H * X = B,
        /// </para>
        /// <para>
        /// where A is a triangular matrix of order N, and B is an N-by-NRHS
        /// matrix.  A check is made to verify that A is nonsingular.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="uplo">
        /// [in] UPLO is CHARACTER*1.
        /// = &#39;U&#39;:  A is upper triangular;
        /// = &#39;L&#39;:  A is lower triangular.
        /// </param>
        /// <param name="trans">
        /// [in] TRANS is CHARACTER*1.
        /// Specifies the form of the system of equations:
        /// = &#39;N&#39;:  A * X = B     (No transpose)
        /// = &#39;T&#39;:  A**T * X = B  (Transpose)
        /// = &#39;C&#39;:  A**H * X = B  (Conjugate transpose)
        /// </param>
        /// <param name="diag">
        /// [in] DIAG is CHARACTER*1.
        /// = &#39;N&#39;:  A is non-unit triangular;
        /// = &#39;U&#39;:  A is unit triangular.
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
        /// <param name="a">
        /// [in] A is COMPLEX array, dimension (LDA,N).
        /// The triangular matrix A.  If UPLO = &#39;U&#39;, the leading N-by-N
        /// upper triangular part of the array A contains the upper
        /// triangular matrix, and the strictly lower triangular part of
        /// A is not referenced.  If UPLO = &#39;L&#39;, the leading N-by-N lower
        /// triangular part of the array A contains the lower triangular
        /// matrix, and the strictly upper triangular part of A is not
        /// referenced.  If DIAG = &#39;U&#39;, the diagonal elements of A are
        /// also not referenced and are assumed to be 1.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,N).
        /// </param>
        /// <param name="b">
        /// [in,out] B is COMPLEX array, dimension (LDB,NRHS).
        /// On entry, the right hand side matrix B.
        /// On exit, if INFO = 0, the solution matrix X.
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// The leading dimension of the array B.  LDB &gt;= max(1,N).
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0: if INFO = -i, the i-th argument had an illegal value
        /// &gt; 0: if INFO = i, the i-th diagonal element of A is zero,
        /// indicating that the matrix is singular and the solutions
        /// X have not been computed.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_ctrtrs", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Ctrtrs(
            MatrixLayout matrixLayout,
            char uplo,
            char trans,
            char diag,
            int n,
            int nrhs,
            Complex32* a,
            int lda,
            Complex32* b,
            int ldb);
    }
}
