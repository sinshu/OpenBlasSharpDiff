using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// ZSYTRS2 solves a system of linear equations A*X = B with a complex
        /// symmetric matrix A using the factorization A = U*D*U**T or
        /// A = L*D*L**T computed by ZSYTRF and converted by ZSYCONV.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="uplo">
        /// [in] UPLO is CHARACTER*1.
        /// Specifies whether the details of the factorization are stored
        /// as an upper or lower triangular matrix.
        /// = &#39;U&#39;:  Upper triangular, form is A = U*D*U**T;
        /// = &#39;L&#39;:  Lower triangular, form is A = L*D*L**T.
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
        /// [in,out] A is COMPLEX*16 array, dimension (LDA,N).
        /// The block diagonal matrix D and the multipliers used to
        /// obtain the factor U or L as computed by ZSYTRF.
        /// Note that A is input / output. This might be counter-intuitive,
        /// and one may think that A is input only. A is input / output. This
        /// is because, at the start of the subroutine, we permute A in a
        /// &quot;better&quot; form and then we permute A back to its original form at
        /// the end.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,N).
        /// </param>
        /// <param name="ipiv">
        /// [in] IPIV is INTEGER array, dimension (N).
        /// Details of the interchanges and the block structure of D
        /// as determined by ZSYTRF.
        /// </param>
        /// <param name="b">
        /// [in,out] B is COMPLEX*16 array, dimension (LDB,NRHS).
        /// On entry, the right hand side matrix B.
        /// On exit, the solution matrix X.
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// The leading dimension of the array B.  LDB &gt;= max(1,N).
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_zsytrs2", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Zsytrs2(
            MatrixLayout matrixLayout,
            char uplo,
            int n,
            int nrhs,
            Complex* a,
            int lda,
            int* ipiv,
            Complex* b,
            int ldb);
    }
}
