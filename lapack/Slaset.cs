using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// SLASET initializes an m-by-n matrix A to BETA on the diagonal and
        /// ALPHA on the offdiagonals.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="uplo">
        /// [in] UPLO is CHARACTER*1.
        /// Specifies the part of the matrix A to be set.
        /// = &#39;U&#39;:      Upper triangular part is set; the strictly lower
        /// triangular part of A is not changed.
        /// = &#39;L&#39;:      Lower triangular part is set; the strictly upper
        /// triangular part of A is not changed.
        /// Otherwise:  All of the matrix A is set.
        /// </param>
        /// <param name="m">
        /// [in] M is INTEGER.
        /// The number of rows of the matrix A.  M &gt;= 0.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The number of columns of the matrix A.  N &gt;= 0.
        /// </param>
        /// <param name="alpha">
        /// [in] ALPHA is REAL.
        /// The constant to which the offdiagonal elements are to be set.
        /// </param>
        /// <param name="beta">
        /// [in] BETA is REAL.
        /// The constant to which the diagonal elements are to be set.
        /// </param>
        /// <param name="a">
        /// [out] A is REAL array, dimension (LDA,N).
        /// On exit, the leading m-by-n submatrix of A is set as follows:
        /// 
        /// if UPLO = &#39;U&#39;, A(i,j) = ALPHA, 1&lt;=i&lt;=j-1, 1&lt;=j&lt;=n,
        /// if UPLO = &#39;L&#39;, A(i,j) = ALPHA, j+1&lt;=i&lt;=m, 1&lt;=j&lt;=n,
        /// otherwise,     A(i,j) = ALPHA, 1&lt;=i&lt;=m, 1&lt;=j&lt;=n, i.ne.j,
        /// 
        /// and, for all UPLO, A(i,i) = BETA, 1&lt;=i&lt;=min(m,n).
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,M).
        /// </param>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_slaset", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Slaset(
            MatrixLayout matrixLayout,
            char uplo,
            int m,
            int n,
            float alpha,
            float beta,
            float* a,
            int lda);
    }
}
