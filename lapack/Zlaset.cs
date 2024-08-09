using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// ZLASET initializes a 2-D array A to BETA on the diagonal and
        /// ALPHA on the offdiagonals.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="uplo">
        /// [in] UPLO is CHARACTER*1.
        /// Specifies the part of the matrix A to be set.
        /// = &#39;U&#39;:      Upper triangular part is set. The lower triangle
        /// is unchanged.
        /// = &#39;L&#39;:      Lower triangular part is set. The upper triangle
        /// is unchanged.
        /// Otherwise:  All of the matrix A is set.
        /// </param>
        /// <param name="m">
        /// [in] M is INTEGER.
        /// On entry, M specifies the number of rows of A.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// On entry, N specifies the number of columns of A.
        /// </param>
        /// <param name="alpha">
        /// [in] ALPHA is COMPLEX*16.
        /// All the offdiagonal array elements are set to ALPHA.
        /// </param>
        /// <param name="beta">
        /// [in] BETA is COMPLEX*16.
        /// All the diagonal array elements are set to BETA.
        /// </param>
        /// <param name="a">
        /// [out] A is COMPLEX*16 array, dimension (LDA,N).
        /// On entry, the m by n matrix A.
        /// On exit, A(i,j) = ALPHA, 1 &lt;= i &lt;= m, 1 &lt;= j &lt;= n, i.ne.j;
        /// A(i,i) = BETA , 1 &lt;= i &lt;= min(m,n)
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,M).
        /// </param>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_zlaset", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Zlaset(
            MatrixLayout matrixLayout,
            char uplo,
            int m,
            int n,
            Complex alpha,
            Complex beta,
            Complex* a,
            int lda);
    }
}
