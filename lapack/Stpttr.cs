using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// STPTTR copies a triangular matrix A from standard packed format (TP)
        /// to standard full format (TR).
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="uplo">
        /// [in] UPLO is CHARACTER*1.
        /// = &#39;U&#39;:  A is upper triangular.
        /// = &#39;L&#39;:  A is lower triangular.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrix A. N &gt;= 0.
        /// </param>
        /// <param name="ap">
        /// [in] AP is REAL array, dimension ( N*(N+1)/2 ),.
        /// On entry, the upper or lower triangular matrix A, packed
        /// columnwise in a linear array. The j-th column of A is stored
        /// in the array AP as follows:
        /// if UPLO = &#39;U&#39;, AP(i + (j-1)*j/2) = A(i,j) for 1&lt;=i&lt;=j;
        /// if UPLO = &#39;L&#39;, AP(i + (j-1)*(2n-j)/2) = A(i,j) for j&lt;=i&lt;=n.
        /// </param>
        /// <param name="a">
        /// [out] A is REAL array, dimension ( LDA, N ).
        /// On exit, the triangular matrix A.  If UPLO = &#39;U&#39;, the leading
        /// N-by-N upper triangular part of A contains the upper
        /// triangular part of the matrix A, and the strictly lower
        /// triangular part of A is not referenced.  If UPLO = &#39;L&#39;, the
        /// leading N-by-N lower triangular part of A contains the lower
        /// triangular part of the matrix A, and the strictly upper
        /// triangular part of A is not referenced.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,N).
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_stpttr", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Stpttr(
            MatrixLayout matrixLayout,
            char uplo,
            int n,
            float* ap,
            float* a,
            int lda);
    }
}
