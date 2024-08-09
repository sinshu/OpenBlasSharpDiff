using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// CTRTTP copies a triangular matrix A from full format (TR) to standard
        /// packed format (TP).
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
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrices AP and A.  N &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in] A is COMPLEX array, dimension (LDA,N).
        /// On entry, the triangular matrix A.  If UPLO = &#39;U&#39;, the leading
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
        /// <param name="ap">
        /// [out] AP is COMPLEX array, dimension ( N*(N+1)/2 ),.
        /// On exit, the upper or lower triangular matrix A, packed
        /// columnwise in a linear array. The j-th column of A is stored
        /// in the array AP as follows:
        /// if UPLO = &#39;U&#39;, AP(i + (j-1)*j/2) = A(i,j) for 1&lt;=i&lt;=j;
        /// if UPLO = &#39;L&#39;, AP(i + (j-1)*(2n-j)/2) = A(i,j) for j&lt;=i&lt;=n.
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_ctrttp", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Ctrttp(
            MatrixLayout matrixLayout,
            char uplo,
            int n,
            Complex32* a,
            int lda,
            Complex32* ap);
    }
}
