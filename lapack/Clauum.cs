using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// CLAUUM computes the product U * U**H or L**H * L, where the triangular
        /// factor U or L is stored in the upper or lower triangular part of
        /// the array A.
        /// </para>
        /// <para>
        /// If UPLO = &#39;U&#39; or &#39;u&#39; then the upper triangle of the result is stored,
        /// overwriting the factor U in A.
        /// If UPLO = &#39;L&#39; or &#39;l&#39; then the lower triangle of the result is stored,
        /// overwriting the factor L in A.
        /// </para>
        /// <para>
        /// This is the blocked form of the algorithm, calling Level 3 BLAS.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="uplo">
        /// [in] UPLO is CHARACTER*1.
        /// Specifies whether the triangular factor stored in the array A
        /// is upper or lower triangular:
        /// = &#39;U&#39;:  Upper triangular
        /// = &#39;L&#39;:  Lower triangular
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the triangular factor U or L.  N &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in,out] A is COMPLEX array, dimension (LDA,N).
        /// On entry, the triangular factor U or L.
        /// On exit, if UPLO = &#39;U&#39;, the upper triangle of A is
        /// overwritten with the upper triangle of the product U * U**H;
        /// if UPLO = &#39;L&#39;, the lower triangle of A is overwritten with
        /// the lower triangle of the product L**H * L.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,N).
        /// </param>
        /// <returns>
        /// = 0: successful exit
        /// &lt; 0: if INFO = -k, the k-th argument had an illegal value
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_clauum", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Clauum(
            MatrixLayout matrixLayout,
            char uplo,
            int n,
            Complex32* a,
            int lda);
    }
}
