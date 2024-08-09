using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// STRTRI computes the inverse of a real upper or lower triangular
        /// matrix A.
        /// </para>
        /// <para>
        /// This is the Level 3 BLAS version of the algorithm.
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
        /// <param name="diag">
        /// [in] DIAG is CHARACTER*1.
        /// = &#39;N&#39;:  A is non-unit triangular;
        /// = &#39;U&#39;:  A is unit triangular.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrix A.  N &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in,out] A is REAL array, dimension (LDA,N).
        /// On entry, the triangular matrix A.  If UPLO = &#39;U&#39;, the
        /// leading N-by-N upper triangular part of the array A contains
        /// the upper triangular matrix, and the strictly lower
        /// triangular part of A is not referenced.  If UPLO = &#39;L&#39;, the
        /// leading N-by-N lower triangular part of the array A contains
        /// the lower triangular matrix, and the strictly upper
        /// triangular part of A is not referenced.  If DIAG = &#39;U&#39;, the
        /// diagonal elements of A are also not referenced and are
        /// assumed to be 1.
        /// On exit, the (triangular) inverse of the original matrix, in
        /// the same storage format.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,N).
        /// </param>
        /// <returns>
        /// = 0: successful exit
        /// &lt; 0: if INFO = -i, the i-th argument had an illegal value
        /// &gt; 0: if INFO = i, A(i,i) is exactly zero.  The triangular
        /// matrix is singular and its inverse can not be computed.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_strtri", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Strtri(
            MatrixLayout matrixLayout,
            char uplo,
            char diag,
            int n,
            float* a,
            int lda);
    }
}
