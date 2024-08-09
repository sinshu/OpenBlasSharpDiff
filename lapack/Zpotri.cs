using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// ZPOTRI computes the inverse of a complex Hermitian positive definite
        /// matrix A using the Cholesky factorization A = U**H*U or A = L*L**H
        /// computed by ZPOTRF.
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
        /// <param name="a">
        /// [in,out] A is COMPLEX*16 array, dimension (LDA,N).
        /// On entry, the triangular factor U or L from the Cholesky
        /// factorization A = U**H*U or A = L*L**H, as computed by
        /// ZPOTRF.
        /// On exit, the upper or lower triangle of the (Hermitian)
        /// inverse of A, overwriting the input factor U or L.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,N).
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// &gt; 0:  if INFO = i, the (i,i) element of the factor U or L is
        /// zero, and the inverse could not be computed.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_zpotri", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Zpotri(
            MatrixLayout matrixLayout,
            char uplo,
            int n,
            Complex* a,
            int lda);
    }
}
