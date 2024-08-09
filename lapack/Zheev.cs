using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// ZHEEV computes all eigenvalues and, optionally, eigenvectors of a
        /// complex Hermitian matrix A.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="jobz">
        /// [in] JOBZ is CHARACTER*1.
        /// = &#39;N&#39;:  Compute eigenvalues only;
        /// = &#39;V&#39;:  Compute eigenvalues and eigenvectors.
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
        /// [in,out] A is COMPLEX*16 array, dimension (LDA, N).
        /// On entry, the Hermitian matrix A.  If UPLO = &#39;U&#39;, the
        /// leading N-by-N upper triangular part of A contains the
        /// upper triangular part of the matrix A.  If UPLO = &#39;L&#39;,
        /// the leading N-by-N lower triangular part of A contains
        /// the lower triangular part of the matrix A.
        /// On exit, if JOBZ = &#39;V&#39;, then if INFO = 0, A contains the
        /// orthonormal eigenvectors of the matrix A.
        /// If JOBZ = &#39;N&#39;, then on exit the lower triangle (if UPLO=&#39;L&#39;)
        /// or the upper triangle (if UPLO=&#39;U&#39;) of A, including the
        /// diagonal, is destroyed.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,N).
        /// </param>
        /// <param name="w">
        /// [out] W is DOUBLE PRECISION array, dimension (N).
        /// If INFO = 0, the eigenvalues in ascending order.
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// &gt; 0:  if INFO = i, the algorithm failed to converge; i
        /// off-diagonal elements of an intermediate tridiagonal
        /// form did not converge to zero.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_zheev", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Zheev(
            MatrixLayout matrixLayout,
            char jobz,
            char uplo,
            int n,
            Complex* a,
            int lda,
            double* w);
    }
}
