using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DPOTRF computes the Cholesky factorization of a real symmetric
        /// positive definite matrix A.
        /// </para>
        /// <para>
        /// The factorization has the form
        ///    A = U**T * U,  if UPLO = &#39;U&#39;, or
        ///    A = L  * L**T,  if UPLO = &#39;L&#39;,
        /// where U is an upper triangular matrix and L is lower triangular.
        /// </para>
        /// <para>
        /// This is the block version of the algorithm, calling Level 3 BLAS.
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
        /// [in,out] A is DOUBLE PRECISION array, dimension (LDA,N).
        /// On entry, the symmetric matrix A.  If UPLO = &#39;U&#39;, the leading
        /// N-by-N upper triangular part of A contains the upper
        /// triangular part of the matrix A, and the strictly lower
        /// triangular part of A is not referenced.  If UPLO = &#39;L&#39;, the
        /// leading N-by-N lower triangular part of A contains the lower
        /// triangular part of the matrix A, and the strictly upper
        /// triangular part of A is not referenced.
        /// 
        /// On exit, if INFO = 0, the factor U or L from the Cholesky
        /// factorization A = U**T*U or A = L*L**T.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,N).
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// &gt; 0:  if INFO = i, the leading principal minor of order i
        /// is not positive, and the factorization could not be
        /// completed.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dpotrf", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dpotrf(
            MatrixLayout matrixLayout,
            char uplo,
            int n,
            double* a,
            int lda);
    }
}
