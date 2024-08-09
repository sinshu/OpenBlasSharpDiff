using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DPOTRF2 computes the Cholesky factorization of a real symmetric
        /// positive definite matrix A using the recursive algorithm.
        /// </para>
        /// <para>
        /// The factorization has the form
        ///    A = U**T * U,  if UPLO = &#39;U&#39;, or
        ///    A = L  * L**T,  if UPLO = &#39;L&#39;,
        /// where U is an upper triangular matrix and L is lower triangular.
        /// </para>
        /// <para>
        /// This is the recursive version of the algorithm. It divides
        /// the matrix into four submatrices:
        /// </para>
        /// <para>
        ///        [  A11 | A12  ]  where A11 is n1 by n1 and A22 is n2 by n2
        ///    A = [ -----|----- ]  with n1 = n/2
        ///        [  A21 | A22  ]       n2 = n-n1
        /// </para>
        /// <para>
        /// The subroutine calls itself to factor A11. Update and scale A21
        /// or A12, update A22 then calls itself to factor A22.
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
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dpotrf2", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dpotrf2(
            MatrixLayout matrixLayout,
            char uplo,
            int n,
            double* a,
            int lda);
    }
}
