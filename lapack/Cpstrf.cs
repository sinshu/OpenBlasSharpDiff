using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// CPSTRF computes the Cholesky factorization with complete
        /// pivoting of a complex Hermitian positive semidefinite matrix A.
        /// </para>
        /// <para>
        /// The factorization has the form
        ///    P**T * A * P = U**H * U ,  if UPLO = &#39;U&#39;,
        ///    P**T * A * P = L  * L**H,  if UPLO = &#39;L&#39;,
        /// where U is an upper triangular matrix and L is lower triangular, and
        /// P is stored as vector PIV.
        /// </para>
        /// <para>
        /// This algorithm does not attempt to check that A is positive
        /// semidefinite. This version of the algorithm calls level 3 BLAS.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="uplo">
        /// [in] UPLO is CHARACTER*1.
        /// Specifies whether the upper or lower triangular part of the
        /// symmetric matrix A is stored.
        /// = &#39;U&#39;:  Upper triangular
        /// = &#39;L&#39;:  Lower triangular
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrix A.  N &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in,out] A is COMPLEX array, dimension (LDA,N).
        /// On entry, the symmetric matrix A.  If UPLO = &#39;U&#39;, the leading
        /// n by n upper triangular part of A contains the upper
        /// triangular part of the matrix A, and the strictly lower
        /// triangular part of A is not referenced.  If UPLO = &#39;L&#39;, the
        /// leading n by n lower triangular part of A contains the lower
        /// triangular part of the matrix A, and the strictly upper
        /// triangular part of A is not referenced.
        /// 
        /// On exit, if INFO = 0, the factor U or L from the Cholesky
        /// factorization as above.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,N).
        /// </param>
        /// <param name="piv">
        /// [out] PIV is INTEGER array, dimension (N).
        /// PIV is such that the nonzero entries are P( PIV(K), K ) = 1.
        /// </param>
        /// <param name="rank">
        /// [out] RANK is INTEGER.
        /// The rank of A given by the number of steps the algorithm
        /// completed.
        /// </param>
        /// <param name="tol">
        /// [in] TOL is REAL.
        /// User defined tolerance. If TOL &lt; 0, then N*U*MAX( A(K,K) )
        /// will be used. The algorithm terminates at the (K-1)st step
        /// if the pivot &lt;= TOL.
        /// </param>
        /// <returns>
        /// &lt; 0: If INFO = -K, the K-th argument had an illegal value,
        /// = 0: algorithm completed successfully, and
        /// &gt; 0: the matrix A is either rank deficient with computed rank
        /// as returned in RANK, or is not positive semidefinite. See
        /// Section 7 of LAPACK Working Note #161 for further
        /// information.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_cpstrf", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Cpstrf(
            MatrixLayout matrixLayout,
            char uplo,
            int n,
            Complex32* a,
            int lda,
            int* piv,
            int* rank,
            float tol);
    }
}
