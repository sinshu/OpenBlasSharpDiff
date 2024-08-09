using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        ///    A = P*U*D*(U**T)*(P**T) or A = P*L*D*(L**T)*(P**T),
        /// </para>
        /// <para>
        /// where U (or L) is unit upper (or lower) triangular matrix,
        /// U**T (or L**T) is the transpose of U (or L), P is a permutation
        /// matrix, P**T is the transpose of P, and D is symmetric and block
        /// diagonal with 1-by-1 and 2-by-2 diagonal blocks.
        /// </para>
        /// <para>
        /// An estimate is obtained for norm(inv(A)), and the reciprocal of the
        /// condition number is computed as RCOND = 1 / (ANORM * norm(inv(A))).
        /// This routine uses BLAS3 solver DSYTRS_3.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="uplo">
        /// [in] UPLO is CHARACTER*1.
        /// Specifies whether the details of the factorization are
        /// stored as an upper or lower triangular matrix:
        /// = &#39;U&#39;:  Upper triangular, form is A = P*U*D*(U**T)*(P**T);
        /// = &#39;L&#39;:  Lower triangular, form is A = P*L*D*(L**T)*(P**T).
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrix A.  N &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in] A is DOUBLE PRECISION array, dimension (LDA,N).
        /// Diagonal of the block diagonal matrix D and factors U or L
        /// as computed by DSYTRF_RK and DSYTRF_BK:
        /// a) ONLY diagonal elements of the symmetric block diagonal
        /// matrix D on the diagonal of A, i.e. D(k,k) = A(k,k);
        /// (superdiagonal (or subdiagonal) elements of D
        /// should be provided on entry in array E), and
        /// b) If UPLO = &#39;U&#39;: factor U in the superdiagonal part of A.
        /// If UPLO = &#39;L&#39;: factor L in the subdiagonal part of A.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,N).
        /// </param>
        /// <param name="e">
        /// [in] E is DOUBLE PRECISION array, dimension (N).
        /// On entry, contains the superdiagonal (or subdiagonal)
        /// elements of the symmetric block diagonal matrix D
        /// with 1-by-1 or 2-by-2 diagonal blocks, where
        /// If UPLO = &#39;U&#39;: E(i) = D(i-1,i),i=2:N, E(1) not referenced;
        /// If UPLO = &#39;L&#39;: E(i) = D(i+1,i),i=1:N-1, E(N) not referenced.
        /// 
        /// NOTE: For 1-by-1 diagonal block D(k), where
        /// 1 &lt;= k &lt;= N, the element E(k) is not referenced in both
        /// UPLO = &#39;U&#39; or UPLO = &#39;L&#39; cases.
        /// </param>
        /// <param name="ipiv">
        /// [in] IPIV is INTEGER array, dimension (N).
        /// Details of the interchanges and the block structure of D
        /// as determined by DSYTRF_RK or DSYTRF_BK.
        /// </param>
        /// <param name="anorm">
        /// [in] ANORM is DOUBLE PRECISION.
        /// The 1-norm of the original matrix A.
        /// </param>
        /// <param name="rcond">
        /// [out] RCOND is DOUBLE PRECISION.
        /// The reciprocal of the condition number of the matrix A,
        /// computed as RCOND = 1/(ANORM * AINVNM), where AINVNM is an
        /// estimate of the 1-norm of inv(A) computed in this routine.
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dsycon_3", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dsycon3(
            MatrixLayout matrixLayout,
            char uplo,
            int n,
            double* a,
            int lda,
            double* e,
            int* ipiv,
            double anorm,
            double* rcond);
    }
}
