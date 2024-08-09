using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        ///     A = P*U*D*(U**T)*(P**T) or A = P*L*D*(L**T)*(P**T),
        /// </para>
        /// <para>
        /// where U (or L) is unit upper (or lower) triangular matrix,
        /// U**T (or L**T) is the transpose of U (or L), P is a permutation
        /// matrix, P**T is the transpose of P, and D is symmetric and block
        /// diagonal with 1-by-1 and 2-by-2 diagonal blocks.
        /// </para>
        /// <para>
        /// SSYTRI_3 sets the leading dimension of the workspace  before calling
        /// SSYTRI_3X that actually computes the inverse.  This is the blocked
        /// version of the algorithm, calling Level 3 BLAS.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="uplo">
        /// [in] UPLO is CHARACTER*1.
        /// Specifies whether the details of the factorization are
        /// stored as an upper or lower triangular matrix.
        /// = &#39;U&#39;:  Upper triangle of A is stored;
        /// = &#39;L&#39;:  Lower triangle of A is stored.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrix A.  N &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in,out] A is REAL array, dimension (LDA,N).
        /// On entry, diagonal of the block diagonal matrix D and
        /// factors U or L as computed by SSYTRF_RK and SSYTRF_BK:
        /// a) ONLY diagonal elements of the symmetric block diagonal
        /// matrix D on the diagonal of A, i.e. D(k,k) = A(k,k);
        /// (superdiagonal (or subdiagonal) elements of D
        /// should be provided on entry in array E), and
        /// b) If UPLO = &#39;U&#39;: factor U in the superdiagonal part of A.
        /// If UPLO = &#39;L&#39;: factor L in the subdiagonal part of A.
        /// 
        /// On exit, if INFO = 0, the symmetric inverse of the original
        /// matrix.
        /// If UPLO = &#39;U&#39;: the upper triangular part of the inverse
        /// is formed and the part of A below the diagonal is not
        /// referenced;
        /// If UPLO = &#39;L&#39;: the lower triangular part of the inverse
        /// is formed and the part of A above the diagonal is not
        /// referenced.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,N).
        /// </param>
        /// <param name="e">
        /// [in] E is REAL array, dimension (N).
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
        /// as determined by SSYTRF_RK or SSYTRF_BK.
        /// </param>
        /// <returns>
        /// = 0: successful exit
        /// &lt; 0: if INFO = -i, the i-th argument had an illegal value
        /// &gt; 0: if INFO = i, D(i,i) = 0; the matrix is singular and its
        /// inverse could not be computed.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_ssytri_3", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Ssytri3(
            MatrixLayout matrixLayout,
            char uplo,
            int n,
            float* a,
            int lda,
            float* e,
            int* ipiv);
    }
}
