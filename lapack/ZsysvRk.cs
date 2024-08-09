using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// The bounded Bunch-Kaufman (rook) diagonal pivoting method is used
        /// to factor A as
        ///    A = P*U*D*(U**T)*(P**T),  if UPLO = &#39;U&#39;, or
        ///    A = P*L*D*(L**T)*(P**T),  if UPLO = &#39;L&#39;,
        /// where U (or L) is unit upper (or lower) triangular matrix,
        /// U**T (or L**T) is the transpose of U (or L), P is a permutation
        /// matrix, P**T is the transpose of P, and D is symmetric and block
        /// diagonal with 1-by-1 and 2-by-2 diagonal blocks.
        /// </para>
        /// <para>
        /// ZSYTRF_RK is called to compute the factorization of a complex
        /// symmetric matrix.  The factored form of A is then used to solve
        /// the system of equations A * X = B by calling BLAS3 routine ZSYTRS_3.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="uplo">
        /// [in] UPLO is CHARACTER*1.
        /// Specifies whether the upper or lower triangular part of the
        /// symmetric matrix A is stored:
        /// = &#39;U&#39;:  Upper triangle of A is stored;
        /// = &#39;L&#39;:  Lower triangle of A is stored.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The number of linear equations, i.e., the order of the
        /// matrix A.  N &gt;= 0.
        /// </param>
        /// <param name="nrhs">
        /// [in] NRHS is INTEGER.
        /// The number of right hand sides, i.e., the number of columns
        /// of the matrix B.  NRHS &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in,out] A is COMPLEX*16 array, dimension (LDA,N).
        /// On entry, the symmetric matrix A.
        /// If UPLO = &#39;U&#39;: the leading N-by-N upper triangular part
        /// of A contains the upper triangular part of the matrix A,
        /// and the strictly lower triangular part of A is not
        /// referenced.
        /// 
        /// If UPLO = &#39;L&#39;: the leading N-by-N lower triangular part
        /// of A contains the lower triangular part of the matrix A,
        /// and the strictly upper triangular part of A is not
        /// referenced.
        /// 
        /// On exit, if INFO = 0, diagonal of the block diagonal
        /// matrix D and factors U or L  as computed by ZSYTRF_RK:
        /// a) ONLY diagonal elements of the symmetric block diagonal
        /// matrix D on the diagonal of A, i.e. D(k,k) = A(k,k);
        /// (superdiagonal (or subdiagonal) elements of D
        /// are stored on exit in array E), and
        /// b) If UPLO = &#39;U&#39;: factor U in the superdiagonal part of A.
        /// If UPLO = &#39;L&#39;: factor L in the subdiagonal part of A.
        /// 
        /// For more info see the description of ZSYTRF_RK routine.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,N).
        /// </param>
        /// <param name="e">
        /// [out] E is COMPLEX*16 array, dimension (N).
        /// On exit, contains the output computed by the factorization
        /// routine ZSYTRF_RK, i.e. the superdiagonal (or subdiagonal)
        /// elements of the symmetric block diagonal matrix D
        /// with 1-by-1 or 2-by-2 diagonal blocks, where
        /// If UPLO = &#39;U&#39;: E(i) = D(i-1,i), i=2:N, E(1) is set to 0;
        /// If UPLO = &#39;L&#39;: E(i) = D(i+1,i), i=1:N-1, E(N) is set to 0.
        /// 
        /// NOTE: For 1-by-1 diagonal block D(k), where
        /// 1 &lt;= k &lt;= N, the element E(k) is set to 0 in both
        /// UPLO = &#39;U&#39; or UPLO = &#39;L&#39; cases.
        /// 
        /// For more info see the description of ZSYTRF_RK routine.
        /// </param>
        /// <param name="ipiv">
        /// [out] IPIV is INTEGER array, dimension (N).
        /// Details of the interchanges and the block structure of D,
        /// as determined by ZSYTRF_RK.
        /// 
        /// For more info see the description of ZSYTRF_RK routine.
        /// </param>
        /// <param name="b">
        /// [in,out] B is COMPLEX*16 array, dimension (LDB,NRHS).
        /// On entry, the N-by-NRHS right hand side matrix B.
        /// On exit, if INFO = 0, the N-by-NRHS solution matrix X.
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// The leading dimension of the array B.  LDB &gt;= max(1,N).
        /// </param>
        /// <returns>
        /// = 0: successful exit
        /// 
        /// &lt; 0: If INFO = -k, the k-th argument had an illegal value
        /// 
        /// &gt; 0: If INFO = k, the matrix A is singular, because:
        /// If UPLO = &#39;U&#39;: column k in the upper
        /// triangular part of A contains all zeros.
        /// If UPLO = &#39;L&#39;: column k in the lower
        /// triangular part of A contains all zeros.
        /// 
        /// Therefore D(k,k) is exactly zero, and superdiagonal
        /// elements of column k of U (or subdiagonal elements of
        /// column k of L ) are all zeros. The factorization has
        /// been completed, but the block diagonal matrix D is
        /// exactly singular, and division by zero will occur if
        /// it is used to solve a system of equations.
        /// 
        /// NOTE: INFO only stores the first occurrence of
        /// a singularity, any subsequent occurrence of singularity
        /// is not stored in INFO even though the factorization
        /// always completes.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_zsysv_rk", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo ZsysvRk(
            MatrixLayout matrixLayout,
            char uplo,
            int n,
            int nrhs,
            Complex* a,
            int lda,
            Complex* e,
            int* ipiv,
            Complex* b,
            int ldb);
    }
}
