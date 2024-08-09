using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        ///    A = P*U*D*(U**H)*(P**T) or A = P*L*D*(L**H)*(P**T),
        /// </para>
        /// <para>
        /// where U (or L) is unit upper (or lower) triangular matrix,
        /// U**H (or L**H) is the conjugate of U (or L), P is a permutation
        /// matrix, P**T is the transpose of P, and D is Hermitian and block
        /// diagonal with 1-by-1 and 2-by-2 diagonal blocks.
        /// </para>
        /// <para>
        /// This is the blocked version of the algorithm, calling Level 3 BLAS.
        /// For more information see Further Details section.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="uplo">
        /// [in] UPLO is CHARACTER*1.
        /// Specifies whether the upper or lower triangular part of the
        /// Hermitian matrix A is stored:
        /// = &#39;U&#39;:  Upper triangular
        /// = &#39;L&#39;:  Lower triangular
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrix A.  N &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in,out] A is COMPLEX*16 array, dimension (LDA,N).
        /// On entry, the Hermitian matrix A.
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
        /// On exit, contains:
        /// a) ONLY diagonal elements of the Hermitian block diagonal
        /// matrix D on the diagonal of A, i.e. D(k,k) = A(k,k);
        /// (superdiagonal (or subdiagonal) elements of D
        /// are stored on exit in array E), and
        /// b) If UPLO = &#39;U&#39;: factor U in the superdiagonal part of A.
        /// If UPLO = &#39;L&#39;: factor L in the subdiagonal part of A.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,N).
        /// </param>
        /// <param name="e">
        /// [out] E is COMPLEX*16 array, dimension (N).
        /// On exit, contains the superdiagonal (or subdiagonal)
        /// elements of the Hermitian block diagonal matrix D
        /// with 1-by-1 or 2-by-2 diagonal blocks, where
        /// If UPLO = &#39;U&#39;: E(i) = D(i-1,i), i=2:N, E(1) is set to 0;
        /// If UPLO = &#39;L&#39;: E(i) = D(i+1,i), i=1:N-1, E(N) is set to 0.
        /// 
        /// NOTE: For 1-by-1 diagonal block D(k), where
        /// 1 &lt;= k &lt;= N, the element E(k) is set to 0 in both
        /// UPLO = &#39;U&#39; or UPLO = &#39;L&#39; cases.
        /// </param>
        /// <param name="ipiv">
        /// [out] IPIV is INTEGER array, dimension (N).
        /// IPIV describes the permutation matrix P in the factorization
        /// of matrix A as follows. The absolute value of IPIV(k)
        /// represents the index of row and column that were
        /// interchanged with the k-th row and column. The value of UPLO
        /// describes the order in which the interchanges were applied.
        /// Also, the sign of IPIV represents the block structure of
        /// the Hermitian block diagonal matrix D with 1-by-1 or 2-by-2
        /// diagonal blocks which correspond to 1 or 2 interchanges
        /// at each factorization step. For more info see Further
        /// Details section.
        /// 
        /// If UPLO = &#39;U&#39;,
        /// ( in factorization order, k decreases from N to 1 ):
        /// a) A single positive entry IPIV(k) &gt; 0 means:
        /// D(k,k) is a 1-by-1 diagonal block.
        /// If IPIV(k) != k, rows and columns k and IPIV(k) were
        /// interchanged in the matrix A(1:N,1:N);
        /// If IPIV(k) = k, no interchange occurred.
        /// 
        /// b) A pair of consecutive negative entries
        /// IPIV(k) &lt; 0 and IPIV(k-1) &lt; 0 means:
        /// D(k-1:k,k-1:k) is a 2-by-2 diagonal block.
        /// (NOTE: negative entries in IPIV appear ONLY in pairs).
        /// 1) If -IPIV(k) != k, rows and columns
        /// k and -IPIV(k) were interchanged
        /// in the matrix A(1:N,1:N).
        /// If -IPIV(k) = k, no interchange occurred.
        /// 2) If -IPIV(k-1) != k-1, rows and columns
        /// k-1 and -IPIV(k-1) were interchanged
        /// in the matrix A(1:N,1:N).
        /// If -IPIV(k-1) = k-1, no interchange occurred.
        /// 
        /// c) In both cases a) and b), always ABS( IPIV(k) ) &lt;= k.
        /// 
        /// d) NOTE: Any entry IPIV(k) is always NONZERO on output.
        /// 
        /// If UPLO = &#39;L&#39;,
        /// ( in factorization order, k increases from 1 to N ):
        /// a) A single positive entry IPIV(k) &gt; 0 means:
        /// D(k,k) is a 1-by-1 diagonal block.
        /// If IPIV(k) != k, rows and columns k and IPIV(k) were
        /// interchanged in the matrix A(1:N,1:N).
        /// If IPIV(k) = k, no interchange occurred.
        /// 
        /// b) A pair of consecutive negative entries
        /// IPIV(k) &lt; 0 and IPIV(k+1) &lt; 0 means:
        /// D(k:k+1,k:k+1) is a 2-by-2 diagonal block.
        /// (NOTE: negative entries in IPIV appear ONLY in pairs).
        /// 1) If -IPIV(k) != k, rows and columns
        /// k and -IPIV(k) were interchanged
        /// in the matrix A(1:N,1:N).
        /// If -IPIV(k) = k, no interchange occurred.
        /// 2) If -IPIV(k+1) != k+1, rows and columns
        /// k-1 and -IPIV(k-1) were interchanged
        /// in the matrix A(1:N,1:N).
        /// If -IPIV(k+1) = k+1, no interchange occurred.
        /// 
        /// c) In both cases a) and b), always ABS( IPIV(k) ) &gt;= k.
        /// 
        /// d) NOTE: Any entry IPIV(k) is always NONZERO on output.
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
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_zhetrf_rk", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo ZhetrfRk(
            MatrixLayout matrixLayout,
            char uplo,
            int n,
            Complex* a,
            int lda,
            Complex* e,
            int* ipiv);
    }
}
