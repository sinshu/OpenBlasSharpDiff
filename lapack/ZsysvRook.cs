using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// ZSYSV_ROOK computes the solution to a complex system of linear
        /// equations
        ///    A * X = B,
        /// where A is an N-by-N symmetric matrix and X and B are N-by-NRHS
        /// matrices.
        /// </para>
        /// <para>
        /// The diagonal pivoting method is used to factor A as
        ///    A = U * D * U**T,  if UPLO = &#39;U&#39;, or
        ///    A = L * D * L**T,  if UPLO = &#39;L&#39;,
        /// where U (or L) is a product of permutation and unit upper (lower)
        /// triangular matrices, and D is symmetric and block diagonal with
        /// 1-by-1 and 2-by-2 diagonal blocks.
        /// </para>
        /// <para>
        /// ZSYTRF_ROOK is called to compute the factorization of a complex
        /// symmetric matrix A using the bounded Bunch-Kaufman (&quot;rook&quot;) diagonal
        /// pivoting method.
        /// </para>
        /// <para>
        /// The factored form of A is then used to solve the system
        /// of equations A * X = B by calling ZSYTRS_ROOK.
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
        /// On entry, the symmetric matrix A.  If UPLO = &#39;U&#39;, the leading
        /// N-by-N upper triangular part of A contains the upper
        /// triangular part of the matrix A, and the strictly lower
        /// triangular part of A is not referenced.  If UPLO = &#39;L&#39;, the
        /// leading N-by-N lower triangular part of A contains the lower
        /// triangular part of the matrix A, and the strictly upper
        /// triangular part of A is not referenced.
        /// 
        /// On exit, if INFO = 0, the block diagonal matrix D and the
        /// multipliers used to obtain the factor U or L from the
        /// factorization A = U*D*U**T or A = L*D*L**T as computed by
        /// ZSYTRF_ROOK.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,N).
        /// </param>
        /// <param name="ipiv">
        /// [out] IPIV is INTEGER array, dimension (N).
        /// Details of the interchanges and the block structure of D,
        /// as determined by ZSYTRF_ROOK.
        /// 
        /// If UPLO = &#39;U&#39;:
        /// If IPIV(k) &gt; 0, then rows and columns k and IPIV(k)
        /// were interchanged and D(k,k) is a 1-by-1 diagonal block.
        /// 
        /// If IPIV(k) &lt; 0 and IPIV(k-1) &lt; 0, then rows and
        /// columns k and -IPIV(k) were interchanged and rows and
        /// columns k-1 and -IPIV(k-1) were inerchaged,
        /// D(k-1:k,k-1:k) is a 2-by-2 diagonal block.
        /// 
        /// If UPLO = &#39;L&#39;:
        /// If IPIV(k) &gt; 0, then rows and columns k and IPIV(k)
        /// were interchanged and D(k,k) is a 1-by-1 diagonal block.
        /// 
        /// If IPIV(k) &lt; 0 and IPIV(k+1) &lt; 0, then rows and
        /// columns k and -IPIV(k) were interchanged and rows and
        /// columns k+1 and -IPIV(k+1) were inerchaged,
        /// D(k:k+1,k:k+1) is a 2-by-2 diagonal block.
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
        /// &lt; 0: if INFO = -i, the i-th argument had an illegal value
        /// &gt; 0: if INFO = i, D(i,i) is exactly zero.  The factorization
        /// has been completed, but the block diagonal matrix D is
        /// exactly singular, so the solution could not be computed.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_zsysv_rook", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo ZsysvRook(
            MatrixLayout matrixLayout,
            char uplo,
            int n,
            int nrhs,
            Complex* a,
            int lda,
            int* ipiv,
            Complex* b,
            int ldb);
    }
}
