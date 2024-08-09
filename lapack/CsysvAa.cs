using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// CSYSV computes the solution to a complex system of linear equations
        ///    A * X = B,
        /// where A is an N-by-N symmetric matrix and X and B are N-by-NRHS
        /// matrices.
        /// </para>
        /// <para>
        /// Aasen&#39;s algorithm is used to factor A as
        ///    A = U**T * T * U,  if UPLO = &#39;U&#39;, or
        ///    A = L * T * L**T,  if UPLO = &#39;L&#39;,
        /// where U (or L) is a product of permutation and unit upper (lower)
        /// triangular matrices, and T is symmetric tridiagonal. The factored
        /// form of A is then used to solve the system of equations A * X = B.
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
        /// [in,out] A is COMPLEX array, dimension (LDA,N).
        /// On entry, the symmetric matrix A.  If UPLO = &#39;U&#39;, the leading
        /// N-by-N upper triangular part of A contains the upper
        /// triangular part of the matrix A, and the strictly lower
        /// triangular part of A is not referenced.  If UPLO = &#39;L&#39;, the
        /// leading N-by-N lower triangular part of A contains the lower
        /// triangular part of the matrix A, and the strictly upper
        /// triangular part of A is not referenced.
        /// 
        /// On exit, if INFO = 0, the tridiagonal matrix T and the
        /// multipliers used to obtain the factor U or L from the
        /// factorization A = U**T*T*U or A = L*T*L**T as computed by
        /// CSYTRF.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,N).
        /// </param>
        /// <param name="ipiv">
        /// [out] IPIV is INTEGER array, dimension (N).
        /// On exit, it contains the details of the interchanges, i.e.,
        /// the row and column k of A were interchanged with the
        /// row and column IPIV(k).
        /// </param>
        /// <param name="b">
        /// [in,out] B is COMPLEX array, dimension (LDB,NRHS).
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
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_csysv_aa", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo CsysvAa(
            MatrixLayout matrixLayout,
            char uplo,
            int n,
            int nrhs,
            Complex32* a,
            int lda,
            int* ipiv,
            Complex32* b,
            int ldb);
    }
}
