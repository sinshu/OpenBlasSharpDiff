using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DSYSV_AA_2STAGE computes the solution to a real system of
        /// linear equations
        ///    A * X = B,
        /// where A is an N-by-N symmetric matrix and X and B are N-by-NRHS
        /// matrices.
        /// </para>
        /// <para>
        /// Aasen&#39;s 2-stage algorithm is used to factor A as
        ///    A = U**T * T * U,  if UPLO = &#39;U&#39;, or
        ///    A = L * T * L**T,  if UPLO = &#39;L&#39;,
        /// where U (or L) is a product of permutation and unit upper (lower)
        /// triangular matrices, and T is symmetric and band. The matrix T is
        /// then LU-factored with partial pivoting. The factored form of A
        /// is then used to solve the system of equations A * X = B.
        /// </para>
        /// <para>
        /// This is the blocked version of the algorithm, calling Level 3 BLAS.
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
        /// <param name="nrhs">
        /// [in] NRHS is INTEGER.
        /// The number of right hand sides, i.e., the number of columns
        /// of the matrix B.  NRHS &gt;= 0.
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
        /// On exit, L is stored below (or above) the subdiagonal blocks,
        /// when UPLO  is &#39;L&#39; (or &#39;U&#39;).
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,N).
        /// </param>
        /// <param name="tb">
        /// [out] TB is DOUBLE PRECISION array, dimension (MAX(1,LTB)).
        /// On exit, details of the LU factorization of the band matrix.
        /// </param>
        /// <param name="ltb">
        /// [in] LTB is INTEGER.
        /// The size of the array TB. LTB &gt;= MAX(1,4*N), internally
        /// used to select NB such that LTB &gt;= (3*NB+1)*N.
        /// 
        /// If LTB = -1, then a workspace query is assumed; the
        /// routine only calculates the optimal size of LTB,
        /// returns this value as the first entry of TB, and
        /// no error message related to LTB is issued by XERBLA.
        /// </param>
        /// <param name="ipiv">
        /// [out] IPIV is INTEGER array, dimension (N).
        /// On exit, it contains the details of the interchanges, i.e.,
        /// the row and column k of A were interchanged with the
        /// row and column IPIV(k).
        /// </param>
        /// <param name="ipiv2">
        /// [out] IPIV2 is INTEGER array, dimension (N).
        /// On exit, it contains the details of the interchanges, i.e.,
        /// the row and column k of T were interchanged with the
        /// row and column IPIV(k).
        /// </param>
        /// <param name="b">
        /// [in,out] B is DOUBLE PRECISION array, dimension (LDB,NRHS).
        /// On entry, the right hand side matrix B.
        /// On exit, the solution matrix X.
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// The leading dimension of the array B.  LDB &gt;= max(1,N).
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value.
        /// &gt; 0:  if INFO = i, band LU factorization failed on i-th column
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dsysv_aa_2stage", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo DsysvAa2stage(
            MatrixLayout matrixLayout,
            char uplo,
            int n,
            int nrhs,
            double* a,
            int lda,
            double* tb,
            int ltb,
            int* ipiv,
            int* ipiv2,
            double* b,
            int ldb);
    }
}
