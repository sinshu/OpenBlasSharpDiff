using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DSYRFS improves the computed solution to a system of linear
        /// equations when the coefficient matrix is symmetric indefinite, and
        /// provides error bounds and backward error estimates for the solution.
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
        /// of the matrices B and X.  NRHS &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in] A is DOUBLE PRECISION array, dimension (LDA,N).
        /// The symmetric matrix A.  If UPLO = &#39;U&#39;, the leading N-by-N
        /// upper triangular part of A contains the upper triangular part
        /// of the matrix A, and the strictly lower triangular part of A
        /// is not referenced.  If UPLO = &#39;L&#39;, the leading N-by-N lower
        /// triangular part of A contains the lower triangular part of
        /// the matrix A, and the strictly upper triangular part of A is
        /// not referenced.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,N).
        /// </param>
        /// <param name="af">
        /// [in] AF is DOUBLE PRECISION array, dimension (LDAF,N).
        /// The factored form of the matrix A.  AF contains the block
        /// diagonal matrix D and the multipliers used to obtain the
        /// factor U or L from the factorization A = U*D*U**T or
        /// A = L*D*L**T as computed by DSYTRF.
        /// </param>
        /// <param name="ldaf">
        /// [in] LDAF is INTEGER.
        /// The leading dimension of the array AF.  LDAF &gt;= max(1,N).
        /// </param>
        /// <param name="ipiv">
        /// [in] IPIV is INTEGER array, dimension (N).
        /// Details of the interchanges and the block structure of D
        /// as determined by DSYTRF.
        /// </param>
        /// <param name="b">
        /// [in] B is DOUBLE PRECISION array, dimension (LDB,NRHS).
        /// The right hand side matrix B.
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// The leading dimension of the array B.  LDB &gt;= max(1,N).
        /// </param>
        /// <param name="x">
        /// [in,out] X is DOUBLE PRECISION array, dimension (LDX,NRHS).
        /// On entry, the solution matrix X, as computed by DSYTRS.
        /// On exit, the improved solution matrix X.
        /// </param>
        /// <param name="ldx">
        /// [in] LDX is INTEGER.
        /// The leading dimension of the array X.  LDX &gt;= max(1,N).
        /// </param>
        /// <param name="ferr">
        /// [out] FERR is DOUBLE PRECISION array, dimension (NRHS).
        /// The estimated forward error bound for each solution vector
        /// X(j) (the j-th column of the solution matrix X).
        /// If XTRUE is the true solution corresponding to X(j), FERR(j)
        /// is an estimated upper bound for the magnitude of the largest
        /// element in (X(j) - XTRUE) divided by the magnitude of the
        /// largest element in X(j).  The estimate is as reliable as
        /// the estimate for RCOND, and is almost always a slight
        /// overestimate of the true error.
        /// </param>
        /// <param name="berr">
        /// [out] BERR is DOUBLE PRECISION array, dimension (NRHS).
        /// The componentwise relative backward error of each solution
        /// vector X(j) (i.e., the smallest relative change in
        /// any element of A or B that makes X(j) an exact solution).
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dsyrfs", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dsyrfs(
            MatrixLayout matrixLayout,
            char uplo,
            int n,
            int nrhs,
            double* a,
            int lda,
            double* af,
            int ldaf,
            int* ipiv,
            double* b,
            int ldb,
            double* x,
            int ldx,
            double* ferr,
            double* berr);
    }
}
