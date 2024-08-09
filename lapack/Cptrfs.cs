using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// CPTRFS improves the computed solution to a system of linear
        /// equations when the coefficient matrix is Hermitian positive definite
        /// and tridiagonal, and provides error bounds and backward error
        /// estimates for the solution.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="uplo">
        /// [in] UPLO is CHARACTER*1.
        /// Specifies whether the superdiagonal or the subdiagonal of the
        /// tridiagonal matrix A is stored and the form of the
        /// factorization:
        /// = &#39;U&#39;:  E is the superdiagonal of A, and A = U**H*D*U;
        /// = &#39;L&#39;:  E is the subdiagonal of A, and A = L*D*L**H.
        /// (The two forms are equivalent if A is real.)
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
        /// <param name="d">
        /// [in] D is REAL array, dimension (N).
        /// The n real diagonal elements of the tridiagonal matrix A.
        /// </param>
        /// <param name="e">
        /// [in] E is COMPLEX array, dimension (N-1).
        /// The (n-1) off-diagonal elements of the tridiagonal matrix A
        /// (see UPLO).
        /// </param>
        /// <param name="df">
        /// [in] DF is REAL array, dimension (N).
        /// The n diagonal elements of the diagonal matrix D from
        /// the factorization computed by CPTTRF.
        /// </param>
        /// <param name="ef">
        /// [in] EF is COMPLEX array, dimension (N-1).
        /// The (n-1) off-diagonal elements of the unit bidiagonal
        /// factor U or L from the factorization computed by CPTTRF
        /// (see UPLO).
        /// </param>
        /// <param name="b">
        /// [in] B is COMPLEX array, dimension (LDB,NRHS).
        /// The right hand side matrix B.
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// The leading dimension of the array B.  LDB &gt;= max(1,N).
        /// </param>
        /// <param name="x">
        /// [in,out] X is COMPLEX array, dimension (LDX,NRHS).
        /// On entry, the solution matrix X, as computed by CPTTRS.
        /// On exit, the improved solution matrix X.
        /// </param>
        /// <param name="ldx">
        /// [in] LDX is INTEGER.
        /// The leading dimension of the array X.  LDX &gt;= max(1,N).
        /// </param>
        /// <param name="ferr">
        /// [out] FERR is REAL array, dimension (NRHS).
        /// The forward error bound for each solution vector
        /// X(j) (the j-th column of the solution matrix X).
        /// If XTRUE is the true solution corresponding to X(j), FERR(j)
        /// is an estimated upper bound for the magnitude of the largest
        /// element in (X(j) - XTRUE) divided by the magnitude of the
        /// largest element in X(j).
        /// </param>
        /// <param name="berr">
        /// [out] BERR is REAL array, dimension (NRHS).
        /// The componentwise relative backward error of each solution
        /// vector X(j) (i.e., the smallest relative change in
        /// any element of A or B that makes X(j) an exact solution).
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_cptrfs", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Cptrfs(
            MatrixLayout matrixLayout,
            char uplo,
            int n,
            int nrhs,
            float* d,
            Complex32* e,
            float* df,
            Complex32* ef,
            Complex32* b,
            int ldb,
            Complex32* x,
            int ldx,
            float* ferr,
            float* berr);
    }
}
