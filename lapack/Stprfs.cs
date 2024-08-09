using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// STPRFS provides error bounds and backward error estimates for the
        /// solution to a system of linear equations with a triangular packed
        /// coefficient matrix.
        /// </para>
        /// <para>
        /// The solution matrix X must be computed by STPTRS or some other
        /// means before entering this routine.  STPRFS does not do iterative
        /// refinement because doing so cannot improve the backward error.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="uplo">
        /// [in] UPLO is CHARACTER*1.
        /// = &#39;U&#39;:  A is upper triangular;
        /// = &#39;L&#39;:  A is lower triangular.
        /// </param>
        /// <param name="trans">
        /// [in] TRANS is CHARACTER*1.
        /// Specifies the form of the system of equations:
        /// = &#39;N&#39;:  A * X = B  (No transpose)
        /// = &#39;T&#39;:  A**T * X = B  (Transpose)
        /// = &#39;C&#39;:  A**H * X = B  (Conjugate transpose = Transpose)
        /// </param>
        /// <param name="diag">
        /// [in] DIAG is CHARACTER*1.
        /// = &#39;N&#39;:  A is non-unit triangular;
        /// = &#39;U&#39;:  A is unit triangular.
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
        /// <param name="ap">
        /// [in] AP is REAL array, dimension (N*(N+1)/2).
        /// The upper or lower triangular matrix A, packed columnwise in
        /// a linear array.  The j-th column of A is stored in the array
        /// AP as follows:
        /// if UPLO = &#39;U&#39;, AP(i + (j-1)*j/2) = A(i,j) for 1&lt;=i&lt;=j;
        /// if UPLO = &#39;L&#39;, AP(i + (j-1)*(2*n-j)/2) = A(i,j) for j&lt;=i&lt;=n.
        /// If DIAG = &#39;U&#39;, the diagonal elements of A are not referenced
        /// and are assumed to be 1.
        /// </param>
        /// <param name="b">
        /// [in] B is REAL array, dimension (LDB,NRHS).
        /// The right hand side matrix B.
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// The leading dimension of the array B.  LDB &gt;= max(1,N).
        /// </param>
        /// <param name="x">
        /// [in] X is REAL array, dimension (LDX,NRHS).
        /// The solution matrix X.
        /// </param>
        /// <param name="ldx">
        /// [in] LDX is INTEGER.
        /// The leading dimension of the array X.  LDX &gt;= max(1,N).
        /// </param>
        /// <param name="ferr">
        /// [out] FERR is REAL array, dimension (NRHS).
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
        /// [out] BERR is REAL array, dimension (NRHS).
        /// The componentwise relative backward error of each solution
        /// vector X(j) (i.e., the smallest relative change in
        /// any element of A or B that makes X(j) an exact solution).
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_stprfs", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Stprfs(
            MatrixLayout matrixLayout,
            char uplo,
            char trans,
            char diag,
            int n,
            int nrhs,
            float* ap,
            float* b,
            int ldb,
            float* x,
            int ldx,
            float* ferr,
            float* berr);
    }
}
