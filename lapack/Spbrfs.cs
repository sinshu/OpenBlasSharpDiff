using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// SPBRFS improves the computed solution to a system of linear
        /// equations when the coefficient matrix is symmetric positive definite
        /// and banded, and provides error bounds and backward error estimates
        /// for the solution.
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
        /// <param name="kd">
        /// [in] KD is INTEGER.
        /// The number of superdiagonals of the matrix A if UPLO = &#39;U&#39;,
        /// or the number of subdiagonals if UPLO = &#39;L&#39;.  KD &gt;= 0.
        /// </param>
        /// <param name="nrhs">
        /// [in] NRHS is INTEGER.
        /// The number of right hand sides, i.e., the number of columns
        /// of the matrices B and X.  NRHS &gt;= 0.
        /// </param>
        /// <param name="ab">
        /// [in] AB is REAL array, dimension (LDAB,N).
        /// The upper or lower triangle of the symmetric band matrix A,
        /// stored in the first KD+1 rows of the array.  The j-th column
        /// of A is stored in the j-th column of the array AB as follows:
        /// if UPLO = &#39;U&#39;, AB(kd+1+i-j,j) = A(i,j) for max(1,j-kd)&lt;=i&lt;=j;
        /// if UPLO = &#39;L&#39;, AB(1+i-j,j)    = A(i,j) for j&lt;=i&lt;=min(n,j+kd).
        /// </param>
        /// <param name="ldab">
        /// [in] LDAB is INTEGER.
        /// The leading dimension of the array AB.  LDAB &gt;= KD+1.
        /// </param>
        /// <param name="afb">
        /// [in] AFB is REAL array, dimension (LDAFB,N).
        /// The triangular factor U or L from the Cholesky factorization
        /// A = U**T*U or A = L*L**T of the band matrix A as computed by
        /// SPBTRF, in the same storage format as A (see AB).
        /// </param>
        /// <param name="ldafb">
        /// [in] LDAFB is INTEGER.
        /// The leading dimension of the array AFB.  LDAFB &gt;= KD+1.
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
        /// [in,out] X is REAL array, dimension (LDX,NRHS).
        /// On entry, the solution matrix X, as computed by SPBTRS.
        /// On exit, the improved solution matrix X.
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
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_spbrfs", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Spbrfs(
            MatrixLayout matrixLayout,
            char uplo,
            int n,
            int kd,
            int nrhs,
            float* ab,
            int ldab,
            float* afb,
            int ldafb,
            float* b,
            int ldb,
            float* x,
            int ldx,
            float* ferr,
            float* berr);
    }
}
