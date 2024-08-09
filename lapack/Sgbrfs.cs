using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// SGBRFS improves the computed solution to a system of linear
        /// equations when the coefficient matrix is banded, and provides
        /// error bounds and backward error estimates for the solution.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="trans">
        /// [in] TRANS is CHARACTER*1.
        /// Specifies the form of the system of equations:
        /// = &#39;N&#39;:  A * X = B     (No transpose)
        /// = &#39;T&#39;:  A**T * X = B  (Transpose)
        /// = &#39;C&#39;:  A**H * X = B  (Conjugate transpose = Transpose)
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrix A.  N &gt;= 0.
        /// </param>
        /// <param name="kl">
        /// [in] KL is INTEGER.
        /// The number of subdiagonals within the band of A.  KL &gt;= 0.
        /// </param>
        /// <param name="ku">
        /// [in] KU is INTEGER.
        /// The number of superdiagonals within the band of A.  KU &gt;= 0.
        /// </param>
        /// <param name="nrhs">
        /// [in] NRHS is INTEGER.
        /// The number of right hand sides, i.e., the number of columns
        /// of the matrices B and X.  NRHS &gt;= 0.
        /// </param>
        /// <param name="ab">
        /// [in] AB is REAL array, dimension (LDAB,N).
        /// The original band matrix A, stored in rows 1 to KL+KU+1.
        /// The j-th column of A is stored in the j-th column of the
        /// array AB as follows:
        /// AB(ku+1+i-j,j) = A(i,j) for max(1,j-ku)&lt;=i&lt;=min(n,j+kl).
        /// </param>
        /// <param name="ldab">
        /// [in] LDAB is INTEGER.
        /// The leading dimension of the array AB.  LDAB &gt;= KL+KU+1.
        /// </param>
        /// <param name="afb">
        /// [in] AFB is REAL array, dimension (LDAFB,N).
        /// Details of the LU factorization of the band matrix A, as
        /// computed by SGBTRF.  U is stored as an upper triangular band
        /// matrix with KL+KU superdiagonals in rows 1 to KL+KU+1, and
        /// the multipliers used during the factorization are stored in
        /// rows KL+KU+2 to 2*KL+KU+1.
        /// </param>
        /// <param name="ldafb">
        /// [in] LDAFB is INTEGER.
        /// The leading dimension of the array AFB.  LDAFB &gt;= 2*KL*KU+1.
        /// </param>
        /// <param name="ipiv">
        /// [in] IPIV is INTEGER array, dimension (N).
        /// The pivot indices from SGBTRF; for 1&lt;=i&lt;=N, row i of the
        /// matrix was interchanged with row IPIV(i).
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
        /// On entry, the solution matrix X, as computed by SGBTRS.
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
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_sgbrfs", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Sgbrfs(
            MatrixLayout matrixLayout,
            char trans,
            int n,
            int kl,
            int ku,
            int nrhs,
            float* ab,
            int ldab,
            float* afb,
            int ldafb,
            int* ipiv,
            float* b,
            int ldb,
            float* x,
            int ldx,
            float* ferr,
            float* berr);
    }
}
