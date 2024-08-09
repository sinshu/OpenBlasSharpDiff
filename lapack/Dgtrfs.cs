using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DGTRFS improves the computed solution to a system of linear
        /// equations when the coefficient matrix is tridiagonal, and provides
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
        /// <param name="nrhs">
        /// [in] NRHS is INTEGER.
        /// The number of right hand sides, i.e., the number of columns
        /// of the matrix B.  NRHS &gt;= 0.
        /// </param>
        /// <param name="dl">
        /// [in] DL is DOUBLE PRECISION array, dimension (N-1).
        /// The (n-1) subdiagonal elements of A.
        /// </param>
        /// <param name="d">
        /// [in] D is DOUBLE PRECISION array, dimension (N).
        /// The diagonal elements of A.
        /// </param>
        /// <param name="du">
        /// [in] DU is DOUBLE PRECISION array, dimension (N-1).
        /// The (n-1) superdiagonal elements of A.
        /// </param>
        /// <param name="dlf">
        /// [in] DLF is DOUBLE PRECISION array, dimension (N-1).
        /// The (n-1) multipliers that define the matrix L from the
        /// LU factorization of A as computed by DGTTRF.
        /// </param>
        /// <param name="df">
        /// [in] DF is DOUBLE PRECISION array, dimension (N).
        /// The n diagonal elements of the upper triangular matrix U from
        /// the LU factorization of A.
        /// </param>
        /// <param name="duf">
        /// [in] DUF is DOUBLE PRECISION array, dimension (N-1).
        /// The (n-1) elements of the first superdiagonal of U.
        /// </param>
        /// <param name="du2">
        /// [in] DU2 is DOUBLE PRECISION array, dimension (N-2).
        /// The (n-2) elements of the second superdiagonal of U.
        /// </param>
        /// <param name="ipiv">
        /// [in] IPIV is INTEGER array, dimension (N).
        /// The pivot indices; for 1 &lt;= i &lt;= n, row i of the matrix was
        /// interchanged with row IPIV(i).  IPIV(i) will always be either
        /// i or i+1; IPIV(i) = i indicates a row interchange was not
        /// required.
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
        /// On entry, the solution matrix X, as computed by DGTTRS.
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
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dgtrfs", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dgtrfs(
            MatrixLayout matrixLayout,
            char trans,
            int n,
            int nrhs,
            double* dl,
            double* d,
            double* du,
            double* dlf,
            double* df,
            double* duf,
            double* du2,
            int* ipiv,
            double* b,
            int ldb,
            double* x,
            int ldx,
            double* ferr,
            double* berr);
    }
}
