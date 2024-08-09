using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// SPTSV computes the solution to a real system of linear equations
        /// A*X = B, where A is an N-by-N symmetric positive definite tridiagonal
        /// matrix, and X and B are N-by-NRHS matrices.
        /// </para>
        /// <para>
        /// A is factored as A = L*D*L**T, and the factored form of A is then
        /// used to solve the system of equations.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
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
        /// [in,out] D is REAL array, dimension (N).
        /// On entry, the n diagonal elements of the tridiagonal matrix
        /// A.  On exit, the n diagonal elements of the diagonal matrix
        /// D from the factorization A = L*D*L**T.
        /// </param>
        /// <param name="e">
        /// [in,out] E is REAL array, dimension (N-1).
        /// On entry, the (n-1) subdiagonal elements of the tridiagonal
        /// matrix A.  On exit, the (n-1) subdiagonal elements of the
        /// unit bidiagonal factor L from the L*D*L**T factorization of
        /// A.  (E can also be regarded as the superdiagonal of the unit
        /// bidiagonal factor U from the U**T*D*U factorization of A.)
        /// </param>
        /// <param name="b">
        /// [in,out] B is REAL array, dimension (LDB,NRHS).
        /// On entry, the N-by-NRHS right hand side matrix B.
        /// On exit, if INFO = 0, the N-by-NRHS solution matrix X.
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// The leading dimension of the array B.  LDB &gt;= max(1,N).
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// &gt; 0:  if INFO = i, the leading principal minor of order i
        /// is not positive, and the solution has not been
        /// computed.  The factorization has not been completed
        /// unless i = N.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_sptsv", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Sptsv(
            MatrixLayout matrixLayout,
            int n,
            int nrhs,
            float* d,
            float* e,
            float* b,
            int ldb);
    }
}
