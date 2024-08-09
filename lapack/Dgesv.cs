using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DGESV computes the solution to a real system of linear equations
        ///    A * X = B,
        /// where A is an N-by-N matrix and X and B are N-by-NRHS matrices.
        /// </para>
        /// <para>
        /// The LU decomposition with partial pivoting and row interchanges is
        /// used to factor A as
        ///    A = P * L * U,
        /// where P is a permutation matrix, L is unit lower triangular, and U is
        /// upper triangular.  The factored form of A is then used to solve the
        /// system of equations A * X = B.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
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
        /// [in,out] A is DOUBLE PRECISION array, dimension (LDA,N).
        /// On entry, the N-by-N coefficient matrix A.
        /// On exit, the factors L and U from the factorization
        /// A = P*L*U; the unit diagonal elements of L are not stored.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,N).
        /// </param>
        /// <param name="ipiv">
        /// [out] IPIV is INTEGER array, dimension (N).
        /// The pivot indices that define the permutation matrix P;
        /// row i of the matrix was interchanged with row IPIV(i).
        /// </param>
        /// <param name="b">
        /// [in,out] B is DOUBLE PRECISION array, dimension (LDB,NRHS).
        /// On entry, the N-by-NRHS matrix of right hand side matrix B.
        /// On exit, if INFO = 0, the N-by-NRHS solution matrix X.
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// The leading dimension of the array B.  LDB &gt;= max(1,N).
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// &gt; 0:  if INFO = i, U(i,i) is exactly zero.  The factorization
        /// has been completed, but the factor U is exactly
        /// singular, so the solution could not be computed.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dgesv", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dgesv(
            MatrixLayout matrixLayout,
            int n,
            int nrhs,
            double* a,
            int lda,
            int* ipiv,
            double* b,
            int ldb);
    }
}
