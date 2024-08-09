using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// ZGTSV  solves the equation
        /// </para>
        /// <para>
        ///    A*X = B,
        /// </para>
        /// <para>
        /// where A is an N-by-N tridiagonal matrix, by Gaussian elimination with
        /// partial pivoting.
        /// </para>
        /// <para>
        /// Note that the equation  A**T *X = B  may be solved by interchanging the
        /// order of the arguments DU and DL.
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
        /// <param name="dl">
        /// [in,out] DL is COMPLEX*16 array, dimension (N-1).
        /// On entry, DL must contain the (n-1) subdiagonal elements of
        /// A.
        /// On exit, DL is overwritten by the (n-2) elements of the
        /// second superdiagonal of the upper triangular matrix U from
        /// the LU factorization of A, in DL(1), ..., DL(n-2).
        /// </param>
        /// <param name="d">
        /// [in,out] D is COMPLEX*16 array, dimension (N).
        /// On entry, D must contain the diagonal elements of A.
        /// On exit, D is overwritten by the n diagonal elements of U.
        /// </param>
        /// <param name="du">
        /// [in,out] DU is COMPLEX*16 array, dimension (N-1).
        /// On entry, DU must contain the (n-1) superdiagonal elements
        /// of A.
        /// On exit, DU is overwritten by the (n-1) elements of the first
        /// superdiagonal of U.
        /// </param>
        /// <param name="b">
        /// [in,out] B is COMPLEX*16 array, dimension (LDB,NRHS).
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
        /// &gt; 0:  if INFO = i, U(i,i) is exactly zero, and the solution
        /// has not been computed.  The factorization has not been
        /// completed unless i = N.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_zgtsv", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Zgtsv(
            MatrixLayout matrixLayout,
            int n,
            int nrhs,
            Complex* dl,
            Complex* d,
            Complex* du,
            Complex* b,
            int ldb);
    }
}
