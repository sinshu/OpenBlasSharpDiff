using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// ZGTTRS solves one of the systems of equations
        ///    A * X = B,  A**T * X = B,  or  A**H * X = B,
        /// with a tridiagonal matrix A using the LU factorization computed
        /// by ZGTTRF.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="trans">
        /// [in] TRANS is CHARACTER*1.
        /// Specifies the form of the system of equations.
        /// = &#39;N&#39;:  A * X = B     (No transpose)
        /// = &#39;T&#39;:  A**T * X = B  (Transpose)
        /// = &#39;C&#39;:  A**H * X = B  (Conjugate transpose)
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrix A.
        /// </param>
        /// <param name="nrhs">
        /// [in] NRHS is INTEGER.
        /// The number of right hand sides, i.e., the number of columns
        /// of the matrix B.  NRHS &gt;= 0.
        /// </param>
        /// <param name="dl">
        /// [in] DL is COMPLEX*16 array, dimension (N-1).
        /// The (n-1) multipliers that define the matrix L from the
        /// LU factorization of A.
        /// </param>
        /// <param name="d">
        /// [in] D is COMPLEX*16 array, dimension (N).
        /// The n diagonal elements of the upper triangular matrix U from
        /// the LU factorization of A.
        /// </param>
        /// <param name="du">
        /// [in] DU is COMPLEX*16 array, dimension (N-1).
        /// The (n-1) elements of the first super-diagonal of U.
        /// </param>
        /// <param name="du2">
        /// [in] DU2 is COMPLEX*16 array, dimension (N-2).
        /// The (n-2) elements of the second super-diagonal of U.
        /// </param>
        /// <param name="ipiv">
        /// [in] IPIV is INTEGER array, dimension (N).
        /// The pivot indices; for 1 &lt;= i &lt;= n, row i of the matrix was
        /// interchanged with row IPIV(i).  IPIV(i) will always be either
        /// i or i+1; IPIV(i) = i indicates a row interchange was not
        /// required.
        /// </param>
        /// <param name="b">
        /// [in,out] B is COMPLEX*16 array, dimension (LDB,NRHS).
        /// On entry, the matrix of right hand side vectors B.
        /// On exit, B is overwritten by the solution vectors X.
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// The leading dimension of the array B.  LDB &gt;= max(1,N).
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -k, the k-th argument had an illegal value
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_zgttrs", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Zgttrs(
            MatrixLayout matrixLayout,
            char trans,
            int n,
            int nrhs,
            Complex* dl,
            Complex* d,
            Complex* du,
            Complex* du2,
            int* ipiv,
            Complex* b,
            int ldb);
    }
}
