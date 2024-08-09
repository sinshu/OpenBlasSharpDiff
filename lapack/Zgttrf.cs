using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// ZGTTRF computes an LU factorization of a complex tridiagonal matrix A
        /// using elimination with partial pivoting and row interchanges.
        /// </para>
        /// <para>
        /// The factorization has the form
        ///    A = L * U
        /// where L is a product of permutation and unit lower bidiagonal
        /// matrices and U is upper triangular with nonzeros in only the main
        /// diagonal and first two superdiagonals.
        /// </para>
        /// </summary>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrix A.
        /// </param>
        /// <param name="dl">
        /// [in,out] DL is COMPLEX*16 array, dimension (N-1).
        /// On entry, DL must contain the (n-1) sub-diagonal elements of
        /// A.
        /// 
        /// On exit, DL is overwritten by the (n-1) multipliers that
        /// define the matrix L from the LU factorization of A.
        /// </param>
        /// <param name="d">
        /// [in,out] D is COMPLEX*16 array, dimension (N).
        /// On entry, D must contain the diagonal elements of A.
        /// 
        /// On exit, D is overwritten by the n diagonal elements of the
        /// upper triangular matrix U from the LU factorization of A.
        /// </param>
        /// <param name="du">
        /// [in,out] DU is COMPLEX*16 array, dimension (N-1).
        /// On entry, DU must contain the (n-1) super-diagonal elements
        /// of A.
        /// 
        /// On exit, DU is overwritten by the (n-1) elements of the first
        /// super-diagonal of U.
        /// </param>
        /// <param name="du2">
        /// [out] DU2 is COMPLEX*16 array, dimension (N-2).
        /// On exit, DU2 is overwritten by the (n-2) elements of the
        /// second super-diagonal of U.
        /// </param>
        /// <param name="ipiv">
        /// [out] IPIV is INTEGER array, dimension (N).
        /// The pivot indices; for 1 &lt;= i &lt;= n, row i of the matrix was
        /// interchanged with row IPIV(i).  IPIV(i) will always be either
        /// i or i+1; IPIV(i) = i indicates a row interchange was not
        /// required.
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -k, the k-th argument had an illegal value
        /// &gt; 0:  if INFO = k, U(k,k) is exactly zero. The factorization
        /// has been completed, but the factor U is exactly
        /// singular, and division by zero will occur if it is used
        /// to solve a system of equations.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_zgttrf", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Zgttrf(
            int n,
            Complex* dl,
            Complex* d,
            Complex* du,
            Complex* du2,
            int* ipiv);
    }
}
