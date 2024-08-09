using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// ZGETRF2 computes an LU factorization of a general M-by-N matrix A
        /// using partial pivoting with row interchanges.
        /// </para>
        /// <para>
        /// The factorization has the form
        ///    A = P * L * U
        /// where P is a permutation matrix, L is lower triangular with unit
        /// diagonal elements (lower trapezoidal if m &gt; n), and U is upper
        /// triangular (upper trapezoidal if m &lt; n).
        /// </para>
        /// <para>
        /// This is the recursive version of the algorithm. It divides
        /// the matrix into four submatrices:
        /// </para>
        /// <para>
        ///        [  A11 | A12  ]  where A11 is n1 by n1 and A22 is n2 by n2
        ///    A = [ -----|----- ]  with n1 = min(m,n)/2
        ///        [  A21 | A22  ]       n2 = n-n1
        /// </para>
        /// <para>
        ///                                       [ A11 ]
        /// The subroutine calls itself to factor [ --- ],
        ///                                       [ A12 ]
        ///                 [ A12 ]
        /// do the swaps on [ --- ], solve A12, update A22,
        ///                 [ A22 ]
        /// </para>
        /// <para>
        /// then calls itself to factor A22 and do the swaps on A21.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="m">
        /// [in] M is INTEGER.
        /// The number of rows of the matrix A.  M &gt;= 0.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The number of columns of the matrix A.  N &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in,out] A is COMPLEX*16 array, dimension (LDA,N).
        /// On entry, the M-by-N matrix to be factored.
        /// On exit, the factors L and U from the factorization
        /// A = P*L*U; the unit diagonal elements of L are not stored.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,M).
        /// </param>
        /// <param name="ipiv">
        /// [out] IPIV is INTEGER array, dimension (min(M,N)).
        /// The pivot indices; for 1 &lt;= i &lt;= min(M,N), row i of the
        /// matrix was interchanged with row IPIV(i).
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// &gt; 0:  if INFO = i, U(i,i) is exactly zero. The factorization
        /// has been completed, but the factor U is exactly
        /// singular, and division by zero will occur if it is used
        /// to solve a system of equations.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_zgetrf2", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Zgetrf2(
            MatrixLayout matrixLayout,
            int m,
            int n,
            Complex* a,
            int lda,
            int* ipiv);
    }
}
