using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// CLASWP performs a series of row interchanges on the matrix A.
        /// One row interchange is initiated for each of rows K1 through K2 of A.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The number of columns of the matrix A.
        /// </param>
        /// <param name="a">
        /// [in,out] A is COMPLEX array, dimension (LDA,N).
        /// On entry, the matrix of column dimension N to which the row
        /// interchanges will be applied.
        /// On exit, the permuted matrix.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.
        /// </param>
        /// <param name="k1">
        /// [in] K1 is INTEGER.
        /// The first element of IPIV for which a row interchange will
        /// be done.
        /// </param>
        /// <param name="k2">
        /// [in] K2 is INTEGER.
        /// (K2-K1+1) is the number of elements of IPIV for which a row
        /// interchange will be done.
        /// </param>
        /// <param name="ipiv">
        /// [in] IPIV is INTEGER array, dimension (K1+(K2-K1)*abs(INCX)).
        /// The vector of pivot indices. Only the elements in positions
        /// K1 through K1+(K2-K1)*abs(INCX) of IPIV are accessed.
        /// IPIV(K1+(K-K1)*abs(INCX)) = L implies rows K and L are to be
        /// interchanged.
        /// </param>
        /// <param name="incx">
        /// [in] INCX is INTEGER.
        /// The increment between successive values of IPIV. If INCX
        /// is negative, the pivots are applied in reverse order.
        /// </param>
        /// <remarks>
        /// <para>
        ///  Modified by
        ///   R. C. Whaley, Computer Science Dept., Univ. of Tenn., Knoxville, USA
        /// </para>
        /// </remarks>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_claswp", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Claswp(
            MatrixLayout matrixLayout,
            int n,
            Complex32* a,
            int lda,
            int k1,
            int k2,
            int* ipiv,
            int incx);
    }
}
