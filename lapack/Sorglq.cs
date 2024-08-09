using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// SORGLQ generates an M-by-N real matrix Q with orthonormal rows,
        /// which is defined as the first M rows of a product of K elementary
        /// reflectors of order N
        /// </para>
        /// <para>
        ///       Q  =  H(k) . . . H(2) H(1)
        /// </para>
        /// <para>
        /// as returned by SGELQF.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="m">
        /// [in] M is INTEGER.
        /// The number of rows of the matrix Q. M &gt;= 0.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The number of columns of the matrix Q. N &gt;= M.
        /// </param>
        /// <param name="k">
        /// [in] K is INTEGER.
        /// The number of elementary reflectors whose product defines the
        /// matrix Q. M &gt;= K &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in,out] A is REAL array, dimension (LDA,N).
        /// On entry, the i-th row must contain the vector which defines
        /// the elementary reflector H(i), for i = 1,2,...,k, as returned
        /// by SGELQF in the first k rows of its array argument A.
        /// On exit, the M-by-N matrix Q.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The first dimension of the array A. LDA &gt;= max(1,M).
        /// </param>
        /// <param name="tau">
        /// [in] TAU is REAL array, dimension (K).
        /// TAU(i) must contain the scalar factor of the elementary
        /// reflector H(i), as returned by SGELQF.
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument has an illegal value
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_sorglq", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Sorglq(
            MatrixLayout matrixLayout,
            int m,
            int n,
            int k,
            float* a,
            int lda,
            float* tau);
    }
}
