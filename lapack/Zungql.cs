using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// ZUNGQL generates an M-by-N complex matrix Q with orthonormal columns,
        /// which is defined as the last N columns of a product of K elementary
        /// reflectors of order M
        /// </para>
        /// <para>
        ///       Q  =  H(k) . . . H(2) H(1)
        /// </para>
        /// <para>
        /// as returned by ZGEQLF.
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
        /// The number of columns of the matrix Q. M &gt;= N &gt;= 0.
        /// </param>
        /// <param name="k">
        /// [in] K is INTEGER.
        /// The number of elementary reflectors whose product defines the
        /// matrix Q. N &gt;= K &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in,out] A is COMPLEX*16 array, dimension (LDA,N).
        /// On entry, the (n-k+i)-th column must contain the vector which
        /// defines the elementary reflector H(i), for i = 1,2,...,k, as
        /// returned by ZGEQLF in the last k columns of its array
        /// argument A.
        /// On exit, the M-by-N matrix Q.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The first dimension of the array A. LDA &gt;= max(1,M).
        /// </param>
        /// <param name="tau">
        /// [in] TAU is COMPLEX*16 array, dimension (K).
        /// TAU(i) must contain the scalar factor of the elementary
        /// reflector H(i), as returned by ZGEQLF.
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument has an illegal value
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_zungql", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Zungql(
            MatrixLayout matrixLayout,
            int m,
            int n,
            int k,
            Complex* a,
            int lda,
            Complex* tau);
    }
}
