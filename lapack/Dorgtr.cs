using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DORGTR generates a real orthogonal matrix Q which is defined as the
        /// product of n-1 elementary reflectors of order N, as returned by
        /// DSYTRD:
        /// </para>
        /// <para>
        /// if UPLO = &#39;U&#39;, Q = H(n-1) . . . H(2) H(1),
        /// </para>
        /// <para>
        /// if UPLO = &#39;L&#39;, Q = H(1) H(2) . . . H(n-1).
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="uplo">
        /// [in] UPLO is CHARACTER*1.
        /// = &#39;U&#39;: Upper triangle of A contains elementary reflectors
        /// from DSYTRD;
        /// = &#39;L&#39;: Lower triangle of A contains elementary reflectors
        /// from DSYTRD.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrix Q. N &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in,out] A is DOUBLE PRECISION array, dimension (LDA,N).
        /// On entry, the vectors which define the elementary reflectors,
        /// as returned by DSYTRD.
        /// On exit, the N-by-N orthogonal matrix Q.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A. LDA &gt;= max(1,N).
        /// </param>
        /// <param name="tau">
        /// [in] TAU is DOUBLE PRECISION array, dimension (N-1).
        /// TAU(i) must contain the scalar factor of the elementary
        /// reflector H(i), as returned by DSYTRD.
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dorgtr", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dorgtr(
            MatrixLayout matrixLayout,
            char uplo,
            int n,
            double* a,
            int lda,
            double* tau);
    }
}
