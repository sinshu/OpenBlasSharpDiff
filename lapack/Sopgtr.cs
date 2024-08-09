using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// SOPGTR generates a real orthogonal matrix Q which is defined as the
        /// product of n-1 elementary reflectors H(i) of order n, as returned by
        /// SSPTRD using packed storage:
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
        /// = &#39;U&#39;: Upper triangular packed storage used in previous
        /// call to SSPTRD;
        /// = &#39;L&#39;: Lower triangular packed storage used in previous
        /// call to SSPTRD.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrix Q. N &gt;= 0.
        /// </param>
        /// <param name="ap">
        /// [in] AP is REAL array, dimension (N*(N+1)/2).
        /// The vectors which define the elementary reflectors, as
        /// returned by SSPTRD.
        /// </param>
        /// <param name="tau">
        /// [in] TAU is REAL array, dimension (N-1).
        /// TAU(i) must contain the scalar factor of the elementary
        /// reflector H(i), as returned by SSPTRD.
        /// </param>
        /// <param name="q">
        /// [out] Q is REAL array, dimension (LDQ,N).
        /// The N-by-N orthogonal matrix Q.
        /// </param>
        /// <param name="ldq">
        /// [in] LDQ is INTEGER.
        /// The leading dimension of the array Q. LDQ &gt;= max(1,N).
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_sopgtr", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Sopgtr(
            MatrixLayout matrixLayout,
            char uplo,
            int n,
            float* ap,
            float* tau,
            float* q,
            int ldq);
    }
}
