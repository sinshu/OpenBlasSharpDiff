using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DOPMTR overwrites the general real M-by-N matrix C with
        /// </para>
        /// <para>
        ///                 SIDE = &#39;L&#39;     SIDE = &#39;R&#39;
        /// TRANS = &#39;N&#39;:      Q * C          C * Q
        /// TRANS = &#39;T&#39;:      Q**T * C       C * Q**T
        /// </para>
        /// <para>
        /// where Q is a real orthogonal matrix of order nq, with nq = m if
        /// SIDE = &#39;L&#39; and nq = n if SIDE = &#39;R&#39;. Q is defined as the product of
        /// nq-1 elementary reflectors, as returned by DSPTRD using packed
        /// storage:
        /// </para>
        /// <para>
        /// if UPLO = &#39;U&#39;, Q = H(nq-1) . . . H(2) H(1);
        /// </para>
        /// <para>
        /// if UPLO = &#39;L&#39;, Q = H(1) H(2) . . . H(nq-1).
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="side">
        /// [in] SIDE is CHARACTER*1.
        /// = &#39;L&#39;: apply Q or Q**T from the Left;
        /// = &#39;R&#39;: apply Q or Q**T from the Right.
        /// </param>
        /// <param name="uplo">
        /// [in] UPLO is CHARACTER*1.
        /// = &#39;U&#39;: Upper triangular packed storage used in previous
        /// call to DSPTRD;
        /// = &#39;L&#39;: Lower triangular packed storage used in previous
        /// call to DSPTRD.
        /// </param>
        /// <param name="trans">
        /// [in] TRANS is CHARACTER*1.
        /// = &#39;N&#39;:  No transpose, apply Q;
        /// = &#39;T&#39;:  Transpose, apply Q**T.
        /// </param>
        /// <param name="m">
        /// [in] M is INTEGER.
        /// The number of rows of the matrix C. M &gt;= 0.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The number of columns of the matrix C. N &gt;= 0.
        /// </param>
        /// <param name="ap">
        /// [in] AP is DOUBLE PRECISION array, dimension.
        /// (M*(M+1)/2) if SIDE = &#39;L&#39;
        /// (N*(N+1)/2) if SIDE = &#39;R&#39;
        /// The vectors which define the elementary reflectors, as
        /// returned by DSPTRD.  AP is modified by the routine but
        /// restored on exit.
        /// </param>
        /// <param name="tau">
        /// [in] TAU is DOUBLE PRECISION array, dimension (M-1) if SIDE = &#39;L&#39;.
        /// or (N-1) if SIDE = &#39;R&#39;
        /// TAU(i) must contain the scalar factor of the elementary
        /// reflector H(i), as returned by DSPTRD.
        /// </param>
        /// <param name="c">
        /// [in,out] C is DOUBLE PRECISION array, dimension (LDC,N).
        /// On entry, the M-by-N matrix C.
        /// On exit, C is overwritten by Q*C or Q**T*C or C*Q**T or C*Q.
        /// </param>
        /// <param name="ldc">
        /// [in] LDC is INTEGER.
        /// The leading dimension of the array C. LDC &gt;= max(1,M).
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dopmtr", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dopmtr(
            MatrixLayout matrixLayout,
            char side,
            char uplo,
            char trans,
            int m,
            int n,
            double* ap,
            double* tau,
            double* c,
            int ldc);
    }
}
