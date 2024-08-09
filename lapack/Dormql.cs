using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DORMQL overwrites the general real M-by-N matrix C with
        /// </para>
        /// <para>
        ///                 SIDE = &#39;L&#39;     SIDE = &#39;R&#39;
        /// TRANS = &#39;N&#39;:      Q * C          C * Q
        /// TRANS = &#39;T&#39;:      Q**T * C       C * Q**T
        /// </para>
        /// <para>
        /// where Q is a real orthogonal matrix defined as the product of k
        /// elementary reflectors
        /// </para>
        /// <para>
        ///       Q = H(k) . . . H(2) H(1)
        /// </para>
        /// <para>
        /// as returned by DGEQLF. Q is of order M if SIDE = &#39;L&#39; and of order N
        /// if SIDE = &#39;R&#39;.
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
        /// <param name="k">
        /// [in] K is INTEGER.
        /// The number of elementary reflectors whose product defines
        /// the matrix Q.
        /// If SIDE = &#39;L&#39;, M &gt;= K &gt;= 0;
        /// if SIDE = &#39;R&#39;, N &gt;= K &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in] A is DOUBLE PRECISION array, dimension (LDA,K).
        /// The i-th column must contain the vector which defines the
        /// elementary reflector H(i), for i = 1,2,...,k, as returned by
        /// DGEQLF in the last k columns of its array argument A.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.
        /// If SIDE = &#39;L&#39;, LDA &gt;= max(1,M);
        /// if SIDE = &#39;R&#39;, LDA &gt;= max(1,N).
        /// </param>
        /// <param name="tau">
        /// [in] TAU is DOUBLE PRECISION array, dimension (K).
        /// TAU(i) must contain the scalar factor of the elementary
        /// reflector H(i), as returned by DGEQLF.
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
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dormql", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dormql(
            MatrixLayout matrixLayout,
            char side,
            char trans,
            int m,
            int n,
            int k,
            double* a,
            int lda,
            double* tau,
            double* c,
            int ldc);
    }
}
