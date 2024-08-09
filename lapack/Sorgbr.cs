using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// SORGBR generates one of the real orthogonal matrices Q or P**T
        /// determined by SGEBRD when reducing a real matrix A to bidiagonal
        /// form: A = Q * B * P**T.  Q and P**T are defined as products of
        /// elementary reflectors H(i) or G(i) respectively.
        /// </para>
        /// <para>
        /// If VECT = &#39;Q&#39;, A is assumed to have been an M-by-K matrix, and Q
        /// is of order M:
        /// if m &gt;= k, Q = H(1) H(2) . . . H(k) and SORGBR returns the first n
        /// columns of Q, where m &gt;= n &gt;= k;
        /// if m &lt; k, Q = H(1) H(2) . . . H(m-1) and SORGBR returns Q as an
        /// M-by-M matrix.
        /// </para>
        /// <para>
        /// If VECT = &#39;P&#39;, A is assumed to have been a K-by-N matrix, and P**T
        /// is of order N:
        /// if k &lt; n, P**T = G(k) . . . G(2) G(1) and SORGBR returns the first m
        /// rows of P**T, where n &gt;= m &gt;= k;
        /// if k &gt;= n, P**T = G(n-1) . . . G(2) G(1) and SORGBR returns P**T as
        /// an N-by-N matrix.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="vect">
        /// [in] VECT is CHARACTER*1.
        /// Specifies whether the matrix Q or the matrix P**T is
        /// required, as defined in the transformation applied by SGEBRD:
        /// = &#39;Q&#39;:  generate Q;
        /// = &#39;P&#39;:  generate P**T.
        /// </param>
        /// <param name="m">
        /// [in] M is INTEGER.
        /// The number of rows of the matrix Q or P**T to be returned.
        /// M &gt;= 0.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The number of columns of the matrix Q or P**T to be returned.
        /// N &gt;= 0.
        /// If VECT = &#39;Q&#39;, M &gt;= N &gt;= min(M,K);
        /// if VECT = &#39;P&#39;, N &gt;= M &gt;= min(N,K).
        /// </param>
        /// <param name="k">
        /// [in] K is INTEGER.
        /// If VECT = &#39;Q&#39;, the number of columns in the original M-by-K
        /// matrix reduced by SGEBRD.
        /// If VECT = &#39;P&#39;, the number of rows in the original K-by-N
        /// matrix reduced by SGEBRD.
        /// K &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in,out] A is REAL array, dimension (LDA,N).
        /// On entry, the vectors which define the elementary reflectors,
        /// as returned by SGEBRD.
        /// On exit, the M-by-N matrix Q or P**T.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A. LDA &gt;= max(1,M).
        /// </param>
        /// <param name="tau">
        /// [in] TAU is REAL array, dimension.
        /// (min(M,K)) if VECT = &#39;Q&#39;
        /// (min(N,K)) if VECT = &#39;P&#39;
        /// TAU(i) must contain the scalar factor of the elementary
        /// reflector H(i) or G(i), which determines Q or P**T, as
        /// returned by SGEBRD in its array argument TAUQ or TAUP.
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_sorgbr", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Sorgbr(
            MatrixLayout matrixLayout,
            char vect,
            int m,
            int n,
            int k,
            float* a,
            int lda,
            float* tau);
    }
}
