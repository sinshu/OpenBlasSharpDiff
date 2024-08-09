using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        ///     SGEMLQ overwrites the general real M-by-N matrix C with
        /// </para>
        /// <para>
        ///                    SIDE = &#39;L&#39;     SIDE = &#39;R&#39;
        ///    TRANS = &#39;N&#39;:      Q * C          C * Q
        ///    TRANS = &#39;T&#39;:      Q**T * C       C * Q**T
        ///    where Q is a real orthogonal matrix defined as the product
        ///    of blocked elementary reflectors computed by short wide LQ
        ///    factorization (SGELQ)
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
        /// The number of rows of the matrix A.  M &gt;=0.
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
        /// [in] A is REAL array, dimension.
        /// (LDA,M) if SIDE = &#39;L&#39;,
        /// (LDA,N) if SIDE = &#39;R&#39;
        /// Part of the data structure to represent Q as returned by DGELQ.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A. LDA &gt;= max(1,K).
        /// </param>
        /// <param name="t">
        /// [in] T is REAL array, dimension (MAX(5,TSIZE)).
        /// Part of the data structure to represent Q as returned by SGELQ.
        /// </param>
        /// <param name="tsize">
        /// [in] TSIZE is INTEGER.
        /// The dimension of the array T. TSIZE &gt;= 5.
        /// </param>
        /// <param name="c">
        /// [in,out] C is REAL array, dimension (LDC,N).
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
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_sgemlq", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Sgemlq(
            MatrixLayout matrixLayout,
            char side,
            char trans,
            int m,
            int n,
            int k,
            float* a,
            int lda,
            float* t,
            int tsize,
            float* c,
            int ldc);
    }
}
