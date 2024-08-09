using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// CGEMQR overwrites the general real M-by-N matrix C with
        /// </para>
        /// <para>
        ///                      SIDE = &#39;L&#39;     SIDE = &#39;R&#39;
        ///      TRANS = &#39;N&#39;:      Q * C          C * Q
        ///      TRANS = &#39;T&#39;:      Q**H * C       C * Q**H
        /// </para>
        /// <para>
        /// where Q is a complex unitary matrix defined as the product
        /// of blocked elementary reflectors computed by tall skinny
        /// QR factorization (CGEQR)
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="side">
        /// [in] SIDE is CHARACTER*1.
        /// = &#39;L&#39;: apply Q or Q**H from the Left;
        /// = &#39;R&#39;: apply Q or Q**H from the Right.
        /// </param>
        /// <param name="trans">
        /// [in] TRANS is CHARACTER*1.
        /// = &#39;N&#39;:  No transpose, apply Q;
        /// = &#39;C&#39;:  Conjugate transpose, apply Q**H.
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
        /// [in] A is COMPLEX array, dimension (LDA,K).
        /// Part of the data structure to represent Q as returned by CGEQR.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.
        /// If SIDE = &#39;L&#39;, LDA &gt;= max(1,M);
        /// if SIDE = &#39;R&#39;, LDA &gt;= max(1,N).
        /// </param>
        /// <param name="t">
        /// [in] T is COMPLEX array, dimension (MAX(5,TSIZE)).
        /// Part of the data structure to represent Q as returned by CGEQR.
        /// </param>
        /// <param name="tsize">
        /// [in] TSIZE is INTEGER.
        /// The dimension of the array T. TSIZE &gt;= 5.
        /// </param>
        /// <param name="c">
        /// [in,out] C is COMPLEX array, dimension (LDC,N).
        /// On entry, the M-by-N matrix C.
        /// On exit, C is overwritten by Q*C or Q**H*C or C*Q**H or C*Q.
        /// </param>
        /// <param name="ldc">
        /// [in] LDC is INTEGER.
        /// The leading dimension of the array C. LDC &gt;= max(1,M).
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_cgemqr", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Cgemqr(
            MatrixLayout matrixLayout,
            char side,
            char trans,
            int m,
            int n,
            int k,
            Complex32* a,
            int lda,
            Complex32* t,
            int tsize,
            Complex32* c,
            int ldc);
    }
}
