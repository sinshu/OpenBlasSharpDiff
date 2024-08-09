using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// If VECT = &#39;Q&#39;, CUNMBR overwrites the general complex M-by-N matrix C
        /// with
        ///                 SIDE = &#39;L&#39;     SIDE = &#39;R&#39;
        /// TRANS = &#39;N&#39;:      Q * C          C * Q
        /// TRANS = &#39;C&#39;:      Q**H * C       C * Q**H
        /// </para>
        /// <para>
        /// If VECT = &#39;P&#39;, CUNMBR overwrites the general complex M-by-N matrix C
        /// with
        ///                 SIDE = &#39;L&#39;     SIDE = &#39;R&#39;
        /// TRANS = &#39;N&#39;:      P * C          C * P
        /// TRANS = &#39;C&#39;:      P**H * C       C * P**H
        /// </para>
        /// <para>
        /// Here Q and P**H are the unitary matrices determined by CGEBRD when
        /// reducing a complex matrix A to bidiagonal form: A = Q * B * P**H. Q
        /// and P**H are defined as products of elementary reflectors H(i) and
        /// G(i) respectively.
        /// </para>
        /// <para>
        /// Let nq = m if SIDE = &#39;L&#39; and nq = n if SIDE = &#39;R&#39;. Thus nq is the
        /// order of the unitary matrix Q or P**H that is applied.
        /// </para>
        /// <para>
        /// If VECT = &#39;Q&#39;, A is assumed to have been an NQ-by-K matrix:
        /// if nq &gt;= k, Q = H(1) H(2) . . . H(k);
        /// if nq &lt; k, Q = H(1) H(2) . . . H(nq-1).
        /// </para>
        /// <para>
        /// If VECT = &#39;P&#39;, A is assumed to have been a K-by-NQ matrix:
        /// if k &lt; nq, P = G(1) G(2) . . . G(k);
        /// if k &gt;= nq, P = G(1) G(2) . . . G(nq-1).
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="vect">
        /// [in] VECT is CHARACTER*1.
        /// = &#39;Q&#39;: apply Q or Q**H;
        /// = &#39;P&#39;: apply P or P**H.
        /// </param>
        /// <param name="side">
        /// [in] SIDE is CHARACTER*1.
        /// = &#39;L&#39;: apply Q, Q**H, P or P**H from the Left;
        /// = &#39;R&#39;: apply Q, Q**H, P or P**H from the Right.
        /// </param>
        /// <param name="trans">
        /// [in] TRANS is CHARACTER*1.
        /// = &#39;N&#39;:  No transpose, apply Q or P;
        /// = &#39;C&#39;:  Conjugate transpose, apply Q**H or P**H.
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
        /// If VECT = &#39;Q&#39;, the number of columns in the original
        /// matrix reduced by CGEBRD.
        /// If VECT = &#39;P&#39;, the number of rows in the original
        /// matrix reduced by CGEBRD.
        /// K &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in] A is COMPLEX array, dimension.
        /// (LDA,min(nq,K)) if VECT = &#39;Q&#39;
        /// (LDA,nq)        if VECT = &#39;P&#39;
        /// The vectors which define the elementary reflectors H(i) and
        /// G(i), whose products determine the matrices Q and P, as
        /// returned by CGEBRD.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.
        /// If VECT = &#39;Q&#39;, LDA &gt;= max(1,nq);
        /// if VECT = &#39;P&#39;, LDA &gt;= max(1,min(nq,K)).
        /// </param>
        /// <param name="tau">
        /// [in] TAU is COMPLEX array, dimension (min(nq,K)).
        /// TAU(i) must contain the scalar factor of the elementary
        /// reflector H(i) or G(i) which determines Q or P, as returned
        /// by CGEBRD in the array argument TAUQ or TAUP.
        /// </param>
        /// <param name="c">
        /// [in,out] C is COMPLEX array, dimension (LDC,N).
        /// On entry, the M-by-N matrix C.
        /// On exit, C is overwritten by Q*C or Q**H*C or C*Q**H or C*Q
        /// or P*C or P**H*C or C*P or C*P**H.
        /// </param>
        /// <param name="ldc">
        /// [in] LDC is INTEGER.
        /// The leading dimension of the array C. LDC &gt;= max(1,M).
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_cunmbr", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Cunmbr(
            MatrixLayout matrixLayout,
            char vect,
            char side,
            char trans,
            int m,
            int n,
            int k,
            Complex32* a,
            int lda,
            Complex32* tau,
            Complex32* c,
            int ldc);
    }
}
