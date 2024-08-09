using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// CLARFB applies a complex block reflector H or its transpose H**H to a
        /// complex M-by-N matrix C, from either the left or the right.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="side">
        /// [in] SIDE is CHARACTER*1.
        /// = &#39;L&#39;: apply H or H**H from the Left
        /// = &#39;R&#39;: apply H or H**H from the Right
        /// </param>
        /// <param name="trans">
        /// [in] TRANS is CHARACTER*1.
        /// = &#39;N&#39;: apply H (No transpose)
        /// = &#39;C&#39;: apply H**H (Conjugate transpose)
        /// </param>
        /// <param name="direct">
        /// [in] DIRECT is CHARACTER*1.
        /// Indicates how H is formed from a product of elementary
        /// reflectors
        /// = &#39;F&#39;: H = H(1) H(2) . . . H(k) (Forward)
        /// = &#39;B&#39;: H = H(k) . . . H(2) H(1) (Backward)
        /// </param>
        /// <param name="storev">
        /// [in] STOREV is CHARACTER*1.
        /// Indicates how the vectors which define the elementary
        /// reflectors are stored:
        /// = &#39;C&#39;: Columnwise
        /// = &#39;R&#39;: Rowwise
        /// </param>
        /// <param name="m">
        /// [in] M is INTEGER.
        /// The number of rows of the matrix C.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The number of columns of the matrix C.
        /// </param>
        /// <param name="k">
        /// [in] K is INTEGER.
        /// The order of the matrix T (= the number of elementary
        /// reflectors whose product defines the block reflector).
        /// If SIDE = &#39;L&#39;, M &gt;= K &gt;= 0;
        /// if SIDE = &#39;R&#39;, N &gt;= K &gt;= 0.
        /// </param>
        /// <param name="v">
        /// [in] V is COMPLEX array, dimension.
        /// (LDV,K) if STOREV = &#39;C&#39;
        /// (LDV,M) if STOREV = &#39;R&#39; and SIDE = &#39;L&#39;
        /// (LDV,N) if STOREV = &#39;R&#39; and SIDE = &#39;R&#39;
        /// The matrix V. See Further Details.
        /// </param>
        /// <param name="ldv">
        /// [in] LDV is INTEGER.
        /// The leading dimension of the array V.
        /// If STOREV = &#39;C&#39; and SIDE = &#39;L&#39;, LDV &gt;= max(1,M);
        /// if STOREV = &#39;C&#39; and SIDE = &#39;R&#39;, LDV &gt;= max(1,N);
        /// if STOREV = &#39;R&#39;, LDV &gt;= K.
        /// </param>
        /// <param name="t">
        /// [in] T is COMPLEX array, dimension (LDT,K).
        /// The triangular K-by-K matrix T in the representation of the
        /// block reflector.
        /// </param>
        /// <param name="ldt">
        /// [in] LDT is INTEGER.
        /// The leading dimension of the array T. LDT &gt;= K.
        /// </param>
        /// <param name="c">
        /// [in,out] C is COMPLEX array, dimension (LDC,N).
        /// On entry, the M-by-N matrix C.
        /// On exit, C is overwritten by H*C or H**H*C or C*H or C*H**H.
        /// </param>
        /// <param name="ldc">
        /// [in] LDC is INTEGER.
        /// The leading dimension of the array C. LDC &gt;= max(1,M).
        /// </param>
        /// <remarks>
        /// <para>
        ///  The shape of the matrix V and the storage of the vectors which define
        ///  the H(i) is best illustrated by the following example with n = 5 and
        ///  k = 3. The elements equal to 1 are not stored; the corresponding
        ///  array elements are modified but restored on exit. The rest of the
        ///  array is not used.
        /// </para>
        /// <para>
        ///  DIRECT = &#39;F&#39; and STOREV = &#39;C&#39;:         DIRECT = &#39;F&#39; and STOREV = &#39;R&#39;:
        /// </para>
        /// <para>
        ///               V = (  1       )                 V = (  1 v1 v1 v1 v1 )
        ///                   ( v1  1    )                     (     1 v2 v2 v2 )
        ///                   ( v1 v2  1 )                     (        1 v3 v3 )
        ///                   ( v1 v2 v3 )
        ///                   ( v1 v2 v3 )
        /// </para>
        /// <para>
        ///  DIRECT = &#39;B&#39; and STOREV = &#39;C&#39;:         DIRECT = &#39;B&#39; and STOREV = &#39;R&#39;:
        /// </para>
        /// <para>
        ///               V = ( v1 v2 v3 )                 V = ( v1 v1  1       )
        ///                   ( v1 v2 v3 )                     ( v2 v2 v2  1    )
        ///                   (  1 v2 v3 )                     ( v3 v3 v3 v3  1 )
        ///                   (     1 v3 )
        ///                   (        1 )
        /// </para>
        /// </remarks>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_clarfb", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Clarfb(
            MatrixLayout matrixLayout,
            char side,
            char trans,
            char direct,
            char storev,
            int m,
            int n,
            int k,
            Complex32* v,
            int ldv,
            Complex32* t,
            int ldt,
            Complex32* c,
            int ldc);
    }
}
