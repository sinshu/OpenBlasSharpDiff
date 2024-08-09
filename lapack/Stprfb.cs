using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// STPRFB applies a real &quot;triangular-pentagonal&quot; block reflector H or its
        /// transpose H**T to a real matrix C, which is composed of two
        /// blocks A and B, either from the left or right.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="side">
        /// [in] SIDE is CHARACTER*1.
        /// = &#39;L&#39;: apply H or H**T from the Left
        /// = &#39;R&#39;: apply H or H**T from the Right
        /// </param>
        /// <param name="trans">
        /// [in] TRANS is CHARACTER*1.
        /// = &#39;N&#39;: apply H (No transpose)
        /// = &#39;T&#39;: apply H**T (Transpose)
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
        /// = &#39;C&#39;: Columns
        /// = &#39;R&#39;: Rows
        /// </param>
        /// <param name="m">
        /// [in] M is INTEGER.
        /// The number of rows of the matrix B.
        /// M &gt;= 0.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The number of columns of the matrix B.
        /// N &gt;= 0.
        /// </param>
        /// <param name="k">
        /// [in] K is INTEGER.
        /// The order of the matrix T, i.e. the number of elementary
        /// reflectors whose product defines the block reflector.
        /// K &gt;= 0.
        /// </param>
        /// <param name="l">
        /// [in] L is INTEGER.
        /// The order of the trapezoidal part of V.
        /// K &gt;= L &gt;= 0.  See Further Details.
        /// </param>
        /// <param name="v">
        /// [in] V is REAL array, dimension.
        /// (LDV,K) if STOREV = &#39;C&#39;
        /// (LDV,M) if STOREV = &#39;R&#39; and SIDE = &#39;L&#39;
        /// (LDV,N) if STOREV = &#39;R&#39; and SIDE = &#39;R&#39;
        /// The pentagonal matrix V, which contains the elementary reflectors
        /// H(1), H(2), ..., H(K).  See Further Details.
        /// </param>
        /// <param name="ldv">
        /// [in] LDV is INTEGER.
        /// The leading dimension of the array V.
        /// If STOREV = &#39;C&#39; and SIDE = &#39;L&#39;, LDV &gt;= max(1,M);
        /// if STOREV = &#39;C&#39; and SIDE = &#39;R&#39;, LDV &gt;= max(1,N);
        /// if STOREV = &#39;R&#39;, LDV &gt;= K.
        /// </param>
        /// <param name="t">
        /// [in] T is REAL array, dimension (LDT,K).
        /// The triangular K-by-K matrix T in the representation of the
        /// block reflector.
        /// </param>
        /// <param name="ldt">
        /// [in] LDT is INTEGER.
        /// The leading dimension of the array T.
        /// LDT &gt;= K.
        /// </param>
        /// <param name="a">
        /// [in,out] A is REAL array, dimension.
        /// (LDA,N) if SIDE = &#39;L&#39; or (LDA,K) if SIDE = &#39;R&#39;
        /// On entry, the K-by-N or M-by-K matrix A.
        /// On exit, A is overwritten by the corresponding block of
        /// H*C or H**T*C or C*H or C*H**T.  See Further Details.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.
        /// If SIDE = &#39;L&#39;, LDA &gt;= max(1,K);
        /// If SIDE = &#39;R&#39;, LDA &gt;= max(1,M).
        /// </param>
        /// <param name="b">
        /// [in,out] B is REAL array, dimension (LDB,N).
        /// On entry, the M-by-N matrix B.
        /// On exit, B is overwritten by the corresponding block of
        /// H*C or H**T*C or C*H or C*H**T.  See Further Details.
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// The leading dimension of the array B.
        /// LDB &gt;= max(1,M).
        /// </param>
        /// <remarks>
        /// <para>
        ///  The matrix C is a composite matrix formed from blocks A and B.
        ///  The block B is of size M-by-N; if SIDE = &#39;R&#39;, A is of size M-by-K,
        ///  and if SIDE = &#39;L&#39;, A is of size K-by-N.
        /// </para>
        /// <para>
        ///  If SIDE = &#39;R&#39; and DIRECT = &#39;F&#39;, C = [A B].
        /// </para>
        /// <para>
        ///  If SIDE = &#39;L&#39; and DIRECT = &#39;F&#39;, C = [A]
        ///                                      [B].
        /// </para>
        /// <para>
        ///  If SIDE = &#39;R&#39; and DIRECT = &#39;B&#39;, C = [B A].
        /// </para>
        /// <para>
        ///  If SIDE = &#39;L&#39; and DIRECT = &#39;B&#39;, C = [B]
        ///                                      [A].
        /// </para>
        /// <para>
        ///  The pentagonal matrix V is composed of a rectangular block V1 and a
        ///  trapezoidal block V2.  The size of the trapezoidal block is determined by
        ///  the parameter L, where 0&lt;=L&lt;=K.  If L=K, the V2 block of V is triangular;
        ///  if L=0, there is no trapezoidal block, thus V = V1 is rectangular.
        /// </para>
        /// <para>
        ///  If DIRECT = &#39;F&#39; and STOREV = &#39;C&#39;:  V = [V1]
        ///                                         [V2]
        ///     - V2 is upper trapezoidal (first L rows of K-by-K upper triangular)
        /// </para>
        /// <para>
        ///  If DIRECT = &#39;F&#39; and STOREV = &#39;R&#39;:  V = [V1 V2]
        /// </para>
        /// <para>
        ///     - V2 is lower trapezoidal (first L columns of K-by-K lower triangular)
        /// </para>
        /// <para>
        ///  If DIRECT = &#39;B&#39; and STOREV = &#39;C&#39;:  V = [V2]
        ///                                         [V1]
        ///     - V2 is lower trapezoidal (last L rows of K-by-K lower triangular)
        /// </para>
        /// <para>
        ///  If DIRECT = &#39;B&#39; and STOREV = &#39;R&#39;:  V = [V2 V1]
        /// </para>
        /// <para>
        ///     - V2 is upper trapezoidal (last L columns of K-by-K upper triangular)
        /// </para>
        /// <para>
        ///  If STOREV = &#39;C&#39; and SIDE = &#39;L&#39;, V is M-by-K with V2 L-by-K.
        /// </para>
        /// <para>
        ///  If STOREV = &#39;C&#39; and SIDE = &#39;R&#39;, V is N-by-K with V2 L-by-K.
        /// </para>
        /// <para>
        ///  If STOREV = &#39;R&#39; and SIDE = &#39;L&#39;, V is K-by-M with V2 K-by-L.
        /// </para>
        /// <para>
        ///  If STOREV = &#39;R&#39; and SIDE = &#39;R&#39;, V is K-by-N with V2 K-by-L.
        /// </para>
        /// </remarks>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_stprfb", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Stprfb(
            MatrixLayout matrixLayout,
            char side,
            char trans,
            char direct,
            char storev,
            int m,
            int n,
            int k,
            int l,
            float* v,
            int ldv,
            float* t,
            int ldt,
            float* a,
            int lda,
            float* b,
            int ldb);
    }
}
