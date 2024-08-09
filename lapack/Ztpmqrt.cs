using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// ZTPMQRT applies a complex orthogonal matrix Q obtained from a
        /// &quot;triangular-pentagonal&quot; complex block reflector H to a general
        /// complex matrix C, which consists of two blocks A and B.
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
        /// The number of rows of the matrix B. M &gt;= 0.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The number of columns of the matrix B. N &gt;= 0.
        /// </param>
        /// <param name="k">
        /// [in] K is INTEGER.
        /// The number of elementary reflectors whose product defines
        /// the matrix Q.
        /// </param>
        /// <param name="l">
        /// [in] L is INTEGER.
        /// The order of the trapezoidal part of V.
        /// K &gt;= L &gt;= 0.  See Further Details.
        /// </param>
        /// <param name="nb">
        /// [in] NB is INTEGER.
        /// The block size used for the storage of T.  K &gt;= NB &gt;= 1.
        /// This must be the same value of NB used to generate T
        /// in CTPQRT.
        /// </param>
        /// <param name="v">
        /// [in] V is COMPLEX*16 array, dimension (LDV,K).
        /// The i-th column must contain the vector which defines the
        /// elementary reflector H(i), for i = 1,2,...,k, as returned by
        /// CTPQRT in B.  See Further Details.
        /// </param>
        /// <param name="ldv">
        /// [in] LDV is INTEGER.
        /// The leading dimension of the array V.
        /// If SIDE = &#39;L&#39;, LDV &gt;= max(1,M);
        /// if SIDE = &#39;R&#39;, LDV &gt;= max(1,N).
        /// </param>
        /// <param name="t">
        /// [in] T is COMPLEX*16 array, dimension (LDT,K).
        /// The upper triangular factors of the block reflectors
        /// as returned by CTPQRT, stored as a NB-by-K matrix.
        /// </param>
        /// <param name="ldt">
        /// [in] LDT is INTEGER.
        /// The leading dimension of the array T.  LDT &gt;= NB.
        /// </param>
        /// <param name="a">
        /// [in,out] A is COMPLEX*16 array, dimension.
        /// (LDA,N) if SIDE = &#39;L&#39; or
        /// (LDA,K) if SIDE = &#39;R&#39;
        /// On entry, the K-by-N or M-by-K matrix A.
        /// On exit, A is overwritten by the corresponding block of
        /// Q*C or Q**H*C or C*Q or C*Q**H.  See Further Details.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.
        /// If SIDE = &#39;L&#39;, LDC &gt;= max(1,K);
        /// If SIDE = &#39;R&#39;, LDC &gt;= max(1,M).
        /// </param>
        /// <param name="b">
        /// [in,out] B is COMPLEX*16 array, dimension (LDB,N).
        /// On entry, the M-by-N matrix B.
        /// On exit, B is overwritten by the corresponding block of
        /// Q*C or Q**H*C or C*Q or C*Q**H.  See Further Details.
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// The leading dimension of the array B.
        /// LDB &gt;= max(1,M).
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// </returns>
        /// <remarks>
        /// <para>
        ///  The columns of the pentagonal matrix V contain the elementary reflectors
        ///  H(1), H(2), ..., H(K); V is composed of a rectangular block V1 and a
        ///  trapezoidal block V2:
        /// </para>
        /// <para>
        ///        V = [V1]
        ///            [V2].
        /// </para>
        /// <para>
        ///  The size of the trapezoidal block V2 is determined by the parameter L,
        ///  where 0 &lt;= L &lt;= K; V2 is upper trapezoidal, consisting of the first L
        ///  rows of a K-by-K upper triangular matrix.  If L=K, V2 is upper triangular;
        ///  if L=0, there is no trapezoidal block, hence V = V1 is rectangular.
        /// </para>
        /// <para>
        ///  If SIDE = &#39;L&#39;:  C = [A]  where A is K-by-N,  B is M-by-N and V is M-by-K.
        ///                      [B]
        /// </para>
        /// <para>
        ///  If SIDE = &#39;R&#39;:  C = [A B]  where A is M-by-K, B is M-by-N and V is N-by-K.
        /// </para>
        /// <para>
        ///  The complex orthogonal matrix Q is formed from V and T.
        /// </para>
        /// <para>
        ///  If TRANS=&#39;N&#39; and SIDE=&#39;L&#39;, C is on exit replaced with Q * C.
        /// </para>
        /// <para>
        ///  If TRANS=&#39;C&#39; and SIDE=&#39;L&#39;, C is on exit replaced with Q**H * C.
        /// </para>
        /// <para>
        ///  If TRANS=&#39;N&#39; and SIDE=&#39;R&#39;, C is on exit replaced with C * Q.
        /// </para>
        /// <para>
        ///  If TRANS=&#39;C&#39; and SIDE=&#39;R&#39;, C is on exit replaced with C * Q**H.
        /// </para>
        /// </remarks>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_ztpmqrt", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Ztpmqrt(
            MatrixLayout matrixLayout,
            char side,
            char trans,
            int m,
            int n,
            int k,
            int l,
            int nb,
            Complex* v,
            int ldv,
            Complex* t,
            int ldt,
            Complex* a,
            int lda,
            Complex* b,
            int ldb);
    }
}
