using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DGEMQRT overwrites the general real M-by-N matrix C with
        /// </para>
        /// <para>
        ///                 SIDE = &#39;L&#39;     SIDE = &#39;R&#39;
        /// TRANS = &#39;N&#39;:      Q C            C Q
        /// TRANS = &#39;T&#39;:   Q**T C            C Q**T
        /// </para>
        /// <para>
        /// where Q is a real orthogonal matrix defined as the product of K
        /// elementary reflectors:
        /// </para>
        /// <para>
        ///       Q = H(1) H(2) . . . H(K) = I - V T V**T
        /// </para>
        /// <para>
        /// generated using the compact WY representation as returned by DGEQRT.
        /// </para>
        /// <para>
        /// Q is of order M if SIDE = &#39;L&#39; and of order N  if SIDE = &#39;R&#39;.
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
        /// = &#39;C&#39;:  Transpose, apply Q**T.
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
        /// <param name="nb">
        /// [in] NB is INTEGER.
        /// The block size used for the storage of T.  K &gt;= NB &gt;= 1.
        /// This must be the same value of NB used to generate T
        /// in DGEQRT.
        /// </param>
        /// <param name="v">
        /// [in] V is DOUBLE PRECISION array, dimension (LDV,K).
        /// The i-th column must contain the vector which defines the
        /// elementary reflector H(i), for i = 1,2,...,k, as returned by
        /// DGEQRT in the first K columns of its array argument A.
        /// </param>
        /// <param name="ldv">
        /// [in] LDV is INTEGER.
        /// The leading dimension of the array V.
        /// If SIDE = &#39;L&#39;, LDA &gt;= max(1,M);
        /// if SIDE = &#39;R&#39;, LDA &gt;= max(1,N).
        /// </param>
        /// <param name="t">
        /// [in] T is DOUBLE PRECISION array, dimension (LDT,K).
        /// The upper triangular factors of the block reflectors
        /// as returned by DGEQRT, stored as a NB-by-N matrix.
        /// </param>
        /// <param name="ldt">
        /// [in] LDT is INTEGER.
        /// The leading dimension of the array T.  LDT &gt;= NB.
        /// </param>
        /// <param name="c">
        /// [in,out] C is DOUBLE PRECISION array, dimension (LDC,N).
        /// On entry, the M-by-N matrix C.
        /// On exit, C is overwritten by Q C, Q**T C, C Q**T or C Q.
        /// </param>
        /// <param name="ldc">
        /// [in] LDC is INTEGER.
        /// The leading dimension of the array C. LDC &gt;= max(1,M).
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dgemqrt", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dgemqrt(
            MatrixLayout matrixLayout,
            char side,
            char trans,
            int m,
            int n,
            int k,
            int nb,
            double* v,
            int ldv,
            double* t,
            int ldt,
            double* c,
            int ldc);
    }
}
