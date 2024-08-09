using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// CLARFT forms the triangular factor T of a complex block reflector H
        /// of order n, which is defined as a product of k elementary reflectors.
        /// </para>
        /// <para>
        /// If DIRECT = &#39;F&#39;, H = H(1) H(2) . . . H(k) and T is upper triangular;
        /// </para>
        /// <para>
        /// If DIRECT = &#39;B&#39;, H = H(k) . . . H(2) H(1) and T is lower triangular.
        /// </para>
        /// <para>
        /// If STOREV = &#39;C&#39;, the vector which defines the elementary reflector
        /// H(i) is stored in the i-th column of the array V, and
        /// </para>
        /// <para>
        ///    H  =  I - V * T * V**H
        /// </para>
        /// <para>
        /// If STOREV = &#39;R&#39;, the vector which defines the elementary reflector
        /// H(i) is stored in the i-th row of the array V, and
        /// </para>
        /// <para>
        ///    H  =  I - V**H * T * V
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="direct">
        /// [in] DIRECT is CHARACTER*1.
        /// Specifies the order in which the elementary reflectors are
        /// multiplied to form the block reflector:
        /// = &#39;F&#39;: H = H(1) H(2) . . . H(k) (Forward)
        /// = &#39;B&#39;: H = H(k) . . . H(2) H(1) (Backward)
        /// </param>
        /// <param name="storev">
        /// [in] STOREV is CHARACTER*1.
        /// Specifies how the vectors which define the elementary
        /// reflectors are stored (see also Further Details):
        /// = &#39;C&#39;: columnwise
        /// = &#39;R&#39;: rowwise
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the block reflector H. N &gt;= 0.
        /// </param>
        /// <param name="k">
        /// [in] K is INTEGER.
        /// The order of the triangular factor T (= the number of
        /// elementary reflectors). K &gt;= 1.
        /// </param>
        /// <param name="v">
        /// [in] V is COMPLEX array, dimension.
        /// (LDV,K) if STOREV = &#39;C&#39;
        /// (LDV,N) if STOREV = &#39;R&#39;
        /// The matrix V. See further details.
        /// </param>
        /// <param name="ldv">
        /// [in] LDV is INTEGER.
        /// The leading dimension of the array V.
        /// If STOREV = &#39;C&#39;, LDV &gt;= max(1,N); if STOREV = &#39;R&#39;, LDV &gt;= K.
        /// </param>
        /// <param name="tau">
        /// [in] TAU is COMPLEX array, dimension (K).
        /// TAU(i) must contain the scalar factor of the elementary
        /// reflector H(i).
        /// </param>
        /// <param name="t">
        /// [out] T is COMPLEX array, dimension (LDT,K).
        /// The k by k triangular factor T of the block reflector.
        /// If DIRECT = &#39;F&#39;, T is upper triangular; if DIRECT = &#39;B&#39;, T is
        /// lower triangular. The rest of the array is not used.
        /// </param>
        /// <param name="ldt">
        /// [in] LDT is INTEGER.
        /// The leading dimension of the array T. LDT &gt;= K.
        /// </param>
        /// <remarks>
        /// <para>
        ///  The shape of the matrix V and the storage of the vectors which define
        ///  the H(i) is best illustrated by the following example with n = 5 and
        ///  k = 3. The elements equal to 1 are not stored.
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
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_clarft", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Clarft(
            MatrixLayout matrixLayout,
            char direct,
            char storev,
            int n,
            int k,
            Complex32* v,
            int ldv,
            Complex32* tau,
            Complex32* t,
            int ldt);
    }
}
