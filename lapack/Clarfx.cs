using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// CLARFX applies a complex elementary reflector H to a complex m by n
        /// matrix C, from either the left or the right. H is represented in the
        /// form
        /// </para>
        /// <para>
        ///       H = I - tau * v * v**H
        /// </para>
        /// <para>
        /// where tau is a complex scalar and v is a complex vector.
        /// </para>
        /// <para>
        /// If tau = 0, then H is taken to be the unit matrix
        /// </para>
        /// <para>
        /// This version uses inline code if H has order &lt; 11.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="side">
        /// [in] SIDE is CHARACTER*1.
        /// = &#39;L&#39;: form  H * C
        /// = &#39;R&#39;: form  C * H
        /// </param>
        /// <param name="m">
        /// [in] M is INTEGER.
        /// The number of rows of the matrix C.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The number of columns of the matrix C.
        /// </param>
        /// <param name="v">
        /// [in] V is COMPLEX array, dimension (M) if SIDE = &#39;L&#39;.
        /// or (N) if SIDE = &#39;R&#39;
        /// The vector v in the representation of H.
        /// </param>
        /// <param name="tau">
        /// [in] TAU is COMPLEX.
        /// The value tau in the representation of H.
        /// </param>
        /// <param name="c">
        /// [in,out] C is COMPLEX array, dimension (LDC,N).
        /// On entry, the m by n matrix C.
        /// On exit, C is overwritten by the matrix H * C if SIDE = &#39;L&#39;,
        /// or C * H if SIDE = &#39;R&#39;.
        /// </param>
        /// <param name="ldc">
        /// [in] LDC is INTEGER.
        /// The leading dimension of the array C. LDC &gt;= max(1,M).
        /// </param>
        /// <param name="work">
        /// [out] WORK is COMPLEX array, dimension (N) if SIDE = &#39;L&#39;.
        /// or (M) if SIDE = &#39;R&#39;
        /// WORK is not referenced if H has order &lt; 11.
        /// </param>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_clarfx", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Clarfx(
            MatrixLayout matrixLayout,
            char side,
            int m,
            int n,
            Complex32* v,
            Complex32 tau,
            Complex32* c,
            int ldc,
            Complex32* work);
    }
}
