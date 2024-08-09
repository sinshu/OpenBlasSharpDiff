using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// SLARFG generates a real elementary reflector H of order n, such
        /// that
        /// </para>
        /// <para>
        ///       H * ( alpha ) = ( beta ),   H**T * H = I.
        ///           (   x   )   (   0  )
        /// </para>
        /// <para>
        /// where alpha and beta are scalars, and x is an (n-1)-element real
        /// vector. H is represented in the form
        /// </para>
        /// <para>
        ///       H = I - tau * ( 1 ) * ( 1 v**T ) ,
        ///                     ( v )
        /// </para>
        /// <para>
        /// where tau is a real scalar and v is a real (n-1)-element
        /// vector.
        /// </para>
        /// <para>
        /// If the elements of x are all zero, then tau = 0 and H is taken to be
        /// the unit matrix.
        /// </para>
        /// <para>
        /// Otherwise  1 &lt;= tau &lt;= 2.
        /// </para>
        /// </summary>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the elementary reflector.
        /// </param>
        /// <param name="alpha">
        /// [in,out] ALPHA is REAL.
        /// On entry, the value alpha.
        /// On exit, it is overwritten with the value beta.
        /// </param>
        /// <param name="x">
        /// [in,out] X is REAL array, dimension.
        /// (1+(N-2)*abs(INCX))
        /// On entry, the vector x.
        /// On exit, it is overwritten with the vector v.
        /// </param>
        /// <param name="incx">
        /// [in] INCX is INTEGER.
        /// The increment between elements of X. INCX &gt; 0.
        /// </param>
        /// <param name="tau">
        /// [out] TAU is REAL.
        /// The value tau.
        /// </param>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_slarfg", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Slarfg(
            int n,
            float* alpha,
            float* x,
            int incx,
            float* tau);
    }
}
