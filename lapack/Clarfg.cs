using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// CLARFG generates a complex elementary reflector H of order n, such
        /// that
        /// </para>
        /// <para>
        ///       H**H * ( alpha ) = ( beta ),   H**H * H = I.
        ///              (   x   )   (   0  )
        /// </para>
        /// <para>
        /// where alpha and beta are scalars, with beta real, and x is an
        /// (n-1)-element complex vector. H is represented in the form
        /// </para>
        /// <para>
        ///       H = I - tau * ( 1 ) * ( 1 v**H ) ,
        ///                     ( v )
        /// </para>
        /// <para>
        /// where tau is a complex scalar and v is a complex (n-1)-element
        /// vector. Note that H is not hermitian.
        /// </para>
        /// <para>
        /// If the elements of x are all zero and alpha is real, then tau = 0
        /// and H is taken to be the unit matrix.
        /// </para>
        /// <para>
        /// Otherwise  1 &lt;= real(tau) &lt;= 2  and  abs(tau-1) &lt;= 1 .
        /// </para>
        /// </summary>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the elementary reflector.
        /// </param>
        /// <param name="alpha">
        /// [in,out] ALPHA is COMPLEX.
        /// On entry, the value alpha.
        /// On exit, it is overwritten with the value beta.
        /// </param>
        /// <param name="x">
        /// [in,out] X is COMPLEX array, dimension.
        /// (1+(N-2)*abs(INCX))
        /// On entry, the vector x.
        /// On exit, it is overwritten with the vector v.
        /// </param>
        /// <param name="incx">
        /// [in] INCX is INTEGER.
        /// The increment between elements of X. INCX &gt; 0.
        /// </param>
        /// <param name="tau">
        /// [out] TAU is COMPLEX.
        /// The value tau.
        /// </param>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_clarfg", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Clarfg(
            int n,
            Complex32* alpha,
            Complex32* x,
            int incx,
            Complex32* tau);
    }
}
