using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Blas
    {
        /// <summary>
        /// <para>
        ///    applies a plane rotation.
        /// </para>
        /// </summary>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// number of elements in input vector(s)
        /// </param>
        /// <param name="x">
        /// [in,out] SX is REAL array, dimension ( 1 + ( N - 1 )*abs( INCX ) ).
        /// </param>
        /// <param name="incx">
        /// [in] INCX is INTEGER.
        /// storage spacing between elements of SX
        /// </param>
        /// <param name="y">
        /// [in,out] SY is REAL array, dimension ( 1 + ( N - 1 )*abs( INCY ) ).
        /// </param>
        /// <param name="incy">
        /// [in] INCY is INTEGER.
        /// storage spacing between elements of SY
        /// </param>
        /// <param name="c">
        /// [in] C is REAL.
        /// </param>
        /// <param name="s">
        /// [in] S is REAL.
        /// </param>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "cblas_srot", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe void Srot(
            int n,
            float* x,
            int incx,
            float* y,
            int incy,
            float c,
            float s);
    }
}
