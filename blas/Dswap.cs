using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Blas
    {
        /// <summary>
        /// <para>
        ///    DSWAP interchanges two vectors.
        ///    uses unrolled loops for increments equal to 1.
        /// </para>
        /// </summary>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// number of elements in input vector(s)
        /// </param>
        /// <param name="x">
        /// [in,out] DX is DOUBLE PRECISION array, dimension ( 1 + ( N - 1 )*abs( INCX ) ).
        /// </param>
        /// <param name="incx">
        /// [in] INCX is INTEGER.
        /// storage spacing between elements of DX
        /// </param>
        /// <param name="y">
        /// [in,out] DY is DOUBLE PRECISION array, dimension ( 1 + ( N - 1 )*abs( INCY ) ).
        /// </param>
        /// <param name="incy">
        /// [in] INCY is INTEGER.
        /// storage spacing between elements of DY
        /// </param>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "cblas_dswap", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe void Dswap(
            int n,
            double* x,
            int incx,
            double* y,
            int incy);
    }
}
