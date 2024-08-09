using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Blas
    {
        /// <summary>
        /// <para>
        ///    SDOT forms the dot product of two vectors.
        ///    uses unrolled loops for increments equal to one.
        /// </para>
        /// </summary>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// number of elements in input vector(s)
        /// </param>
        /// <param name="x">
        /// [in] SX is REAL array, dimension ( 1 + ( N - 1 )*abs( INCX ) ).
        /// </param>
        /// <param name="incx">
        /// [in] INCX is INTEGER.
        /// storage spacing between elements of SX
        /// </param>
        /// <param name="y">
        /// [in] SY is REAL array, dimension ( 1 + ( N - 1 )*abs( INCY ) ).
        /// </param>
        /// <param name="incy">
        /// [in] INCY is INTEGER.
        /// storage spacing between elements of SY
        /// </param>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "cblas_sdot", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe float Sdot(
            int n,
            float* x,
            int incx,
            float* y,
            int incy);
    }
}
