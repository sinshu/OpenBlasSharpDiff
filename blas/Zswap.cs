using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Blas
    {
        /// <summary>
        /// <para>
        ///    ZSWAP interchanges two vectors.
        /// </para>
        /// </summary>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// number of elements in input vector(s)
        /// </param>
        /// <param name="x">
        /// [in,out] ZX is COMPLEX*16 array, dimension ( 1 + ( N - 1 )*abs( INCX ) ).
        /// </param>
        /// <param name="incx">
        /// [in] INCX is INTEGER.
        /// storage spacing between elements of ZX
        /// </param>
        /// <param name="y">
        /// [in,out] ZY is COMPLEX*16 array, dimension ( 1 + ( N - 1 )*abs( INCY ) ).
        /// </param>
        /// <param name="incy">
        /// [in] INCY is INTEGER.
        /// storage spacing between elements of ZY
        /// </param>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "cblas_zswap", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe void Zswap(
            int n,
            void* x,
            int incx,
            void* y,
            int incy);
    }
}
