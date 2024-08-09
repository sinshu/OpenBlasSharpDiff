using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Blas
    {
        /// <summary>
        /// <para>
        ///    DZASUM takes the sum of the (|Re(.)| + |Im(.)|)&#39;s of a complex vector and
        ///    returns a single precision result.
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
        [DllImport(OpenBlas.LibraryName, EntryPoint = "cblas_dzasum", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe double Dzasum(
            int n,
            void* x,
            int incx);
    }
}
