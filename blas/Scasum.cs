using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Blas
    {
        /// <summary>
        /// <para>
        ///    SCASUM takes the sum of the (|Re(.)| + |Im(.)|)&#39;s of a complex vector and
        ///    returns a single precision result.
        /// </para>
        /// </summary>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// number of elements in input vector(s)
        /// </param>
        /// <param name="x">
        /// [in,out] CX is COMPLEX array, dimension ( 1 + ( N - 1 )*abs( INCX ) ).
        /// </param>
        /// <param name="incx">
        /// [in] INCX is INTEGER.
        /// storage spacing between elements of SX
        /// </param>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "cblas_scasum", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe float Scasum(
            int n,
            void* x,
            int incx);
    }
}
