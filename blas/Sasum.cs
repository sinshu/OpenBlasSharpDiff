using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Blas
    {
        /// <summary>
        /// <para>
        ///    SASUM takes the sum of the absolute values.
        ///    uses unrolled loops for increment equal to one.
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
        [DllImport(OpenBlas.LibraryName, EntryPoint = "cblas_sasum", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe float Sasum(
            int n,
            float* x,
            int incx);
    }
}
