using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Blas
    {
        /// <summary>
        /// <para>
        ///    SSCAL scales a vector by a constant.
        ///    uses unrolled loops for increment equal to 1.
        /// </para>
        /// </summary>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// number of elements in input vector(s)
        /// </param>
        /// <param name="alpha">
        /// No description available.
        /// </param>
        /// <param name="x">
        /// [in,out] SX is REAL array, dimension ( 1 + ( N - 1 )*abs( INCX ) ).
        /// </param>
        /// <param name="incx">
        /// [in] INCX is INTEGER.
        /// storage spacing between elements of SX
        /// </param>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "cblas_sscal", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe void Sscal(
            int n,
            float alpha,
            float* x,
            int incx);
    }
}
