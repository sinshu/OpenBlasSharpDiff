using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Blas
    {
        /// <summary>
        /// <para>
        ///   Compute the inner product of two vectors with extended
        ///   precision accumulation.
        /// </para>
        /// <para>
        ///   Returns S.P. result with dot product accumulated in D.P.
        ///   SDSDOT = SB + sum for I = 0 to N-1 of SX(LX+I*INCX)*SY(LY+I*INCY),
        ///   where LX = 1 if INCX .GE. 0, else LX = 1+(1-N)*INCX, and LY is
        ///   defined in a similar way using INCY.
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
        /// [in] SX is REAL array, dimension ( 1 + ( N - 1 )*abs( INCX ) ).
        /// single precision vector with N elements
        /// </param>
        /// <param name="incx">
        /// [in] INCX is INTEGER.
        /// storage spacing between elements of SX
        /// </param>
        /// <param name="y">
        /// [in] SY is REAL array, dimension ( 1 + ( N - 1 )*abs( INCX ) ).
        /// single precision vector with N elements
        /// </param>
        /// <param name="incy">
        /// [in] INCY is INTEGER.
        /// storage spacing between elements of SY
        /// </param>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "cblas_sdsdot", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe float Sdsdot(
            int n,
            float alpha,
            float* x,
            int incx,
            float* y,
            int incy);
    }
}
