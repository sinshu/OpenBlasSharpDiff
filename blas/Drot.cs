using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Blas
    {
        /// <summary>
        /// <para>
        ///    DROT applies a plane rotation.
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
        /// <param name="c">
        /// [in] C is DOUBLE PRECISION.
        /// </param>
        /// <param name="s">
        /// [in] S is DOUBLE PRECISION.
        /// </param>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "cblas_drot", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe void Drot(
            int n,
            double* x,
            int incx,
            double* y,
            int incy,
            double c,
            double s);
    }
}
