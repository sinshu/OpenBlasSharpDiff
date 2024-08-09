using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Blas
    {
        /// <summary>
        /// <para>
        /// CDOTC forms the dot product of two complex vectors
        ///      CDOTC = X^H * Y
        /// </para>
        /// </summary>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// number of elements in input vector(s)
        /// </param>
        /// <param name="x">
        /// [in] CX is REAL array, dimension ( 1 + ( N - 1 )*abs( INCX ) ).
        /// </param>
        /// <param name="incx">
        /// [in] INCX is INTEGER.
        /// storage spacing between elements of CX
        /// </param>
        /// <param name="y">
        /// [in] CY is REAL array, dimension ( 1 + ( N - 1 )*abs( INCY ) ).
        /// </param>
        /// <param name="incy">
        /// [in] INCY is INTEGER.
        /// storage spacing between elements of CY
        /// </param>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "cblas_cdotc", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe Complex32 Cdotc(
            int n,
            void* x,
            int incx,
            void* y,
            int incy);
    }
}
