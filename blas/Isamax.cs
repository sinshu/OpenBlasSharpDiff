using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Blas
    {
        /// <summary>
        /// <para>
        ///    ISAMAX finds the index of the first element having maximum absolute value.
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
        [DllImport(OpenBlas.LibraryName, EntryPoint = "cblas_isamax", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe UIntPtr Isamax(
            int n,
            float* x,
            int incx);
    }
}
