using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Blas
    {
        /// <summary>
        /// <para>
        ///    IZAMAX finds the index of the first element having maximum |Re(.)| + |Im(.)|
        /// </para>
        /// </summary>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// number of elements in input vector(s)
        /// </param>
        /// <param name="x">
        /// [in] ZX is COMPLEX*16 array, dimension ( 1 + ( N - 1 )*abs( INCX ) ).
        /// </param>
        /// <param name="incx">
        /// [in] INCX is INTEGER.
        /// storage spacing between elements of ZX
        /// </param>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "cblas_izamax", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe UIntPtr Izamax(
            int n,
            void* x,
            int incx);
    }
}
