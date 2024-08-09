using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Blas
    {
        /// <summary>
        /// <para>
        ///    CSCAL scales a vector by a constant.
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
        /// [in,out] CX is COMPLEX array, dimension ( 1 + ( N - 1 )*abs( INCX ) ).
        /// </param>
        /// <param name="incx">
        /// [in] INCX is INTEGER.
        /// storage spacing between elements of CX
        /// </param>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "cblas_cscal", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe void Cscal(
            int n,
            void* alpha,
            void* x,
            int incx);
    }
}
