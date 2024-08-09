using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Blas
    {
        /// <summary>
        /// No description available.
        /// </summary>
        /// <param name="n">
        /// No description available.
        /// </param>
        /// <param name="x">
        /// No description available.
        /// </param>
        /// <param name="incx">
        /// No description available.
        /// </param>
        /// <param name="y">
        /// No description available.
        /// </param>
        /// <param name="incy">
        /// No description available.
        /// </param>
        /// <param name="ret">
        /// No description available.
        /// </param>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "cblas_cdotu_sub", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe void CdotuSub(
            int n,
            void* x,
            int incx,
            void* y,
            int incy,
            void* ret);
    }
}
