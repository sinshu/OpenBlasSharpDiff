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
        [DllImport(OpenBlas.LibraryName, EntryPoint = "cblas_scsum", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe float Scsum(
            int n,
            void* x,
            int incx);
    }
}
