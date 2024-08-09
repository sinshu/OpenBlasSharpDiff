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
        /// <param name="corder">
        /// No description available.
        /// </param>
        /// <param name="crows">
        /// No description available.
        /// </param>
        /// <param name="ccols">
        /// No description available.
        /// </param>
        /// <param name="calpha">
        /// No description available.
        /// </param>
        /// <param name="a">
        /// No description available.
        /// </param>
        /// <param name="clda">
        /// No description available.
        /// </param>
        /// <param name="cbeta">
        /// No description available.
        /// </param>
        /// <param name="c">
        /// No description available.
        /// </param>
        /// <param name="cldc">
        /// No description available.
        /// </param>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "cblas_dgeadd", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe void Dgeadd(
            Order corder,
            int crows,
            int ccols,
            double calpha,
            double* a,
            int clda,
            double cbeta,
            double* c,
            int cldc);
    }
}
