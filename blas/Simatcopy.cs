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
        /// <param name="ctrans">
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
        /// <param name="cldb">
        /// No description available.
        /// </param>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "cblas_simatcopy", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe void Simatcopy(
            Order corder,
            Transpose ctrans,
            int crows,
            int ccols,
            float calpha,
            float* a,
            int clda,
            int cldb);
    }
}
