using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Blas
    {
        /// <summary>
        /// <para>
        ///    SROTG construct givens plane rotation.
        /// </para>
        /// </summary>
        /// <param name="a">
        /// [in] SA is REAL.
        /// </param>
        /// <param name="b">
        /// [in] SB is REAL.
        /// </param>
        /// <param name="c">
        /// [out] C is REAL.
        /// </param>
        /// <param name="s">
        /// [out] S is REAL.
        /// </param>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "cblas_srotg", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe void Srotg(
            float* a,
            float* b,
            float* c,
            float* s);
    }
}
