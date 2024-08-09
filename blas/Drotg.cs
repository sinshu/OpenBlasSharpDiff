using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Blas
    {
        /// <summary>
        /// <para>
        ///    DROTG construct givens plane rotation.
        /// </para>
        /// </summary>
        /// <param name="a">
        /// [in] DA is DOUBLE PRECISION.
        /// </param>
        /// <param name="b">
        /// [in] DB is DOUBLE PRECISION.
        /// </param>
        /// <param name="c">
        /// [out] C is DOUBLE PRECISION.
        /// </param>
        /// <param name="s">
        /// [out] S is DOUBLE PRECISION.
        /// </param>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "cblas_drotg", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe void Drotg(
            double* a,
            double* b,
            double* c,
            double* s);
    }
}
