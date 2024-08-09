using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Blas
    {
        /// <summary>
        /// <para>
        /// Applies a plane rotation, where the cos and sin (c and s) are real
        /// and the vectors cx and cy are complex.
        /// jack dongarra, linpack, 3/11/78.
        /// </para>
        /// </summary>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// On entry, N specifies the order of the vectors cx and cy.
        /// N must be at least zero.
        /// </param>
        /// <param name="x">
        /// No description available.
        /// </param>
        /// <param name="incx">
        /// [in] INCX is INTEGER.
        /// On entry, INCX specifies the increment for the elements of
        /// CX. INCX must not be zero.
        /// </param>
        /// <param name="y">
        /// No description available.
        /// </param>
        /// <param name="incy">
        /// [in] INCY is INTEGER.
        /// On entry, INCY specifies the increment for the elements of
        /// CY. INCY must not be zero.
        /// </param>
        /// <param name="c">
        /// [in] C is DOUBLE PRECISION.
        /// On entry, C specifies the cosine, cos.
        /// </param>
        /// <param name="s">
        /// [in] S is DOUBLE PRECISION.
        /// On entry, S specifies the sine, sin.
        /// </param>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "cblas_zdrot", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe void Zdrot(
            int n,
            void* x,
            int incx,
            void* y,
            int incy,
            double c,
            double s);
    }
}
