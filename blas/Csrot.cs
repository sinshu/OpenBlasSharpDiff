using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Blas
    {
        /// <summary>
        /// <para>
        /// CSROT applies a plane rotation, where the cos and sin (c and s) are real
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
        /// [in,out] CX is COMPLEX array, dimension at least.
        /// ( 1 + ( N - 1 )*abs( INCX ) ).
        /// Before entry, the incremented array CX must contain the n
        /// element vector cx. On exit, CX is overwritten by the updated
        /// vector cx.
        /// </param>
        /// <param name="incx">
        /// [in] INCX is INTEGER.
        /// On entry, INCX specifies the increment for the elements of
        /// CX. INCX must not be zero.
        /// </param>
        /// <param name="y">
        /// [in,out] CY is COMPLEX array, dimension at least.
        /// ( 1 + ( N - 1 )*abs( INCY ) ).
        /// Before entry, the incremented array CY must contain the n
        /// element vector cy. On exit, CY is overwritten by the updated
        /// vector cy.
        /// </param>
        /// <param name="incy">
        /// [in] INCY is INTEGER.
        /// On entry, INCY specifies the increment for the elements of
        /// CY. INCY must not be zero.
        /// </param>
        /// <param name="c">
        /// [in] C is REAL.
        /// On entry, C specifies the cosine, cos.
        /// </param>
        /// <param name="s">
        /// [in] S is REAL.
        /// On entry, S specifies the sine, sin.
        /// </param>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "cblas_csrot", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe void Csrot(
            int n,
            void* x,
            int incx,
            void* y,
            int incy,
            float c,
            float s);
    }
}
