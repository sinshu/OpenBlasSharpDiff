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
        /// <param name="order">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="transa">
        /// No description available.
        /// </param>
        /// <param name="transb">
        /// No description available.
        /// </param>
        /// <param name="m">
        /// No description available.
        /// </param>
        /// <param name="n">
        /// No description available.
        /// </param>
        /// <param name="k">
        /// No description available.
        /// </param>
        /// <param name="alpha">
        /// No description available.
        /// </param>
        /// <param name="a">
        /// No description available.
        /// </param>
        /// <param name="lda">
        /// No description available.
        /// </param>
        /// <param name="b">
        /// No description available.
        /// </param>
        /// <param name="ldb">
        /// No description available.
        /// </param>
        /// <param name="beta">
        /// No description available.
        /// </param>
        /// <param name="c">
        /// No description available.
        /// </param>
        /// <param name="ldc">
        /// No description available.
        /// </param>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "cblas_zgemm3m", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe void Zgemm3m(
            Order order,
            Transpose transa,
            Transpose transb,
            int m,
            int n,
            int k,
            void* alpha,
            void* a,
            int lda,
            void* b,
            int ldb,
            void* beta,
            void* c,
            int ldc);
    }
}
