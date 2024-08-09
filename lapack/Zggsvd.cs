using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// No description available.
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="jobu">
        /// No description available.
        /// </param>
        /// <param name="jobv">
        /// No description available.
        /// </param>
        /// <param name="jobq">
        /// No description available.
        /// </param>
        /// <param name="m">
        /// No description available.
        /// </param>
        /// <param name="n">
        /// No description available.
        /// </param>
        /// <param name="p">
        /// No description available.
        /// </param>
        /// <param name="k">
        /// No description available.
        /// </param>
        /// <param name="l">
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
        /// <param name="alpha">
        /// No description available.
        /// </param>
        /// <param name="beta">
        /// No description available.
        /// </param>
        /// <param name="u">
        /// No description available.
        /// </param>
        /// <param name="ldu">
        /// No description available.
        /// </param>
        /// <param name="v">
        /// No description available.
        /// </param>
        /// <param name="ldv">
        /// No description available.
        /// </param>
        /// <param name="q">
        /// No description available.
        /// </param>
        /// <param name="ldq">
        /// No description available.
        /// </param>
        /// <param name="iwork">
        /// No description available.
        /// </param>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_zggsvd", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Zggsvd(
            MatrixLayout matrixLayout,
            char jobu,
            char jobv,
            char jobq,
            int m,
            int n,
            int p,
            int* k,
            int* l,
            Complex* a,
            int lda,
            Complex* b,
            int ldb,
            double* alpha,
            double* beta,
            Complex* u,
            int ldu,
            Complex* v,
            int ldv,
            Complex* q,
            int ldq,
            int* iwork);
    }
}
