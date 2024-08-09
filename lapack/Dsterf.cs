using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DSTERF computes all eigenvalues of a symmetric tridiagonal matrix
        /// using the Pal-Walker-Kahan variant of the QL or QR algorithm.
        /// </para>
        /// </summary>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrix.  N &gt;= 0.
        /// </param>
        /// <param name="d">
        /// [in,out] D is DOUBLE PRECISION array, dimension (N).
        /// On entry, the n diagonal elements of the tridiagonal matrix.
        /// On exit, if INFO = 0, the eigenvalues in ascending order.
        /// </param>
        /// <param name="e">
        /// [in,out] E is DOUBLE PRECISION array, dimension (N-1).
        /// On entry, the (n-1) subdiagonal elements of the tridiagonal
        /// matrix.
        /// On exit, E has been destroyed.
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// &gt; 0:  the algorithm failed to find all of the eigenvalues in
        /// a total of 30*N iterations; if INFO = i, then i
        /// elements of E have not converged to zero.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dsterf", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dsterf(
            int n,
            double* d,
            double* e);
    }
}
