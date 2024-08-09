using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DSTEVD computes all eigenvalues and, optionally, eigenvectors of a
        /// real symmetric tridiagonal matrix. If eigenvectors are desired, it
        /// uses a divide and conquer algorithm.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="jobz">
        /// [in] JOBZ is CHARACTER*1.
        /// = &#39;N&#39;:  Compute eigenvalues only;
        /// = &#39;V&#39;:  Compute eigenvalues and eigenvectors.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrix.  N &gt;= 0.
        /// </param>
        /// <param name="d">
        /// [in,out] D is DOUBLE PRECISION array, dimension (N).
        /// On entry, the n diagonal elements of the tridiagonal matrix
        /// A.
        /// On exit, if INFO = 0, the eigenvalues in ascending order.
        /// </param>
        /// <param name="e">
        /// [in,out] E is DOUBLE PRECISION array, dimension (N-1).
        /// On entry, the (n-1) subdiagonal elements of the tridiagonal
        /// matrix A, stored in elements 1 to N-1 of E.
        /// On exit, the contents of E are destroyed.
        /// </param>
        /// <param name="z">
        /// [out] Z is DOUBLE PRECISION array, dimension (LDZ, N).
        /// If JOBZ = &#39;V&#39;, then if INFO = 0, Z contains the orthonormal
        /// eigenvectors of the matrix A, with the i-th column of Z
        /// holding the eigenvector associated with D(i).
        /// If JOBZ = &#39;N&#39;, then Z is not referenced.
        /// </param>
        /// <param name="ldz">
        /// [in] LDZ is INTEGER.
        /// The leading dimension of the array Z.  LDZ &gt;= 1, and if
        /// JOBZ = &#39;V&#39;, LDZ &gt;= max(1,N).
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// &gt; 0:  if INFO = i, the algorithm failed to converge; i
        /// off-diagonal elements of E did not converge to zero.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dstevd", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dstevd(
            MatrixLayout matrixLayout,
            char jobz,
            int n,
            double* d,
            double* e,
            double* z,
            int ldz);
    }
}
