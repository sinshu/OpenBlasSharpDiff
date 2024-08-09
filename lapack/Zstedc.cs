using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// ZSTEDC computes all eigenvalues and, optionally, eigenvectors of a
        /// symmetric tridiagonal matrix using the divide and conquer method.
        /// The eigenvectors of a full or band complex Hermitian matrix can also
        /// be found if ZHETRD or ZHPTRD or ZHBTRD has been used to reduce this
        /// matrix to tridiagonal form.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="compz">
        /// [in] COMPZ is CHARACTER*1.
        /// = &#39;N&#39;:  Compute eigenvalues only.
        /// = &#39;I&#39;:  Compute eigenvectors of tridiagonal matrix also.
        /// = &#39;V&#39;:  Compute eigenvectors of original Hermitian matrix
        /// also.  On entry, Z contains the unitary matrix used
        /// to reduce the original matrix to tridiagonal form.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The dimension of the symmetric tridiagonal matrix.  N &gt;= 0.
        /// </param>
        /// <param name="d">
        /// [in,out] D is DOUBLE PRECISION array, dimension (N).
        /// On entry, the diagonal elements of the tridiagonal matrix.
        /// On exit, if INFO = 0, the eigenvalues in ascending order.
        /// </param>
        /// <param name="e">
        /// [in,out] E is DOUBLE PRECISION array, dimension (N-1).
        /// On entry, the subdiagonal elements of the tridiagonal matrix.
        /// On exit, E has been destroyed.
        /// </param>
        /// <param name="z">
        /// [in,out] Z is COMPLEX*16 array, dimension (LDZ,N).
        /// On entry, if COMPZ = &#39;V&#39;, then Z contains the unitary
        /// matrix used in the reduction to tridiagonal form.
        /// On exit, if INFO = 0, then if COMPZ = &#39;V&#39;, Z contains the
        /// orthonormal eigenvectors of the original Hermitian matrix,
        /// and if COMPZ = &#39;I&#39;, Z contains the orthonormal eigenvectors
        /// of the symmetric tridiagonal matrix.
        /// If  COMPZ = &#39;N&#39;, then Z is not referenced.
        /// </param>
        /// <param name="ldz">
        /// [in] LDZ is INTEGER.
        /// The leading dimension of the array Z.  LDZ &gt;= 1.
        /// If eigenvectors are desired, then LDZ &gt;= max(1,N).
        /// </param>
        /// <returns>
        /// = 0:  successful exit.
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value.
        /// &gt; 0:  The algorithm failed to compute an eigenvalue while
        /// working on the submatrix lying in rows and columns
        /// INFO/(N+1) through mod(INFO,N+1).
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_zstedc", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Zstedc(
            MatrixLayout matrixLayout,
            char compz,
            int n,
            double* d,
            double* e,
            Complex* z,
            int ldz);
    }
}
