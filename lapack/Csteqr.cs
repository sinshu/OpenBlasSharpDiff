using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// CSTEQR computes all eigenvalues and, optionally, eigenvectors of a
        /// symmetric tridiagonal matrix using the implicit QL or QR method.
        /// The eigenvectors of a full or band complex Hermitian matrix can also
        /// be found if CHETRD or CHPTRD or CHBTRD has been used to reduce this
        /// matrix to tridiagonal form.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="compz">
        /// [in] COMPZ is CHARACTER*1.
        /// = &#39;N&#39;:  Compute eigenvalues only.
        /// = &#39;V&#39;:  Compute eigenvalues and eigenvectors of the original
        /// Hermitian matrix.  On entry, Z must contain the
        /// unitary matrix used to reduce the original matrix
        /// to tridiagonal form.
        /// = &#39;I&#39;:  Compute eigenvalues and eigenvectors of the
        /// tridiagonal matrix.  Z is initialized to the identity
        /// matrix.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrix.  N &gt;= 0.
        /// </param>
        /// <param name="d">
        /// [in,out] D is REAL array, dimension (N).
        /// On entry, the diagonal elements of the tridiagonal matrix.
        /// On exit, if INFO = 0, the eigenvalues in ascending order.
        /// </param>
        /// <param name="e">
        /// [in,out] E is REAL array, dimension (N-1).
        /// On entry, the (n-1) subdiagonal elements of the tridiagonal
        /// matrix.
        /// On exit, E has been destroyed.
        /// </param>
        /// <param name="z">
        /// [in,out] Z is COMPLEX array, dimension (LDZ, N).
        /// On entry, if  COMPZ = &#39;V&#39;, then Z contains the unitary
        /// matrix used in the reduction to tridiagonal form.
        /// On exit, if INFO = 0, then if COMPZ = &#39;V&#39;, Z contains the
        /// orthonormal eigenvectors of the original Hermitian matrix,
        /// and if COMPZ = &#39;I&#39;, Z contains the orthonormal eigenvectors
        /// of the symmetric tridiagonal matrix.
        /// If COMPZ = &#39;N&#39;, then Z is not referenced.
        /// </param>
        /// <param name="ldz">
        /// [in] LDZ is INTEGER.
        /// The leading dimension of the array Z.  LDZ &gt;= 1, and if
        /// eigenvectors are desired, then  LDZ &gt;= max(1,N).
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// &gt; 0:  the algorithm has failed to find all the eigenvalues in
        /// a total of 30*N iterations; if INFO = i, then i
        /// elements of E have not converged to zero; on exit, D
        /// and E contain the elements of a symmetric tridiagonal
        /// matrix which is unitarily similar to the original
        /// matrix.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_csteqr", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Csteqr(
            MatrixLayout matrixLayout,
            char compz,
            int n,
            float* d,
            float* e,
            Complex32* z,
            int ldz);
    }
}
