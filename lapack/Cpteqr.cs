using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// CPTEQR computes all eigenvalues and, optionally, eigenvectors of a
        /// symmetric positive definite tridiagonal matrix by first factoring the
        /// matrix using SPTTRF and then calling CBDSQR to compute the singular
        /// values of the bidiagonal factor.
        /// </para>
        /// <para>
        /// This routine computes the eigenvalues of the positive definite
        /// tridiagonal matrix to high relative accuracy.  This means that if the
        /// eigenvalues range over many orders of magnitude in size, then the
        /// small eigenvalues and corresponding eigenvectors will be computed
        /// more accurately than, for example, with the standard QR method.
        /// </para>
        /// <para>
        /// The eigenvectors of a full or band positive definite Hermitian matrix
        /// can also be found if CHETRD, CHPTRD, or CHBTRD has been used to
        /// reduce this matrix to tridiagonal form.  (The reduction to
        /// tridiagonal form, however, may preclude the possibility of obtaining
        /// high relative accuracy in the small eigenvalues of the original
        /// matrix, if these eigenvalues range over many orders of magnitude.)
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="compz">
        /// [in] COMPZ is CHARACTER*1.
        /// = &#39;N&#39;:  Compute eigenvalues only.
        /// = &#39;V&#39;:  Compute eigenvectors of original Hermitian
        /// matrix also.  Array Z contains the unitary matrix
        /// used to reduce the original matrix to tridiagonal
        /// form.
        /// = &#39;I&#39;:  Compute eigenvectors of tridiagonal matrix also.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrix.  N &gt;= 0.
        /// </param>
        /// <param name="d">
        /// [in,out] D is REAL array, dimension (N).
        /// On entry, the n diagonal elements of the tridiagonal matrix.
        /// On normal exit, D contains the eigenvalues, in descending
        /// order.
        /// </param>
        /// <param name="e">
        /// [in,out] E is REAL array, dimension (N-1).
        /// On entry, the (n-1) subdiagonal elements of the tridiagonal
        /// matrix.
        /// On exit, E has been destroyed.
        /// </param>
        /// <param name="z">
        /// [in,out] Z is COMPLEX array, dimension (LDZ, N).
        /// On entry, if COMPZ = &#39;V&#39;, the unitary matrix used in the
        /// reduction to tridiagonal form.
        /// On exit, if COMPZ = &#39;V&#39;, the orthonormal eigenvectors of the
        /// original Hermitian matrix;
        /// if COMPZ = &#39;I&#39;, the orthonormal eigenvectors of the
        /// tridiagonal matrix.
        /// If INFO &gt; 0 on exit, Z contains the eigenvectors associated
        /// with only the stored eigenvalues.
        /// If  COMPZ = &#39;N&#39;, then Z is not referenced.
        /// </param>
        /// <param name="ldz">
        /// [in] LDZ is INTEGER.
        /// The leading dimension of the array Z.  LDZ &gt;= 1, and if
        /// COMPZ = &#39;V&#39; or &#39;I&#39;, LDZ &gt;= max(1,N).
        /// </param>
        /// <returns>
        /// = 0:  successful exit.
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value.
        /// &gt; 0:  if INFO = i, and i is:
        /// &lt;= N  the Cholesky factorization of the matrix could
        /// not be performed because the leading principal
        /// minor of order i was not positive.
        /// &gt; N   the SVD algorithm failed to converge;
        /// if INFO = N+i, i off-diagonal elements of the
        /// bidiagonal factor did not converge to zero.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_cpteqr", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Cpteqr(
            MatrixLayout matrixLayout,
            char compz,
            int n,
            float* d,
            float* e,
            Complex32* z,
            int ldz);
    }
}
