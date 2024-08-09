using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// ZHPEV computes all the eigenvalues and, optionally, eigenvectors of a
        /// complex Hermitian matrix in packed storage.
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
        /// <param name="uplo">
        /// [in] UPLO is CHARACTER*1.
        /// = &#39;U&#39;:  Upper triangle of A is stored;
        /// = &#39;L&#39;:  Lower triangle of A is stored.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrix A.  N &gt;= 0.
        /// </param>
        /// <param name="ap">
        /// [in,out] AP is COMPLEX*16 array, dimension (N*(N+1)/2).
        /// On entry, the upper or lower triangle of the Hermitian matrix
        /// A, packed columnwise in a linear array.  The j-th column of A
        /// is stored in the array AP as follows:
        /// if UPLO = &#39;U&#39;, AP(i + (j-1)*j/2) = A(i,j) for 1&lt;=i&lt;=j;
        /// if UPLO = &#39;L&#39;, AP(i + (j-1)*(2*n-j)/2) = A(i,j) for j&lt;=i&lt;=n.
        /// 
        /// On exit, AP is overwritten by values generated during the
        /// reduction to tridiagonal form.  If UPLO = &#39;U&#39;, the diagonal
        /// and first superdiagonal of the tridiagonal matrix T overwrite
        /// the corresponding elements of A, and if UPLO = &#39;L&#39;, the
        /// diagonal and first subdiagonal of T overwrite the
        /// corresponding elements of A.
        /// </param>
        /// <param name="w">
        /// [out] W is DOUBLE PRECISION array, dimension (N).
        /// If INFO = 0, the eigenvalues in ascending order.
        /// </param>
        /// <param name="z">
        /// [out] Z is COMPLEX*16 array, dimension (LDZ, N).
        /// If JOBZ = &#39;V&#39;, then if INFO = 0, Z contains the orthonormal
        /// eigenvectors of the matrix A, with the i-th column of Z
        /// holding the eigenvector associated with W(i).
        /// If JOBZ = &#39;N&#39;, then Z is not referenced.
        /// </param>
        /// <param name="ldz">
        /// [in] LDZ is INTEGER.
        /// The leading dimension of the array Z.  LDZ &gt;= 1, and if
        /// JOBZ = &#39;V&#39;, LDZ &gt;= max(1,N).
        /// </param>
        /// <returns>
        /// = 0:  successful exit.
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value.
        /// &gt; 0:  if INFO = i, the algorithm failed to converge; i
        /// off-diagonal elements of an intermediate tridiagonal
        /// form did not converge to zero.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_zhpev", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Zhpev(
            MatrixLayout matrixLayout,
            char jobz,
            char uplo,
            int n,
            Complex* ap,
            double* w,
            Complex* z,
            int ldz);
    }
}
