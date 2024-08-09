using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// SSBEVD_2STAGE computes all the eigenvalues and, optionally, eigenvectors of
        /// a real symmetric band matrix A using the 2stage technique for
        /// the reduction to tridiagonal. If eigenvectors are desired, it uses
        /// a divide and conquer algorithm.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="jobz">
        /// [in] JOBZ is CHARACTER*1.
        /// = &#39;N&#39;:  Compute eigenvalues only;
        /// = &#39;V&#39;:  Compute eigenvalues and eigenvectors.
        /// Not available in this release.
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
        /// <param name="kd">
        /// [in] KD is INTEGER.
        /// The number of superdiagonals of the matrix A if UPLO = &#39;U&#39;,
        /// or the number of subdiagonals if UPLO = &#39;L&#39;.  KD &gt;= 0.
        /// </param>
        /// <param name="ab">
        /// [in,out] AB is REAL array, dimension (LDAB, N).
        /// On entry, the upper or lower triangle of the symmetric band
        /// matrix A, stored in the first KD+1 rows of the array.  The
        /// j-th column of A is stored in the j-th column of the array AB
        /// as follows:
        /// if UPLO = &#39;U&#39;, AB(kd+1+i-j,j) = A(i,j) for max(1,j-kd)&lt;=i&lt;=j;
        /// if UPLO = &#39;L&#39;, AB(1+i-j,j)    = A(i,j) for j&lt;=i&lt;=min(n,j+kd).
        /// 
        /// On exit, AB is overwritten by values generated during the
        /// reduction to tridiagonal form.  If UPLO = &#39;U&#39;, the first
        /// superdiagonal and the diagonal of the tridiagonal matrix T
        /// are returned in rows KD and KD+1 of AB, and if UPLO = &#39;L&#39;,
        /// the diagonal and first subdiagonal of T are returned in the
        /// first two rows of AB.
        /// </param>
        /// <param name="ldab">
        /// [in] LDAB is INTEGER.
        /// The leading dimension of the array AB.  LDAB &gt;= KD + 1.
        /// </param>
        /// <param name="w">
        /// [out] W is REAL array, dimension (N).
        /// If INFO = 0, the eigenvalues in ascending order.
        /// </param>
        /// <param name="z">
        /// [out] Z is REAL array, dimension (LDZ, N).
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
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// &gt; 0:  if INFO = i, the algorithm failed to converge; i
        /// off-diagonal elements of an intermediate tridiagonal
        /// form did not converge to zero.
        /// </returns>
        /// <remarks>
        /// <para>
        ///  All details about the 2stage techniques are available in:
        /// </para>
        /// <para>
        ///  Azzam Haidar, Hatem Ltaief, and Jack Dongarra.
        ///  Parallel reduction to condensed forms for symmetric eigenvalue problems
        ///  using aggregated fine-grained and memory-aware kernels. In Proceedings
        ///  of 2011 International Conference for High Performance Computing,
        ///  Networking, Storage and Analysis (SC &#39;11), New York, NY, USA,
        ///  Article 8 , 11 pages.
        ///  http://doi.acm.org/10.1145/2063384.2063394
        /// </para>
        /// <para>
        ///  A. Haidar, J. Kurzak, P. Luszczek, 2013.
        ///  An improved parallel singular value algorithm and its implementation
        ///  for multicore hardware, In Proceedings of 2013 International Conference
        ///  for High Performance Computing, Networking, Storage and Analysis (SC &#39;13).
        ///  Denver, Colorado, USA, 2013.
        ///  Article 90, 12 pages.
        ///  http://doi.acm.org/10.1145/2503210.2503292
        /// </para>
        /// <para>
        ///  A. Haidar, R. Solca, S. Tomov, T. Schulthess and J. Dongarra.
        ///  A novel hybrid CPU-GPU generalized eigensolver for electronic structure
        ///  calculations based on fine-grained memory aware tasks.
        ///  International Journal of High Performance Computing Applications.
        ///  Volume 28 Issue 2, Pages 196-209, May 2014.
        ///  http://hpc.sagepub.com/content/28/2/196
        /// </para>
        /// </remarks>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_ssbevd_2stage", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Ssbevd2stage(
            MatrixLayout matrixLayout,
            char jobz,
            char uplo,
            int n,
            int kd,
            float* ab,
            int ldab,
            float* w,
            float* z,
            int ldz);
    }
}
